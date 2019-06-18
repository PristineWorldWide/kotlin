/*
 * Copyright 2010-2016 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.jetbrains.kotlin.idea.refactoring.move.moveDeclarations

import com.intellij.ide.highlighter.JavaFileType
import com.intellij.ide.util.EditorHelper
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.text.StringUtil
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiNamedElement
import com.intellij.psi.PsiReference
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.refactoring.BaseRefactoringProcessor
import com.intellij.refactoring.move.MoveCallback
import com.intellij.refactoring.move.MoveMultipleElementsViewDescriptor
import com.intellij.refactoring.move.moveClassesOrPackages.MoveClassHandler
import com.intellij.refactoring.rename.RenameUtil
import com.intellij.refactoring.util.NonCodeUsageInfo
import com.intellij.refactoring.util.RefactoringUIUtil
import com.intellij.refactoring.util.TextOccurrencesUtil
import com.intellij.usageView.UsageInfo
import com.intellij.usageView.UsageViewBundle
import com.intellij.usageView.UsageViewDescriptor
import com.intellij.usageView.UsageViewUtil
import com.intellij.util.IncorrectOperationException
import com.intellij.util.containers.MultiMap
import gnu.trove.THashMap
import gnu.trove.TObjectHashingStrategy
import org.jetbrains.kotlin.asJava.elements.KtLightDeclaration
import org.jetbrains.kotlin.asJava.findFacadeClass
import org.jetbrains.kotlin.asJava.namedUnwrappedElement
import org.jetbrains.kotlin.asJava.toLightElements
import org.jetbrains.kotlin.idea.codeInsight.shorten.addToBeShortenedDescendantsToWaitingSet
import org.jetbrains.kotlin.idea.core.deleteSingle
import org.jetbrains.kotlin.idea.core.quoteIfNeeded
import org.jetbrains.kotlin.idea.core.util.toPsiDirectory
import org.jetbrains.kotlin.idea.refactoring.broadcastRefactoringExit
import org.jetbrains.kotlin.idea.refactoring.fqName.getKotlinFqName
import org.jetbrains.kotlin.idea.refactoring.getOrCreateKotlinFile
import org.jetbrains.kotlin.idea.refactoring.move.*
import org.jetbrains.kotlin.idea.refactoring.move.moveFilesOrDirectories.MoveKotlinClassHandler
import org.jetbrains.kotlin.idea.search.projectScope
import org.jetbrains.kotlin.idea.search.restrictByFileType
import org.jetbrains.kotlin.idea.stubindex.KotlinFullClassNameIndex
import org.jetbrains.kotlin.idea.util.*
import org.jetbrains.kotlin.idea.util.projectStructure.module
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.psi.psiUtil.*
import org.jetbrains.kotlin.utils.addIfNotNull
import org.jetbrains.kotlin.utils.addToStdlib.safeAs
import org.jetbrains.kotlin.utils.ifEmpty
import org.jetbrains.kotlin.utils.keysToMap
import java.util.*

interface Mover : (KtNamedDeclaration, KtElement) -> KtNamedDeclaration {
    object Default : Mover {
        override fun invoke(originalElement: KtNamedDeclaration, targetContainer: KtElement): KtNamedDeclaration {
            return when (targetContainer) {
                is KtFile -> {
                    val declarationContainer: KtElement =
                        if (targetContainer.isScript()) targetContainer.script!!.blockExpression else targetContainer
                    declarationContainer.add(originalElement) as KtNamedDeclaration
                }
                is KtClassOrObject -> targetContainer.addDeclaration(originalElement)
                else -> error("Unexpected element: ${targetContainer.getElementTextWithContext()}")
            }.apply {
                val container = originalElement.containingClassOrObject
                if (container is KtObjectDeclaration && container.isCompanion() && container.declarations.singleOrNull() == originalElement) {
                    container.deleteSingle()
                } else {
                    originalElement.deleteSingle()
                }
            }
        }
    }
}

sealed class MoveSource {
    abstract val elementsToMove: Collection<KtNamedDeclaration>

    class Elements(override val elementsToMove: Collection<KtNamedDeclaration>) : MoveSource()

    class File(val file: KtFile) : MoveSource() {
        override val elementsToMove: Collection<KtNamedDeclaration>
            get() = file.declarations.filterIsInstance<KtNamedDeclaration>()
    }
}

fun MoveSource(declaration: KtNamedDeclaration) = MoveSource.Elements(listOf(declaration))
fun MoveSource(declarations: Collection<KtNamedDeclaration>) = MoveSource.Elements(declarations)
fun MoveSource(file: KtFile) = MoveSource.File(file)

class MoveDeclarationsDescriptor @JvmOverloads constructor(
    val project: Project,
    val moveSource: MoveSource,
    val moveTarget: KotlinMoveTarget,
    val delegate: MoveDeclarationsDelegate,
    val searchInCommentsAndStrings: Boolean = true,
    val searchInNonCode: Boolean = true,
    val deleteSourceFiles: Boolean = false,
    val moveCallback: MoveCallback? = null,
    val openInEditor: Boolean = false,
    val allElementsToMove: List<PsiElement>? = null,
    val analyzeConflicts: Boolean = true,
    val searchReferences: Boolean = true
)

class ConflictUsageInfo(element: PsiElement, val messages: Collection<String>) : UsageInfo(element)

private object ElementHashingStrategy : TObjectHashingStrategy<PsiElement> {
    override fun equals(e1: PsiElement?, e2: PsiElement?): Boolean {
        if (e1 === e2) return true
        // Name should be enough to distinguish different light elements based on the same original declaration
        if (e1 is KtLightDeclaration<*, *> && e2 is KtLightDeclaration<*, *>) {
            return e1.kotlinOrigin == e2.kotlinOrigin && e1.name == e2.name
        }
        return e1 == e2
    }

    override fun computeHashCode(e: PsiElement?): Int {
        return when (e) {
            null -> 0
            is KtLightDeclaration<*, *> -> (e.kotlinOrigin?.hashCode() ?: 0) * 31 + (e.name?.hashCode() ?: 0)
            else -> e.hashCode()
        }
    }
}

class MoveKotlinDeclarationsProcessor(
    val descriptor: MoveDeclarationsDescriptor,
    val mover: Mover = Mover.Default
) : BaseRefactoringProcessor(descriptor.project) {
    companion object {
        private const val REFACTORING_NAME = "Move declarations"
        const val REFACTORING_ID = "move.kotlin.declarations"
    }

    val project get() = descriptor.project

    private var nonCodeUsages: Array<NonCodeUsageInfo>? = null
    private val moveEntireFile = descriptor.moveSource is MoveSource.File
    private val elementsToMove = descriptor.moveSource.elementsToMove
        .filter { e -> e.parent != descriptor.moveTarget.getTargetPsiIfExists(e) }
        .let { selectedElements ->
            val additionalElements = ArrayList<KtNamedDeclaration>()
            selectedElements.forEach { elem ->
                if (elem.hasExpectModifier()) {
                    elem.actualsForExpected(null).forEach { additionalElements.addIfNotNull(it.safeAs()) }
                }
                if (elem.hasActualModifier()) {
                    // TODO: error
                }
            }
            selectedElements + additionalElements
        }
    private val kotlinToLightElementsBySourceFile = elementsToMove
        .groupBy { it.containingKtFile }
        .mapValues { it.value.keysToMap { it.toLightElements().ifEmpty { listOf(it) } } }
    private val conflicts = MultiMap<PsiElement, String>()

    override fun getRefactoringId() = REFACTORING_ID

    override fun createUsageViewDescriptor(usages: Array<out UsageInfo>): UsageViewDescriptor {
        val targetContainerFqName = descriptor.moveTarget.targetContainerFqName?.let {
            if (it.isRoot) UsageViewBundle.message("default.package.presentable.name") else it.asString()
        }
        return MoveMultipleElementsViewDescriptor(elementsToMove.toTypedArray(), targetContainerFqName)
    }

    fun getConflictsAsUsages(): List<UsageInfo> = conflicts.entrySet().map { ConflictUsageInfo(it.key, it.value) }

    public override fun findUsages(): Array<UsageInfo> {
        if (!descriptor.searchReferences || elementsToMove.isEmpty()) return UsageInfo.EMPTY_ARRAY

        val newContainerName = descriptor.moveTarget.targetContainerFqName?.asString() ?: ""

        fun getSearchScope(element: PsiElement): GlobalSearchScope? {
            val projectScope = project.projectScope()
            val ktDeclaration = element.namedUnwrappedElement as? KtNamedDeclaration ?: return projectScope
            if (ktDeclaration.hasModifier(KtTokens.PRIVATE_KEYWORD)) return projectScope
            val moveTarget = descriptor.moveTarget
            val (oldContainer, newContainer) = descriptor.delegate.getContainerChangeInfo(ktDeclaration, moveTarget)
            val targetModule = moveTarget.getTargetModule(project) ?: return projectScope
            if (oldContainer != newContainer || ktDeclaration.module != targetModule) return projectScope
            // Check if facade class may change
            if (newContainer is ContainerInfo.Package) {
                val javaScope = projectScope.restrictByFileType(JavaFileType.INSTANCE)
                val currentFile = ktDeclaration.containingKtFile
                val newFile = when (moveTarget) {
                    is KotlinMoveTargetForExistingElement -> moveTarget.targetElement as? KtFile ?: return null
                    is KotlinMoveTargetForDeferredFile -> return javaScope
                    else -> return null
                }
                val currentFacade = currentFile.findFacadeClass()
                val newFacade = newFile.findFacadeClass()
                return if (currentFacade?.qualifiedName != newFacade?.qualifiedName) javaScope else null
            }
            return null
        }

        fun collectUsages(kotlinToLightElements: Map<KtNamedDeclaration, List<PsiNamedElement>>, result: MutableCollection<UsageInfo>) {
            kotlinToLightElements.values.flatten().flatMapTo(result) { lightElement ->
                val searchScope = getSearchScope(lightElement) ?: return@flatMapTo emptyList()
                val elementName = lightElement.name ?: return@flatMapTo emptyList()
                val newFqName = StringUtil.getQualifiedName(newContainerName, elementName)

                val foundReferences = HashSet<PsiReference>()
                val results = ReferencesSearch
                    .search(lightElement, searchScope)
                    .mapNotNullTo(ArrayList()) { ref ->
                        if (foundReferences.add(ref) && elementsToMove.none { it.isAncestor(ref.element) }) {
                            createMoveUsageInfoIfPossible(ref, lightElement, addImportToOriginalFile = true, isInternal = false)
                        } else null
                    }

                val name = lightElement.getKotlinFqName()?.quoteIfNeeded()?.asString()
                if (name != null) {
                    TextOccurrencesUtil.findNonCodeUsages(
                        lightElement,
                        name,
                        descriptor.searchInCommentsAndStrings,
                        descriptor.searchInNonCode,
                        FqName(newFqName).quoteIfNeeded().asString(),
                        results
                    )
                }

                MoveClassHandler.EP_NAME.extensions.forEach { handler ->
                    if (handler !is MoveKotlinClassHandler) handler.preprocessUsages(results)
                }

                results
            }
        }

        val usages = ArrayList<UsageInfo>()

        fun updateUsages(sourceFile: KtFile, elements: Map<KtNamedDeclaration, List<PsiNamedElement>>) {
            if (elements.isEmpty()) return

            val conflictChecker = MoveConflictChecker(
                project,
                elements.keys,
                transformMoveTarget(elements.keys.first(), descriptor.moveTarget),
                elements.keys.first(),
                allElementsToMove = descriptor.allElementsToMove // TODO: ?
            )

            val internalUsages = LinkedHashSet<UsageInfo>()
            val externalUsages = LinkedHashSet<UsageInfo>()

            if (moveEntireFile) {
                val changeInfo = ContainerChangeInfo(
                    ContainerInfo.Package(sourceFile.packageFqName),
                    descriptor.moveTarget.targetContainerFqName?.let { ContainerInfo.Package(it) } ?: ContainerInfo.UnknownPackage
                )
                internalUsages += sourceFile.getInternalReferencesToUpdateOnPackageNameChange(changeInfo)
            } else {
                elements.keys.forEach {
                    val packageNameInfo = descriptor.delegate.getContainerChangeInfo(it, descriptor.moveTarget)
                    internalUsages += it.getInternalReferencesToUpdateOnPackageNameChange(packageNameInfo)
                }
            }

            internalUsages += descriptor.delegate.findInternalUsages(descriptor)
            collectUsages(elements, externalUsages)
            if (descriptor.analyzeConflicts) {
                conflictChecker.checkAllConflicts(externalUsages, internalUsages, conflicts)
                descriptor.delegate.collectConflicts(descriptor, internalUsages, conflicts)
            }

            usages += internalUsages
            usages += externalUsages
        }

        for ((sourceFile, kotlinToLightElements) in kotlinToLightElementsBySourceFile) {
            updateUsages(sourceFile, kotlinToLightElements.filterKeys { !it.hasActualModifier() })
            kotlinToLightElements.forEach {
                if (it.key.hasActualModifier()) {
                    updateUsages(sourceFile, mapOf(it.toPair()))
                }
            }
        }

        return UsageViewUtil.removeDuplicatedUsages(usages.toTypedArray())
    }

    private fun getRelativePath(sourceRootPath: String, file: VirtualFile): String = file.path
        .removePrefix(sourceRootPath)
        .removePrefix("/")

    private fun findSuitableSourceRoot(
        allActualSourceRoots: Array<VirtualFile>,
        expectedSourceRootPath: String,
        currentDir: VirtualFile,
        sourceRootPath: String
    ): VirtualFile? {
        val relativePath = getRelativePath(sourceRootPath, currentDir)
        if (relativePath.isEmpty()) return allActualSourceRoots.maxBy { it.path.commonSuffixWith(expectedSourceRootPath).length }
        return allActualSourceRoots.find { it.findFileByRelativePath(relativePath) != null }
            ?: findSuitableSourceRoot(allActualSourceRoots, expectedSourceRootPath, currentDir.parent, sourceRootPath)
    }

    // TODO : add caching
    private fun transformMoveTarget(
        element: KtDeclaration,
        moveTarget: KotlinMoveTarget
    ): KotlinMoveTarget {
        if (!element.hasActualModifier()) return moveTarget
        val actualElement = element
        when (moveTarget) {
            is KotlinMoveTargetForDeferredFile -> {
                val expectedElement = element.expectedDeclarationIfAny() ?: return moveTarget
                when {
                    moveTarget.directory != null -> {
                        val currentDirForExpected = moveTarget.directory.virtualFile
                        val expectedSourceRootPath = currentDirForExpected.getSourceRoot(project)?.path ?: return moveTarget
                        val allActualSourceRoots = actualElement.module?.sourceRoots ?: return moveTarget
                        val actualSourceRoot = findSuitableSourceRoot(
                            allActualSourceRoots,
                            expectedSourceRootPath,
                            currentDirForExpected,
                            expectedSourceRootPath
                        ) ?: return moveTarget
                        val actualTargetDir = actualSourceRoot.findFileByRelativePath(
                            getRelativePath(
                                expectedSourceRootPath,
                                currentDirForExpected
                            )
                        )
                        val actualTargetFile = actualTargetDir?.findChild("${actualElement.name}.kt")
                        return KotlinMoveTargetForDeferredFile(
                            moveTarget.targetContainerFqName,
                            actualTargetDir?.toPsiDirectory(project),
                            actualTargetFile,
                            moveTarget.module,
                        ) { originalFile ->
                            getOrCreateKotlinFile(
                                if (targetFileName != null) targetFileName else originalFile.name,
                                moveDestination.getTargetDirectory(originalFile)
                            )
                        }
                    }
                }
            }
            is KotlinMoveTargetForExistingElement -> {
                val targetFqName = moveTarget.targetElement.getKotlinFqName()?.asString() ?: return moveTarget
                val moduleScope = element.module?.moduleScope ?: return moveTarget
                val actualTarget = KotlinFullClassNameIndex.getInstance().get(
                    targetFqName,
                    project,
                    moduleScope
                ).firstOrNull() ?: return moveTarget
                return KotlinMoveTargetForExistingElement(actualTarget)
            }
            else -> return moveTarget
        }
    }

    override fun preprocessUsages(refUsages: Ref<Array<UsageInfo>>): Boolean {
        return showConflicts(conflicts, refUsages.get())
    }

    override fun performRefactoring(usages: Array<out UsageInfo>) = doPerformRefactoring(usages.toList())

    internal fun doPerformRefactoring(usages: List<UsageInfo>) {
        fun moveDeclaration(declaration: KtNamedDeclaration, moveTarget: KotlinMoveTarget): KtNamedDeclaration {
            val targetContainer = moveTarget.getOrCreateTargetPsi(declaration)
                ?: throw AssertionError("Couldn't create Kotlin file for: ${declaration::class.java}: ${declaration.text}")
            descriptor.delegate.preprocessDeclaration(descriptor, declaration)
            if (moveEntireFile) return declaration
            return mover(declaration, targetContainer).apply {
                addToBeShortenedDescendantsToWaitingSet()
            }
        }

        val (oldInternalUsages, externalUsages) = usages.partition { it is KotlinMoveUsage && it.isInternal }
        val newInternalUsages = ArrayList<UsageInfo>()

        markInternalUsages(oldInternalUsages)

        val usagesToProcess = ArrayList(externalUsages)

        try {
            descriptor.delegate.preprocessUsages(descriptor, usages)

            val oldToNewElementsMapping = THashMap<PsiElement, PsiElement>(ElementHashingStrategy)

            val newDeclarations = ArrayList<KtNamedDeclaration>()

            for ((sourceFile, kotlinToLightElements) in kotlinToLightElementsBySourceFile) {
                for ((oldDeclaration, oldLightElements) in kotlinToLightElements) {
                    val elementListener = transaction?.getElementListener(oldDeclaration)

                    val newDeclaration = moveDeclaration(
                        oldDeclaration, transformMoveTarget(oldDeclaration, descriptor.moveTarget)
                    )

                    newDeclarations += newDeclaration

                    oldToNewElementsMapping[oldDeclaration] = newDeclaration
                    oldToNewElementsMapping[sourceFile] = newDeclaration.containingKtFile

                    elementListener?.elementMoved(newDeclaration)
                    for ((oldElement, newElement) in oldLightElements.asSequence().zip(newDeclaration.toLightElements().asSequence())) {
                        oldToNewElementsMapping[oldElement] = newElement
                    }

                    if (descriptor.openInEditor) {
                        EditorHelper.openInEditor(newDeclaration)
                    }
                }

                if (descriptor.deleteSourceFiles) {
                    sourceFile.delete()
                }
            }

            val internalUsageScopes: List<KtElement> = if (moveEntireFile) {
                newDeclarations.asSequence().map { it.containingKtFile }.distinct().toList()
            } else {
                newDeclarations
            }
            internalUsageScopes.forEach { newInternalUsages += restoreInternalUsages(it, oldToNewElementsMapping) }

            usagesToProcess += newInternalUsages

            nonCodeUsages = postProcessMoveUsages(usagesToProcess, oldToNewElementsMapping).toTypedArray()
        } catch (e: IncorrectOperationException) {
            nonCodeUsages = null
            RefactoringUIUtil.processIncorrectOperation(myProject, e)
        } finally {
            cleanUpInternalUsages(newInternalUsages + oldInternalUsages)
        }
    }

    override fun performPsiSpoilingRefactoring() {
        nonCodeUsages?.let { nonCodeUsages -> RenameUtil.renameNonCodeUsages(myProject, nonCodeUsages) }
        descriptor.moveCallback?.refactoringCompleted()
    }

    fun execute(usages: List<UsageInfo>) {
        execute(usages.toTypedArray())
    }

    override fun doRun() {
        try {
            super.doRun()
        } finally {
            broadcastRefactoringExit(myProject, refactoringId)
        }
    }

    override fun getCommandName(): String = REFACTORING_NAME
}