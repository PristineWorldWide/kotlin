/*
 * Copyright 2010-2023 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

@file:Suppress("UnstableApiUsage")

package org.jetbrains.kotlin.kapt4

import com.google.common.collect.HashMultimap
import com.google.common.collect.Multimap
import com.intellij.lang.ASTNode
import com.intellij.psi.*
import com.intellij.psi.impl.source.tree.LeafPsiElement
import org.jetbrains.kotlin.analysis.api.KtAnalysisSession
import org.jetbrains.kotlin.analysis.api.symbols.KtCallableSymbol
import org.jetbrains.kotlin.analysis.api.symbols.KtClassOrObjectSymbol
import org.jetbrains.kotlin.analysis.api.symbols.KtEnumEntrySymbol
import org.jetbrains.kotlin.analysis.utils.printer.PrettyPrinter
import org.jetbrains.kotlin.analysis.utils.printer.prettyPrint
import org.jetbrains.kotlin.asJava.classes.KtLightClass
import org.jetbrains.kotlin.asJava.classes.KtLightClassForFacade
import org.jetbrains.kotlin.asJava.elements.*
import org.jetbrains.kotlin.asJava.findFacadeClass
import org.jetbrains.kotlin.asJava.toLightClass
import org.jetbrains.kotlin.builtins.StandardNames
import org.jetbrains.kotlin.config.LanguageVersion
import org.jetbrains.kotlin.fileClasses.JvmFileClassUtil
import org.jetbrains.kotlin.idea.references.KtReference
import org.jetbrains.kotlin.kapt3.base.KaptFlag
import org.jetbrains.kotlin.kapt3.base.stubs.KaptStubLineInformation
import org.jetbrains.kotlin.kapt3.stubs.MemberData
import org.jetbrains.kotlin.kapt3.stubs.extractComment
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.light.classes.symbol.classes.SymbolLightClassForNamedClassLike
import org.jetbrains.kotlin.light.classes.symbol.escapeString
import org.jetbrains.kotlin.load.java.JvmAnnotationNames.*
import org.jetbrains.kotlin.load.kotlin.header.KotlinClassHeader
import org.jetbrains.kotlin.metadata.deserialization.BinaryVersion
import org.jetbrains.kotlin.name.*
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.psi.psiUtil.parameterIndex
import org.jetbrains.kotlin.psi.stubs.elements.KtAnnotationEntryElementType
import org.jetbrains.kotlin.resolve.calls.util.getCalleeExpressionIfAny
import org.jetbrains.kotlin.utils.addToStdlib.ifNotEmpty
import org.jetbrains.kotlin.utils.toMetadataVersion
import org.jetbrains.kotlin.kapt3.base.KaptOptions
import java.io.File

fun generateStubs(
    files: List<KtFile>,
    options: KaptOptions,
    onError: (messages: String) -> Unit,
    analysisSession: KtAnalysisSession,
    metadataRenderer: ((Metadata) -> String)? = null,
    overriddenMetadataVersion: BinaryVersion? = null
): Map<KtLightClass, KaptStub?> =
    Kapt4StubGenerator(files, options, onError, analysisSession, metadataRenderer, overriddenMetadataVersion).generateStubs()

class KaptStub(val file: String, private val kaptMetadata: ByteArray) {
    fun writeMetadataIfNeeded(forSource: File): File =
        File(forSource.parentFile, forSource.nameWithoutExtension + KaptStubLineInformation.KAPT_METADATA_EXTENSION)
            .apply { writeBytes(kaptMetadata) }
}

private class Kapt4StubGenerator(
    private val files: List<KtFile>,
    options: KaptOptions,
    private val onError: (String) -> Unit,
    private val analysisSession: KtAnalysisSession,
    private val metadataRenderer: ((Metadata) -> String)? = null,
    private val overriddenMetadataVersion: BinaryVersion? = null
) {
    private val strictMode = options[KaptFlag.STRICT]
    private val stripMetadata = options[KaptFlag.STRIP_METADATA]
    private val keepKdocComments = options[KaptFlag.KEEP_KDOC_COMMENTS_IN_STUBS]
    private val dumpDefaultParameterValues = options[KaptFlag.DUMP_DEFAULT_PARAMETER_VALUES]
    private val typeConverter: JvmPsiConversionHelper = JvmPsiConversionHelper.getInstance(files.first().project)

    private lateinit var packageName: String
    private lateinit var containingClass: PsiClass
    private lateinit var unresolvedQualifiersRecorder: UnresolvedQualifiersRecorder


    fun generateStubs(): Map<KtLightClass, KaptStub?> =
        buildSet {
            files.flatMapTo(this) { file ->
                file.children.filterIsInstance<KtClassOrObject>().mapNotNull {
                    it.toLightClass()
                }
            }
            files.mapNotNullTo(this) { ktFile -> ktFile.findFacadeClass() }
        }.associateWith { convertTopLevelClass(it) }

    private fun convertTopLevelClass(lightClass: KtLightClass): KaptStub? {
        val ktFiles = when (lightClass) {
            is KtLightClassForFacade -> lightClass.files
            else -> listOfNotNull(lightClass.kotlinOrigin?.containingKtFile)
        }
        val lineMappings = Kapt4LineMappingCollector()
        this.packageName = (lightClass.parent as? PsiJavaFile)?.packageName ?: return null
        unresolvedQualifiersRecorder = UnresolvedQualifiersRecorder(ktFiles)
        val classBody = renderClass(lightClass, lineMappings)
        if (classBody.isEmpty()) return null

        val imports = ktFiles.fold(listOf<String>()) { acc, file -> acc + convertImports(file) }
        val stub = (if (packageName.isNotEmpty()) "package $packageName;\n\n" else "") +
                (if (imports.isEmpty()) "" else imports.joinToString("\n", "\n", "\n")) +
                classBody

        return KaptStub(stub, lineMappings.serialize())
    }

    private fun renderMetadata(m: Metadata): String =
        metadataRenderer?.invoke(m) ?: (
                "@kotlin.Metadata(" +
                        "k=${m.kind}, " +
                        "mv=${(overriddenMetadataVersion?.toArray() ?: m.metadataVersion).joinToString(", ", "{", "}")}, " +
                        "d1=${m.data1.joinToString(", ", "{", "}") { "\"${escapeString(it)}\"" }}, " +
                        "d2=${m.data2.joinToString(", ", "{", "}") { "\"${escapeString(it)}\"" }}, " +
                        "xs=\"${escapeString(m.extraString)}\", " +
                        "pn=\"${escapeString(m.packageName)}\", " +
                        "xi=${m.extraInt}" +
                        ")\n"
                )

    private fun PsiMethod.isSyntheticStaticEnumMethod(): Boolean {
        if (!isStatic) return false
        return when (name) {
            StandardNames.ENUM_VALUES.asString() -> parameters.isEmpty()
            StandardNames.ENUM_VALUE_OF.asString() -> (parameters.singleOrNull()?.type as? PsiClassType)?.qualifiedName == "java.lang.String"
            else -> false
        }
    }

    private fun convertImports(file: KtFile): List<String> {
        if (unresolvedQualifiersRecorder.isEmpty()) return emptyList()

        val imports = mutableListOf<String>()
        val importedShortNames = mutableSetOf<String>()

        // We prefer ordinary imports over aliased ones.
        val sortedImportDirectives = file.importDirectives.partition { it.aliasName == null }.run { first + second }

        for (importDirective in sortedImportDirectives) {
            val acceptableByName = when {
                importDirective.isAllUnder -> unresolvedQualifiersRecorder.simpleNames.isNotEmpty()
                else -> {
                    val fqName = importDirective.importedFqName ?: continue
                    fqName.asString() in unresolvedQualifiersRecorder.qualifiedNames || fqName.shortName().identifier in unresolvedQualifiersRecorder.simpleNames
                }
            }

            if (!acceptableByName) continue

            val importedSymbols = with(analysisSession) {
                val importedReference = importDirective.importedReference
                    ?.getCalleeExpressionIfAny()
                    ?.references
                    ?.firstOrNull() as? KtReference
                importedReference?.resolveToSymbols().orEmpty()
            }

            val isAllUnderClassifierImport = importDirective.isAllUnder && importedSymbols.any { it is KtClassOrObjectSymbol }
            val isCallableImport = !importDirective.isAllUnder && importedSymbols.any { it is KtCallableSymbol }
            val isEnumEntryImport = !importDirective.isAllUnder && importedSymbols.any { it is KtEnumEntrySymbol }

            if (isAllUnderClassifierImport || isCallableImport || isEnumEntryImport) continue

            // Qualified name should be valid Java fq-name
            val importedFqName = importDirective.importedFqName?.takeIf { it.pathSegments().size > 1 } ?: continue
            if (!isValidQualifiedName(importedFqName)) continue
            when {
                importDirective.isAllUnder -> imports += "import $importedFqName.*;"
                importedShortNames.add(importedFqName.shortName().asString()) -> imports += "import $importedFqName;"
            }
        }

        return imports
    }

    private fun checkIfValidTypeName(type: PsiType): Boolean {
        when (type) {
            is PsiArrayType -> return checkIfValidTypeName(type.componentType)
            is PsiPrimitiveType -> return true
        }

        val internalName = type.qualifiedName
        // Ignore type names with Java keywords in it
        if (internalName.split('/', '.').any { it in JAVA_KEYWORDS }) {
            if (strictMode) {
                onError("Can't generate a stub for '${internalName}'.\nType name '${type.qualifiedName}' contains a Java keyword.")
            }

            return false
        }

        val clazz = type.resolvedClass ?: return true

        if (doesInnerClassNameConflictWithOuter(clazz)) {
            if (strictMode) {
                onError(
                    "Can't generate a stub for '${clazz.qualifiedNameWithDollars}'.\n" +
                            "Its name '${clazz.name}' is the same as one of the outer class names." +
                            "\nJava forbids it. Please change one of the class names."
                )
            }

            return false
        }

        reportIfIllegalTypeUsage(containingClass, type)

        return true
    }

    private fun findContainingClassNode(clazz: PsiClass): PsiClass? =
        clazz.parent as? PsiClass

    // Java forbids outer and inner class names to be the same. Check if the names are different
    private tailrec fun doesInnerClassNameConflictWithOuter(
        clazz: PsiClass,
        outerClass: PsiClass? = findContainingClassNode(clazz),
    ): Boolean {
        if (outerClass == null) return false
        if (clazz.name == outerClass.name) return true
        // Try to find the containing class for outerClassNode (to check the whole tree recursively)
        val containingClassForOuterClass = findContainingClassNode(outerClass) ?: return false
        return doesInnerClassNameConflictWithOuter(clazz, containingClassForOuterClass)
    }

    private fun reportIfIllegalTypeUsage(
        containingClass: PsiClass,
        type: PsiType,
    ) {
        val typeName = type.simpleNameOrNull ?: return
        if (typeName !in unresolvedQualifiersRecorder.reportedTypes && typeName in unresolvedQualifiersRecorder.importsFromRoot) {
            unresolvedQualifiersRecorder.reportedTypes += typeName
            onError("${containingClass.qualifiedName}: Can't reference type '${typeName}' from default package in Java stub.")
        }
    }

    private fun PsiType.recordErrorTypes() {
        if (this is PsiEllipsisType) {
            componentType.recordErrorTypes()
            return
        }
        if (qualifiedNameOrNull == null) {
            unresolvedQualifiersRecorder.recordUnresolvedQualifier(qualifiedName)
        }
        when (this) {
            is PsiClassType -> typeArguments().forEach { (it as? PsiType)?.recordErrorTypes() }
            is PsiArrayType -> componentType.recordErrorTypes()
        }
    }

    private fun isValidQualifiedName(name: FqName) = name.pathSegments().all { isValidIdentifier(it.asString()) }

    private fun isValidIdentifier(name: String): Boolean =
        !(name.isEmpty()
                || (name in JAVA_KEYWORDS)
                || !Character.isJavaIdentifierStart(name[0])
                || name.drop(1).any { !Character.isJavaIdentifierPart(it) })

    private fun calculateMetadata(lightClass: PsiClass): Metadata? =
        if (stripMetadata) null
        else with(analysisSession) {
            when (lightClass) {
                is KtLightClassForFacade ->
                    if (lightClass.multiFileClass)
                        lightClass.qualifiedName?.let { createMultifileClassMetadata(lightClass, it) }
                    else
                        lightClass.files.singleOrNull()?.calculateMetadata(elementMapping(lightClass))
                is SymbolLightClassForNamedClassLike ->
                    lightClass.kotlinOrigin?.calculateMetadata(elementMapping(lightClass))
                else -> null
            }
        }

    private fun createMultifileClassMetadata(lightClass: KtLightClassForFacade, qualifiedName: String): Metadata =
        Metadata(
            kind = KotlinClassHeader.Kind.MULTIFILE_CLASS.id,
            metadataVersion = LanguageVersion.KOTLIN_2_0.toMetadataVersion().toArray(),
            data1 = lightClass.files.map {
                JvmFileClassUtil.manglePartName(qualifiedName.replace('.', '/'), it.name)
            }.toTypedArray(),
            extraInt = METADATA_JVM_IR_FLAG or METADATA_JVM_IR_STABLE_ABI_FLAG
        )

    private fun elementMapping(lightClass: PsiClass): Multimap<KtElement, PsiElement> =
        HashMultimap.create<KtElement, PsiElement>().apply {
            (lightClass.methods.asSequence() + lightClass.fields.asSequence() + lightClass.constructors.asSequence()).forEach {
                put((it as KtLightElement<*, *>).kotlinOrigin, it)
            }
        }

    private fun PrettyPrinter.renderClass(psiClass: PsiClass, lineMappings: Kapt4LineMappingCollector) {
        val simpleName = psiClass.name ?: return
        if (!isValidIdentifier(simpleName)) return
        containingClass = psiClass
        if (!checkIfValidTypeName(psiClass.defaultType)) return

        lineMappings.registerClass(psiClass)

        append(renderComment(psiClass))

        val classWord = when {
            psiClass.isAnnotationType -> "@interface"
            psiClass.isInterface -> "interface"
            psiClass.isEnum -> "enum"
            else -> "class"
        }
        calculateMetadata(psiClass)?.let { append(renderMetadata(it)) }
        append(psiClass.renderModifiers())
        append("$classWord ")
        append("$simpleName ")
        append(psiClass.typeParameters.renderTypeParams())
        append(psiClass.extendsList.renderRefList("extends"))
        append(psiClass.implementsList.renderRefList("implements"))
        appendLine(" {")
        withIndent {
            if (psiClass.isEnum) {
                psiClass.fields
                    .filterIsInstance<PsiEnumConstant>()
                    .filter { isValidIdentifier(it.name) }
                    .joinTo(this, ",\n") { it.renderEnumConstant() }

                append(";\n\n")
            }

            renderMembers(psiClass, lineMappings)
        }

        append("}")
    }

    private fun renderComment(element: PsiElement): String =
        getKDocComment(element)?.let {
            val lines = it.split("\n")
            return buildString {
                append("/**\n")
                lines.forEach {
                    append(" * ").append(it).append("\n")
                }
                append("*/\n")
            }
        } ?: ""

    private fun renderClass(psiClass: PsiClass, lineMappings: Kapt4LineMappingCollector): String = prettyPrint {
        renderClass(psiClass, lineMappings)
    }

    private fun PsiType.renderType() = buildString {
        this@renderType.run {
            recordErrorTypes()

            if (annotations.isNotEmpty()) {
                append(annotations.joinToString(" ", postfix = " ") { it.renderAnnotation() })
            }

            append((if (this is PsiClassType && isErroneous(this)) rawType() else this).canonicalText.replace('$', '.'))
        }
    }

    private fun isErroneous(type: PsiType): Boolean {
        if (type.canonicalText == StandardNames.NON_EXISTENT_CLASS.asString()) return true
        if (type is PsiClassType) return type.parameters.any { isErroneous(it) }
        return false
    }

    private fun PsiReferenceList?.renderRefList(keyword: String): String {
        if (this == null) return ""

        var references = referencedTypes.map { it.renderType() }.filterNot { keyword == "implements" && it.startsWith("kotlin.collections.") }
        if (references.isEmpty()) return ""
        if (keyword == "extends") references = references.take(1)
        return " $keyword ${references.joinToString()}"
    }

    private fun PsiVariable.renderVar(): String? {
        if (!isValidIdentifier(name!!)) return null
        if ((this is PsiField) && (containingClass != null) && !checkIfValidTypeName(type)) return null

        if (this is PsiEnumConstant)
            return "${renderModifiers()} $name"

        return renderComment(this) + renderModifiers() + type.renderType() + " " + name +
                if (hasInitializer() && (dumpDefaultParameterValues || navigationElement !is KtParameter))
                    " = ${initializer?.text};"
                else if (this.isFinal) " = ${getDefaultExpression(type)};"
                else ";"
    }

    private fun Array<PsiTypeParameter>.renderTypeParams() =
        if (isEmpty()) ""
        else joinToString(", ", "<", ">") {
            val bounds =
                if (it.extendsListTypes.isNotEmpty())
                    " extends " + it.extendsListTypes.joinToString(" & ", transform = { it.renderType() })
                else " extends java.lang.Object"
            it.name!! + bounds
        }

    private fun PsiAnnotationMemberValue.renderAnnotationMemberValue(): String? = when (this) {
        is PsiClassObjectAccessExpression -> text.takeIf { checkIfValidTypeName(operand.type) }
        is PsiArrayInitializerMemberValue ->
            initializers.mapNotNull { it.renderAnnotationMemberValue() }.joinToString(", ", "{", "}")
        is PsiAnnotation -> renderAnnotation()
        else -> text
    }

    private fun convertDotQualifiedExpression(dotQualifiedExpression: KtDotQualifiedExpression): String? {
        val qualifier = dotQualifiedExpression.lastChild as? KtNameReferenceExpression ?: return null
        val name = qualifier.text.takeIf { isValidIdentifier(it) } ?: "InvalidFieldName"
        val lhs = when (val left = dotQualifiedExpression.firstChild) {
            is KtNameReferenceExpression -> left.getReferencedName()
            is KtDotQualifiedExpression -> convertDotQualifiedExpression(left) ?: return null
            else -> return null
        }
        return "$lhs.$name"
    }

    private fun PsiMethod.renderMethod(): String? {
        if (!isValidIdentifier(name)) return null

        if (!checkIfValidTypeName(returnType ?: PsiType.VOID)
            || parameterList.parameters.any { !checkIfValidTypeName(typeConverter.convertType(it.type)) }
        ) return null

        return renderComment(this) +
                renderModifiers() +
                typeParameters.renderTypeParams() +
                (returnType?.renderType() ?: "") + " " +
                name +
                "(" + parameterList.parameters.filter { isValidIdentifier(paramName(it)) }
            .joinToString { it.renderModifiers() + it.type.renderType() + " " + paramName(it) } + ")" +
                (this as? PsiAnnotationMethod)?.defaultValue?.let { " default " + it.renderAnnotationMemberValue() }.orEmpty() +
                throwsList.referencedTypes.let { thrownTypes ->
                    if (thrownTypes.isEmpty()) ""
                    else " throws " + thrownTypes.joinToString { it.renderType() }
                } +
                if (isConstructor) {
                    val superConstructor = containingClass?.superClass?.constructors?.firstOrNull { !it.isPrivate }
                    if (!containingClass!!.isEnum && superConstructor != null) {
                        val args = superConstructor.parameterList.parameters.map { getDefaultExpression(it.type) }
                        " {\n    super(${args.joinToString(", ")});\n }"
                    } else {
                        " {\n }"
                    }
                } else if (isAbstract) ";" else (" {\n"
                        + (if (returnType == null || returnType == PsiType.VOID) ""
                else "    return ${getDefaultExpression(returnType!!)};\n"
                        )
                        + "}")

    }

    private fun paramName(info: PsiParameter): String {
        val defaultName = info.name
        return when {
            isValidIdentifier(defaultName) -> defaultName
            defaultName == SpecialNames.IMPLICIT_SET_PARAMETER.asString() -> "p0"
            else -> "p${info.parameterIndex()}_${info.name.hashCode().ushr(1)}"
        }
    }

    private fun PsiEnumConstant.renderEnumConstant(): String {
        val annotations = this@renderEnumConstant.annotations
            .map { it.renderAnnotation() }
            .filter { it.isNotBlank() }
            .joinToString(separator = " ", postfix = " ")
            .takeIf { it.isNotBlank() }
            ?: ""

        return prettyPrint {
            append(annotations)
            appendLine(name)
        }
    }

    private fun PrettyPrinter.renderMembers(psiClass: PsiClass, lineMappings: Kapt4LineMappingCollector) {
        var wasRendered = false
        val fieldsPositions = mutableMapOf<PsiField, MemberData>()
        psiClass.fields.filterNot { it is PsiEnumConstant }.forEach {
            lineMappings.registerField(psiClass, it)
            fieldsPositions[it] = MemberData(it.name, it.signature, lineMappings.getPosition(psiClass, it))
        }

        val sortedFields = fieldsPositions.keys.sortedBy { fieldsPositions[it] }

        append(sortedFields, wasRendered) { it.renderVar() }

        sortedFields.ifNotEmpty { wasRendered = true }
        val methodsPositions = mutableMapOf<PsiMethod, MemberData>()

        psiClass.methods
            .filterNot {
                it.isConstructor && psiClass is PsiEnumConstantInitializer
                        || psiClass.isEnum && it.isSyntheticStaticEnumMethod()
                        || it.hasAnnotation("kotlinx.kapt.KaptIgnored")
            }
            .forEach {
                lineMappings.registerMethod(psiClass, it)
                methodsPositions[it] = MemberData(it.name, it.signature, lineMappings.getPosition(psiClass, it))
            }
        val sortedMethods = methodsPositions.keys.sortedBy { methodsPositions[it] }
        append(sortedMethods, wasRendered) {
            it.renderMethod()
        }

        sortedMethods.ifNotEmpty { wasRendered = true }

        val classes = psiClass.innerClasses.toList()
        append(classes, wasRendered) {
            this@Kapt4StubGenerator.renderClass(it, lineMappings)
        }
    }

    private fun <T> PrettyPrinter.append(list: List<T>, addPrefix: Boolean, render: (T) -> String?) {
        if (list.isEmpty()) return
        val prefix = if (addPrefix) "\n\n" else ""
        list.mapNotNull(render).joinTo(this, separator = "\n\n", prefix = prefix)
    }

    private fun PsiAnnotation.renderAnnotation(): String {
        fun collectNameParts(node: ASTNode, builder: StringBuilder, takeNext: Boolean) {
            when (node) {
                is LeafPsiElement -> {
                    when (node.elementType) {
                        KtTokens.IDENTIFIER, KtTokens.DOT -> builder.append((node as ASTNode).text)
                    }
                    if (takeNext) node.treeNext?.let { collectNameParts(it, builder, true) }
                }
                else ->
                    if (node.elementType is KtAnnotationEntryElementType) {
                        collectNameParts(node.firstChildNode.treeNext, builder, false)
                    } else {
                        collectNameParts(node.firstChildNode, builder, true)
                        if (takeNext) node.treeNext?.let { collectNameParts(it, builder, true) }
                    }
            }
        }

        fun qualifiedName(node: ASTNode): String = buildString {
            collectNameParts(node, this, false)
        }

        val rawQualifiedName = when (this.qualifiedName) {
            // A temporary fix for KT-60482
            "<error>" -> (this as? KtLightElement<*, *>)?.kotlinOrigin?.node?.let { qualifiedName(it) }
                ?.also { unresolvedQualifiersRecorder.recordUnresolvedQualifier(it) }
            else -> this.qualifiedName
        } ?: return ""

        val renderedAttributes = parameterList.attributes.mapNotNull {
            val attributeValue = if (it.value == null) {
                ((it as? KtLightElementBase)?.kotlinOrigin as? KtDotQualifiedExpression)?.let { convertDotQualifiedExpression(it) }
            } else {
                it.value?.renderAnnotationMemberValue()
            } ?: return@mapNotNull null
            it.name?.takeIf { isValidIdentifier(it) }?.let { "$it = $attributeValue" }
        }

        val renderedAttributesString = renderedAttributes.joinToString()
        if (qualifiedName == null && renderedAttributesString.isEmpty()) return ""
        val qname =
            if (rawQualifiedName.startsWith(packageName) && rawQualifiedName.lastIndexOf('.') == packageName.length) rawQualifiedName.substring(
                packageName.length + 1
            ) else rawQualifiedName
        return "@$qname(${renderedAttributes.joinToString()})"
    }

    private fun PsiModifierListOwner.renderModifiers(): String {
        val annotationsBuffer = mutableListOf<String>()

        for (annotation in annotations) {
            val renderedAnnotation = annotation.renderAnnotation()
            if (renderedAnnotation.isNotEmpty()) {
                annotationsBuffer.add(
                    renderedAnnotation + (if (this is PsiParameter) " " else "\n")
                )
            }
        }

        val resultBuffer = StringBuilder(annotationsBuffer.joinToString(separator = ""))
        if (!(this is PsiMethod && isConstructor && containingClass?.isEnum == true) && (this !is PsiEnumConstant)) {
            for (modifier in PsiModifier.MODIFIERS.filter(::hasModifierProperty)) {
                if (modifier == PsiModifier.PRIVATE && (this as? PsiMember)?.containingClass?.isInterface == true) continue
                if ((modifier != PsiModifier.FINAL && modifier != PsiModifier.ABSTRACT) || !(this is PsiClass && isEnum)) {
                    resultBuffer.append(modifier).append(" ")
                }
            }
        }
        return resultBuffer.toString()
    }

    private fun getKDocComment(psiElement: PsiElement): String? {
        if (!keepKdocComments) return null
        val ktElement = psiElement.extractOriginalKtDeclaration() ?: return null
        if (psiElement is PsiField && ktElement is KtObjectDeclaration) {
            // Do not write KDoc on object instance field
            return null
        }

        val docComment = when {
            ktElement is KtProperty -> ktElement.docComment
            ktElement.docComment == null && ktElement is KtPropertyAccessor -> ktElement.property.docComment
            else -> ktElement.docComment
        } ?: return null

        if (psiElement is PsiMethod && psiElement.isConstructor && ktElement is KtClassOrObject) {
            // We don't want the class comment to be duplicated on <init>()
            return null
        }

        return extractComment(docComment)
    }

    private fun PsiElement.extractOriginalKtDeclaration(): KtDeclaration? {
        // This when is needed to avoid recursion
        val elementToExtract = when (this) {
            is KtLightParameter -> when (kotlinOrigin) {
                null -> method
                else -> return kotlinOrigin
            }
            else -> this
        }

        return when (elementToExtract) {
            is KtLightMember<*> -> {
                val origin = elementToExtract.lightMemberOrigin
                origin?.auxiliaryOriginalElement ?: origin?.originalElement ?: elementToExtract.kotlinOrigin
            }
            is KtLightElement<*, *> -> elementToExtract.kotlinOrigin
            else -> null
        } as? KtDeclaration
    }
}

private class UnresolvedQualifiersRecorder(ktFiles: Iterable<KtFile>) {
    val importsFromRoot: Set<String> by lazy {
        ktFiles.flatMap { it.importDirectives }
            .filter { !it.isAllUnder }
            .mapNotNull { im -> im.importPath?.fqName?.takeIf { it.isOneSegmentFQN() }?.asString() }
            .toSet()
    }

    private val _qualifiedNames = mutableSetOf<String>()
    private val _simpleNames = mutableSetOf<String>()
    val reportedTypes = mutableSetOf<String>()

    val qualifiedNames: Set<String>
        get() = _qualifiedNames
    val simpleNames: Set<String>
        get() = _simpleNames

    fun isEmpty(): Boolean =
        simpleNames.isEmpty()

    fun recordUnresolvedQualifier(qualifier: String) {
        val separated = qualifier.split(".")
        if (separated.size > 1) {
            _qualifiedNames += qualifier
            _simpleNames += separated.first()
        } else {
            _simpleNames += qualifier
        }
    }
}

private fun getDefaultExpression(type: PsiType): String = when (type) {
    PsiType.BYTE -> "0"
    PsiType.BOOLEAN -> "false"
    PsiType.CHAR -> "\'\\u0000\'"
    PsiType.SHORT -> "0"
    PsiType.INT -> "0"
    PsiType.LONG -> "0L"
    PsiType.FLOAT -> "0.0F"
    PsiType.DOUBLE -> "0.0"
    else -> "null"
}

private val JAVA_KEYWORDS = setOf(
    "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "default", "do",
    "double", "else", "enum", "extends", "false", "final", "finally", "float", "for", "goto", "if", "implements", "import",
    "instanceof", "int", "interface", "long", "native", "new", "null", "package", "private", "protected", "public", "return",
    "short", "static", "strictfp", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "true", "try",
    "void", "volatile", "while"
)
