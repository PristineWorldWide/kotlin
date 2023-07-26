/*
 * Copyright 2010-2023 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.library.abi.impl

import it.unimi.dsi.fastutil.objects.ObjectOpenHashSet
import org.jetbrains.kotlin.backend.common.serialization.*
import org.jetbrains.kotlin.backend.common.serialization.encodings.*
import org.jetbrains.kotlin.backend.common.serialization.encodings.BinarySymbolData.SymbolKind.CLASS_SYMBOL
import org.jetbrains.kotlin.backend.common.serialization.encodings.BinarySymbolData.SymbolKind.TYPE_PARAMETER_SYMBOL
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.ir.util.IdSignature
import org.jetbrains.kotlin.ir.util.IdSignature.*
import org.jetbrains.kotlin.ir.util.IdSignatureRenderer
import org.jetbrains.kotlin.ir.util.render
import org.jetbrains.kotlin.library.*
import org.jetbrains.kotlin.library.abi.*
import org.jetbrains.kotlin.library.abi.AbiClassifierReference.ClassReference
import org.jetbrains.kotlin.library.abi.AbiTypeNullability.*
import org.jetbrains.kotlin.metadata.ProtoBuf
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.SpecialNames
import org.jetbrains.kotlin.types.Variance
import org.jetbrains.kotlin.utils.*
import org.jetbrains.kotlin.utils.addToStdlib.ifTrue
import java.io.File
import org.jetbrains.kotlin.konan.file.File as KFile
import org.jetbrains.kotlin.backend.common.serialization.proto.IrClass as ProtoClass
import org.jetbrains.kotlin.backend.common.serialization.proto.IrDeclaration as ProtoDeclaration
import org.jetbrains.kotlin.backend.common.serialization.proto.IrDeclarationBase as ProtoDeclarationBase
import org.jetbrains.kotlin.backend.common.serialization.proto.IrEnumEntry as ProtoEnumEntry
import org.jetbrains.kotlin.backend.common.serialization.proto.IrFile as ProtoFile
import org.jetbrains.kotlin.backend.common.serialization.proto.IrFunctionBase as ProtoFunctionBase
import org.jetbrains.kotlin.backend.common.serialization.proto.IrProperty as ProtoProperty
import org.jetbrains.kotlin.backend.common.serialization.proto.IrValueParameter as ProtoValueParameter
import org.jetbrains.kotlin.backend.common.serialization.proto.IrConstructorCall as ProtoConstructorCall
import org.jetbrains.kotlin.backend.common.serialization.proto.IrType as ProtoType
import org.jetbrains.kotlin.backend.common.serialization.proto.IrDefinitelyNotNullType as ProtoIrDefinitelyNotNullType
import org.jetbrains.kotlin.backend.common.serialization.proto.IrSimpleType as ProtoSimpleType
import org.jetbrains.kotlin.backend.common.serialization.proto.IrSimpleTypeNullability as ProtoSimpleTypeNullability
import org.jetbrains.kotlin.backend.common.serialization.proto.IrSimpleTypeLegacy as ProtoSimpleTypeLegacy
import org.jetbrains.kotlin.backend.common.serialization.proto.IrTypeParameter as ProtoTypeParameter
import org.jetbrains.kotlin.backend.common.serialization.IrFlags as ProtoFlags

@ExperimentalLibraryAbiReader
internal class LibraryAbiReaderImpl(libraryFile: File, filters: List<AbiReadingFilter>) {
    private val library = resolveSingleFileKlib(
        KFile(libraryFile.absolutePath),
        strategy = ToolingSingleFileKlibResolveStrategy
    )

    private val compositeFilter: AbiReadingFilter.Composite? = if (filters.isNotEmpty()) AbiReadingFilter.Composite(filters) else null

    fun readAbi(): LibraryAbi {
        val supportedSignatureVersions = readSupportedSignatureVersions()

        return LibraryAbi(
            manifest = readManifest(),
            uniqueName = library.uniqueName,
            signatureVersions = supportedSignatureVersions,
            topLevelDeclarations = LibraryDeserializer(library, supportedSignatureVersions, compositeFilter).deserialize()
        )
    }

    private fun readManifest(): LibraryManifest {
        val versions = library.versions
        return LibraryManifest(
            platform = library.builtInsPlatform,
            nativeTargets = library.nativeTargets.sorted(),
            compilerVersion = versions.compilerVersion,
            abiVersion = versions.abiVersion?.toString(),
            libraryVersion = versions.libraryVersion,
            irProviderName = library.irProviderName
        )
    }

    private fun readSupportedSignatureVersions(): Set<AbiSignatureVersion> {
        return library.versions.irSignatureVersions.mapTo(hashSetOf()) { AbiSignatureVersions.resolveByVersionNumber(it.number) }
    }
}

@ExperimentalLibraryAbiReader
private class LibraryDeserializer(
    private val library: KotlinLibrary,
    supportedSignatureVersions: Set<AbiSignatureVersion>,
    private val compositeFilter: AbiReadingFilter.Composite?
) {
    private val interner = IrInterningService()

    private val annotationsInterner = object {
        private val uniqueAnnotationClassNames = ObjectOpenHashSet<AbiQualifiedName>()
        fun intern(annotationClassName: AbiQualifiedName): AbiQualifiedName = uniqueAnnotationClassNames.addOrGet(annotationClassName)
    }

    private val needV1Signatures = AbiSignatureVersions.Supported.V1 in supportedSignatureVersions
    private val needV2Signatures = AbiSignatureVersions.Supported.V2 in supportedSignatureVersions

    private fun <T : AbiDeclaration> T?.discardIfExcluded(): T? =
        if (this != null && compositeFilter?.isDeclarationExcluded(this) == true) null else this

    private inner class FileDeserializer(fileIndex: Int) {
        private val fileReader = IrLibraryFileFromBytes(IrKlibBytesSource(library, fileIndex))

        private val packageName: AbiCompoundName
        private val topLevelDeclarationIds: List<Int>
        private val signatureDeserializer: IdSignatureDeserializer
        private val typeDeserializer: TypeDeserializer

        init {
            val proto = ProtoFile.parseFrom(library.file(fileIndex).codedInputStream, IrLibraryFileFromBytes.extensionRegistryLite)
            topLevelDeclarationIds = proto.declarationIdList

            val packageFQN = fileReader.deserializeFqName(proto.fqNameList)
            packageName = AbiCompoundName(packageFQN)

            val fileSignature = FileSignature(
                id = Any(), // Just an unique object.
                fqName = FqName(packageFQN),
                fileName = if (proto.hasFileEntry() && proto.fileEntry.hasName()) proto.fileEntry.name else "<unknown>"
            )
            signatureDeserializer = IdSignatureDeserializer(fileReader, fileSignature, interner)
            typeDeserializer = TypeDeserializer(fileReader, signatureDeserializer)
        }

        fun deserializeTo(output: MutableList<AbiDeclaration>) {
            if (compositeFilter?.isPackageExcluded(packageName) == true)
                return

            topLevelDeclarationIds.mapNotNullTo(output) { topLevelDeclarationId ->
                deserializeDeclaration(
                    proto = fileReader.declaration(topLevelDeclarationId),
                    containingEntity = ContainingEntity.Package(packageName),
                    parentTypeParameterResolver = null
                )
            }
        }

        private fun deserializeDeclaration(
            proto: ProtoDeclaration,
            containingEntity: ContainingEntity,
            parentTypeParameterResolver: TypeParameterResolver?
        ): AbiDeclaration? =
            when (proto.declaratorCase) {
                ProtoDeclaration.DeclaratorCase.IR_CLASS -> deserializeClass(proto.irClass, containingEntity, parentTypeParameterResolver)
                ProtoDeclaration.DeclaratorCase.IR_CONSTRUCTOR -> deserializeFunction(
                    proto.irConstructor.base,
                    isConstructor = true,
                    containingEntity,
                    parentTypeParameterResolver
                )
                ProtoDeclaration.DeclaratorCase.IR_FUNCTION -> deserializeFunction(
                    proto.irFunction.base,
                    isConstructor = false,
                    containingEntity,
                    parentTypeParameterResolver
                )
                ProtoDeclaration.DeclaratorCase.IR_PROPERTY -> deserializeProperty(
                    proto.irProperty,
                    containingEntity,
                    parentTypeParameterResolver
                )
                ProtoDeclaration.DeclaratorCase.IR_ENUM_ENTRY -> deserializeEnumEntry(proto.irEnumEntry, containingEntity)
                else -> null
            }.discardIfExcluded()

        private fun deserializeClass(
            proto: ProtoClass,
            containingEntity: ContainingEntity,
            parentTypeParameterResolver: TypeParameterResolver?,
        ): AbiClass? {
            val annotations = deserializeAnnotations(proto.base)
            val containingClassModality = (containingEntity as? ContainingEntity.Class)?.modality

            if (!computeVisibilityStatus(proto.base, annotations, containingClassModality).isPubliclyVisible)
                return null

            val flags = ClassFlags.decode(proto.base.flags)

            val modality = flags.modality.toAbiModality(
                containingClassModality = /* Open nested classes in final class remain open. */ null
            )

            val qualifiedName = deserializeQualifiedName(proto.name, containingEntity)
            val thisClassEntity = ContainingEntity.Class(qualifiedName, modality)

            // Note: For inner classes pass the `parentTypeParameterResolver` to the constructor so that is could be
            // possible to resolve TPs by delegating to the parent TP resolver. For non-inner classes just keep
            // "level" to facilitate the proper TP numbering.
            val thisClassTypeParameterResolver = TypeParameterResolver(
                declarationName = qualifiedName,
                parent = if (flags.isInner) parentTypeParameterResolver else null,
                levelAdjustment = if (!flags.isInner && parentTypeParameterResolver != null) parentTypeParameterResolver.level + 1 else 0
            )

            val memberDeclarations = proto.declarationList.memoryOptimizedMapNotNull { declaration ->
                deserializeDeclaration(declaration, containingEntity = thisClassEntity, thisClassTypeParameterResolver)
            }

            return AbiClassImpl(
                qualifiedName = qualifiedName,
                signatures = deserializeSignatures(proto.base),
                annotations = annotations,
                modality = modality,
                kind = when (val kind = flags.kind) {
                    ClassKind.CLASS -> AbiClassKind.CLASS
                    ClassKind.INTERFACE -> AbiClassKind.INTERFACE
                    ClassKind.OBJECT -> AbiClassKind.OBJECT
                    ClassKind.ENUM_CLASS -> AbiClassKind.ENUM_CLASS
                    ClassKind.ANNOTATION_CLASS -> AbiClassKind.ANNOTATION_CLASS
                    ClassKind.ENUM_ENTRY -> error("Unexpected class kind: $kind")
                },
                isInner = flags.isInner,
                isValue = flags.isValue,
                isFunction = flags.isFun,
                superTypes = deserializeTypes(proto.superTypeList, thisClassTypeParameterResolver) { type ->
                    !isKotlinBuiltInType(type, KOTLIN_ANY_QUALIFIED_NAME, DEFINITELY_NOT_NULL) && typeDeserializer.isPubliclyVisible(type)
                },
                declarations = memberDeclarations,
                typeParameters = deserializeTypeParameters(proto.typeParameterList, thisClassTypeParameterResolver)
            )
        }

        private inline fun deserializeTypes(
            typeIds: List<Int>,
            typeParameterResolver: TypeParameterResolver,
            predicate: (AbiType) -> Boolean
        ): List<AbiType> = typeIds.memoryOptimizedMapNotNull { typeId ->
            typeDeserializer.deserializeType(typeId, typeParameterResolver).takeIf(predicate)
        }

        private fun deserializeEnumEntry(proto: ProtoEnumEntry, containingEntity: ContainingEntity): AbiEnumEntry {
            return AbiEnumEntryImpl(
                qualifiedName = deserializeQualifiedName(proto.name, containingEntity),
                signatures = deserializeSignatures(proto.base),
                annotations = deserializeAnnotations(proto.base)
            )
        }

        private fun deserializeFunction(
            proto: ProtoFunctionBase,
            isConstructor: Boolean,
            containingEntity: ContainingEntity,
            parentTypeParameterResolver: TypeParameterResolver?
        ): AbiFunction? {
            val annotations = deserializeAnnotations(proto.base)

            val containingClassModality = when (containingEntity) {
                is ContainingEntity.Class -> containingEntity.modality
                is ContainingEntity.Property -> containingEntity.containingClassModality
                else -> null
            }

            val parentVisibilityStatus = (containingEntity as? ContainingEntity.Property)?.propertyVisibilityStatus
            if (!computeVisibilityStatus(proto.base, annotations, containingClassModality, parentVisibilityStatus).isPubliclyVisible)
                return null

            val flags = FunctionFlags.decode(proto.base.flags)
            if (flags.isFakeOverride) // TODO: FO of class with supertype from interop library
                return null

            val nameAndType = BinaryNameAndType.decode(proto.nameType)
            val functionName = deserializeQualifiedName(
                nameId = nameAndType.nameIndex,
                containingEntity = containingEntity
            )

            val thisFunctionTypeParameterResolver = when {
                isConstructor -> {
                    // Reuse the TP resolved from the class as far as constructors can't have own TPs.
                    parentTypeParameterResolver!!
                }
                containingEntity is ContainingEntity.Property -> {
                    // 1. A TP of a serialized property accessor has signature that points to the property itself.
                    //    So for the need of TP resolution it's necessary to pass the name of the property to the TP resolver.
                    // 2. Properties don't have their own TPs, but their accessors can have TPs. This means that there is
                    //    no need to create a TP resolver for a property, only for the accessor. To make rendering of
                    //    accessor's TPs consistent with the position of the accessor inside the declaration's tree,
                    //    it's necessary to adjust the "level" field inside the TP resolver by 1.
                    TypeParameterResolver(containingEntity.propertyName, parentTypeParameterResolver, levelAdjustment = 1)
                }
                else -> TypeParameterResolver(functionName, parentTypeParameterResolver)
            }

            val extensionReceiver = if (proto.hasExtensionReceiver())
                deserializeValueParameter(proto.extensionReceiver, thisFunctionTypeParameterResolver)
            else
                null
            val contextReceiversCount = if (proto.hasContextReceiverParametersCount()) proto.contextReceiverParametersCount else 0

            val allValueParameters = ArrayList<AbiValueParameter>()
            allValueParameters.addIfNotNull(extensionReceiver)
            proto.valueParameterList.mapTo(allValueParameters) { deserializeValueParameter(it, thisFunctionTypeParameterResolver) }

            return if (isConstructor) {
                check(extensionReceiver == null) { "Unexpected extension receiver found for constructor $functionName" }

                AbiConstructorImpl(
                    qualifiedName = functionName,
                    signatures = deserializeSignatures(proto.base),
                    annotations = annotations,
                    isInline = flags.isInline,
                    contextReceiverParametersCount = contextReceiversCount,
                    valueParameters = allValueParameters.compact()
                )
            } else {
                // Show only a non-trivial return type for the others.
                val nonTrivialReturnType = typeDeserializer.deserializeType(nameAndType.typeIndex, thisFunctionTypeParameterResolver)
                    .takeUnless { isKotlinBuiltInType(it, KOTLIN_UNIT_QUALIFIED_NAME, DEFINITELY_NOT_NULL) }

                AbiFunctionImpl(
                    qualifiedName = functionName,
                    signatures = deserializeSignatures(proto.base),
                    annotations = annotations,
                    modality = flags.modality.toAbiModality(containingClassModality),
                    isInline = flags.isInline,
                    isSuspend = flags.isSuspend,
                    typeParameters = deserializeTypeParameters(proto.typeParameterList, thisFunctionTypeParameterResolver),
                    hasExtensionReceiverParameter = extensionReceiver != null,
                    contextReceiverParametersCount = contextReceiversCount,
                    valueParameters = allValueParameters.compact(),
                    returnType = nonTrivialReturnType
                )
            }
        }

        private fun deserializeProperty(
            proto: ProtoProperty,
            containingEntity: ContainingEntity,
            typeParameterResolver: TypeParameterResolver?
        ): AbiProperty? {
            val annotations = deserializeAnnotations(proto.base)
            val containingClassModality = (containingEntity as? ContainingEntity.Class)?.modality

            val visibilityStatus = computeVisibilityStatus(proto.base, annotations, containingClassModality)
            if (!visibilityStatus.isPubliclyVisible)
                return null

            val flags = PropertyFlags.decode(proto.base.flags)
            if (flags.isFakeOverride) // TODO: FO of class with supertype from interop library
                return null

            val qualifiedName = deserializeQualifiedName(proto.name, containingEntity)
            val thisPropertyEntity = ContainingEntity.Property(qualifiedName, containingClassModality, visibilityStatus)

            return AbiPropertyImpl(
                qualifiedName = qualifiedName,
                signatures = deserializeSignatures(proto.base),
                annotations = annotations,
                modality = flags.modality.toAbiModality(containingClassModality),
                kind = when {
                    flags.isConst -> AbiPropertyKind.CONST_VAL
                    flags.isVar -> AbiPropertyKind.VAR
                    else -> AbiPropertyKind.VAL
                },
                getter = proto.hasGetter().ifTrue {
                    deserializeFunction(
                        proto = proto.getter.base,
                        isConstructor = false,
                        containingEntity = thisPropertyEntity,
                        parentTypeParameterResolver = typeParameterResolver
                    ).discardIfExcluded()
                },
                setter = proto.hasSetter().ifTrue {
                    deserializeFunction(
                        proto = proto.setter.base,
                        isConstructor = false,
                        containingEntity = thisPropertyEntity,
                        parentTypeParameterResolver = typeParameterResolver
                    ).discardIfExcluded()
                }
            )
        }

        private fun deserializeQualifiedName(nameId: Int, containingEntity: ContainingEntity): AbiQualifiedName {
            return containingEntity.computeNestedName(fileReader.string(nameId))
        }

        private fun deserializeSignatures(proto: ProtoDeclarationBase): AbiSignatures {
            val signature = deserializeIdSignature(proto.symbol)

            return AbiSignaturesImpl(
                signatureV1 = if (needV1Signatures) signature.render(IdSignatureRenderer.LEGACY) else null,
                signatureV2 = if (needV2Signatures) signature.render(IdSignatureRenderer.DEFAULT) else null
            )
        }

        private fun deserializeTypeParameters(
            protos: List<ProtoTypeParameter>,
            typeParameterResolver: TypeParameterResolver,
        ): List<AbiTypeParameter> = protos.memoryOptimizedMapIndexed { index, proto ->
            val flags = TypeParameterFlags.decode(proto.base.flags)

            AbiTypeParameterImpl(
                tag = typeParameterResolver.computeTypeParameterTag(index),
                variance = flags.variance.toAbiVariance(),
                isReified = flags.isReified,
                upperBounds = deserializeTypes(proto.superTypeList, typeParameterResolver) { type ->
                    !isKotlinBuiltInType(type, KOTLIN_ANY_QUALIFIED_NAME, MARKED_NULLABLE)
                }
            )
        }

        private fun deserializeAnnotations(proto: ProtoDeclarationBase): Set<AbiQualifiedName> {
            fun deserialize(annotation: ProtoConstructorCall): AbiQualifiedName {
                val signature = deserializeIdSignature(annotation.symbol)
                val annotationClassName = when {
                    signature is CommonSignature -> signature
                    signature is CompositeSignature && signature.container is FileSignature -> signature.inner as CommonSignature
                    else -> error("Unexpected annotation signature encountered: ${signature::class.java}, ${signature.render()}")
                }.extractQualifiedName { rawRelativeName -> rawRelativeName.removeSuffix(INIT_SUFFIX) }

                // Avoid duplicated instances of popular signature names:
                return annotationsInterner.intern(annotationClassName)
            }

            return when (proto.annotationCount) {
                0 -> return emptySet()
                1 -> return setOf(deserialize(proto.annotationList[0]))
                else -> proto.annotationList.mapTo(SmartSet.create(), ::deserialize)
            }
        }

        private fun computeVisibilityStatus(
            proto: ProtoDeclarationBase,
            annotations: Set<AbiQualifiedName>,
            containingClassModality: AbiModality?,
            parentVisibilityStatus: VisibilityStatus? = null
        ): VisibilityStatus = when (ProtoFlags.VISIBILITY.get(proto.flags.toInt())) {
            ProtoBuf.Visibility.PUBLIC -> VisibilityStatus.PUBLIC

            ProtoBuf.Visibility.PROTECTED -> {
                if (containingClassModality == AbiModality.FINAL)
                    VisibilityStatus.NON_PUBLIC
                else
                    VisibilityStatus.PUBLIC
            }

            ProtoBuf.Visibility.INTERNAL -> when {
                parentVisibilityStatus == VisibilityStatus.INTERNAL_PUBLISHED_API -> VisibilityStatus.INTERNAL_PUBLISHED_API
                PUBLISHED_API_CONSTRUCTOR_QUALIFIED_NAME in annotations -> VisibilityStatus.INTERNAL_PUBLISHED_API
                else -> VisibilityStatus.NON_PUBLIC
            }

            else -> VisibilityStatus.NON_PUBLIC
        }

        private fun deserializeIdSignature(symbolId: Long): IdSignature {
            val signatureId = BinarySymbolData.decode(symbolId).signatureId
            return signatureDeserializer.deserializeIdSignature(signatureId)
        }

        private fun deserializeValueParameter(
            proto: ProtoValueParameter,
            typeParameterResolver: TypeParameterResolver
        ): AbiValueParameter {
            val flags = ValueParameterFlags.decode(proto.base.flags)

            return AbiValueParameterImpl(
                type = typeDeserializer.deserializeType(BinaryNameAndType.decode(proto.nameType).typeIndex, typeParameterResolver),
                isVararg = proto.hasVarargElementType(),
                hasDefaultArg = proto.hasDefaultValue(),
                isNoinline = flags.isNoInline,
                isCrossinline = flags.isCrossInline
            )
        }
    }

    private sealed interface ContainingEntity {
        fun computeNestedName(simpleName: String): AbiQualifiedName

        class Package(val packageName: AbiCompoundName) : ContainingEntity {
            override fun computeNestedName(simpleName: String) = qualifiedName(packageName, simpleName)
        }

        class Class(val className: AbiQualifiedName, val modality: AbiModality) : ContainingEntity {
            override fun computeNestedName(simpleName: String) = qualifiedName(className, simpleName)
        }

        class Property(
            val propertyName: AbiQualifiedName,
            val containingClassModality: AbiModality?,
            val propertyVisibilityStatus: VisibilityStatus
        ) : ContainingEntity {
            override fun computeNestedName(simpleName: String) = qualifiedName(propertyName, simpleName)
        }

        companion object {
            private fun qualifiedName(packageName: AbiCompoundName, topLevelSimpleName: String) =
                AbiQualifiedName(packageName, AbiCompoundName(topLevelSimpleName))

            private fun qualifiedName(parentName: AbiQualifiedName, memberSimpleName: String) =
                AbiQualifiedName(
                    parentName.packageName,
                    AbiCompoundName("${parentName.relativeName}${AbiCompoundName.SEPARATOR}$memberSimpleName")
                )
        }
    }

    private class TypeParameterResolver(
        val declarationName: AbiQualifiedName,
        val parent: TypeParameterResolver?,
        levelAdjustment: Int = 0
    ) {
        val level: Int = (parent?.let { it.level + 1 } ?: 0) + levelAdjustment

        fun computeTypeParameterTag(index: Int): String {
            val tagPrefix = computeTagPrefix(index)
            return if (level > 0) "$tagPrefix$level" else tagPrefix
        }

        fun resolveTypeParameterTag(declarationName: AbiQualifiedName, index: Int): String {
            return if (declarationName == this.declarationName)
                computeTypeParameterTag(index)
            else
                parent?.resolveTypeParameterTag(declarationName, index)
                    ?: error("Type parameter with local index $index can not be resolved for $declarationName")
        }

        companion object {
            private const val ALPHABET_SIZE: Int = 'Z' - 'A' + 1

            private fun computeTagPrefix(index: Int): String {
                val result = mutableListOf<Char>()

                var quotient = index
                var remainder = quotient % ALPHABET_SIZE
                do {
                    result += ('A' + remainder)
                    quotient /= ALPHABET_SIZE
                    remainder = (quotient - 1) % ALPHABET_SIZE
                } while (quotient != 0)

                return result.reversed().joinToString(separator = "")
            }
        }
    }

    private class TypeDeserializer(
        private val libraryFile: IrLibraryFile,
        private val signatureDeserializer: IdSignatureDeserializer
    ) {
        private val cache = HashMap</* type id */ Int, AbiType>()
        private val nonPublicTopLevelClassNames = HashSet<AbiQualifiedName>()

        fun deserializeType(typeId: Int, typeParameterResolver: TypeParameterResolver): AbiType {
            return cache.computeIfAbsent(typeId) {
                val proto = libraryFile.type(typeId)
                when (val kindCase = proto.kindCase) {
                    ProtoType.KindCase.DNN -> deserializeDefinitelyNotNullType(proto.dnn, typeParameterResolver)
                    ProtoType.KindCase.SIMPLE -> deserializeSimpleType(proto.simple, typeParameterResolver)
                    ProtoType.KindCase.LEGACYSIMPLE -> deserializeSimpleType(proto.legacySimple, typeParameterResolver)
                    ProtoType.KindCase.DYNAMIC -> DynamicTypeImpl
                    ProtoType.KindCase.ERROR -> ErrorTypeImpl
                    ProtoType.KindCase.KIND_NOT_SET -> error("Unexpected IR type: $kindCase")
                }
            }
        }

        fun isPubliclyVisible(type: AbiType): Boolean =
            ((type as? AbiType.Simple)?.classifierReference as? ClassReference)?.className !in nonPublicTopLevelClassNames

        private fun deserializeDefinitelyNotNullType(
            proto: ProtoIrDefinitelyNotNullType,
            typeParameterResolver: TypeParameterResolver,
        ): AbiType {
            assert(proto.typesCount == 1) { "Only DefinitelyNotNull type is now supported" }

            val underlyingType = deserializeType(proto.getTypes(0), typeParameterResolver)
            return if (underlyingType is AbiType.Simple && underlyingType.nullability != DEFINITELY_NOT_NULL)
                SimpleTypeImpl(underlyingType.classifierReference, underlyingType.arguments, DEFINITELY_NOT_NULL)
            else
                underlyingType
        }

        private fun deserializeSimpleType(proto: ProtoSimpleType, typeParameterResolver: TypeParameterResolver): AbiType.Simple =
            deserializeSimpleType(
                symbolId = proto.classifier,
                typeArgumentIds = proto.argumentList,
                nullability = if (proto.hasNullability()) {
                    when (proto.nullability!!) {
                        ProtoSimpleTypeNullability.MARKED_NULLABLE -> MARKED_NULLABLE
                        ProtoSimpleTypeNullability.NOT_SPECIFIED -> NOT_SPECIFIED
                        ProtoSimpleTypeNullability.DEFINITELY_NOT_NULL -> DEFINITELY_NOT_NULL
                    }
                } else NOT_SPECIFIED,
                typeParameterResolver
            )

        private fun deserializeSimpleType(proto: ProtoSimpleTypeLegacy, typeParameterResolver: TypeParameterResolver): AbiType.Simple =
            deserializeSimpleType(
                symbolId = proto.classifier,
                typeArgumentIds = proto.argumentList,
                nullability = if (proto.hasHasQuestionMark() && proto.hasQuestionMark) MARKED_NULLABLE else NOT_SPECIFIED,
                typeParameterResolver
            )

        private fun deserializeSimpleType(
            symbolId: Long,
            typeArgumentIds: List<Long>,
            nullability: AbiTypeNullability,
            typeParameterResolver: TypeParameterResolver
        ): AbiType.Simple {
            val symbolData = BinarySymbolData.decode(symbolId)
            val signature = signatureDeserializer.deserializeIdSignature(symbolData.signatureId)
            val symbolKind = symbolData.kind

            return when {
                symbolKind == CLASS_SYMBOL && signature is CommonSignature -> {
                    // Publicly visible class or interface.
                    SimpleTypeImpl(
                        classifierReference = ClassReferenceImpl(
                            className = signature.extractQualifiedName()
                        ),
                        arguments = deserializeTypeArguments(typeArgumentIds, typeParameterResolver),
                        nullability = nullability
                    )
                }

                symbolKind == CLASS_SYMBOL && signature is CompositeSignature && signature.container is FileSignature -> {
                    // Non-publicly visible classifier. Practically, this can only be a private top-level interface
                    // that some publicly visible class inherits. Need to memoize it to avoid displaying it later among
                    // supertypes of the inherited class.
                    val className = (signature.inner as CommonSignature).extractQualifiedName()
                    nonPublicTopLevelClassNames += className

                    SimpleTypeImpl(
                        classifierReference = ClassReferenceImpl(className),
                        arguments = deserializeTypeArguments(typeArgumentIds, typeParameterResolver),
                        nullability = nullability
                    )
                }

                symbolKind == TYPE_PARAMETER_SYMBOL && signature is CompositeSignature -> {
                    // A type-parameter.
                    SimpleTypeImpl(
                        classifierReference = TypeParameterReferenceImpl(
                            tag = typeParameterResolver.resolveTypeParameterTag(
                                declarationName = (signature.container as CommonSignature).extractQualifiedName(),
                                index = (signature.inner as LocalSignature).index()
                            )
                        ),
                        arguments = emptyList(),
                        nullability = nullability
                    )
                }

                else -> error("Unexpected combination of symbol kind ($symbolKind) and a signature: ${signature::class.java}, ${signature.render()}")
            }
        }

        private fun deserializeTypeArguments(
            typeArgumentIds: List<Long>,
            typeParameterResolver: TypeParameterResolver
        ): List<AbiTypeArgument> {
            return if (typeArgumentIds.isEmpty())
                emptyList()
            else typeArgumentIds.memoryOptimizedMap { typeArgumentId ->
                val typeProjection = BinaryTypeProjection.decode(typeArgumentId)
                if (typeProjection.isStarProjection)
                    StarProjectionImpl
                else
                    TypeProjectionImpl(
                        type = deserializeType(typeProjection.typeIndex, typeParameterResolver),
                        variance = typeProjection.variance.toAbiVariance()
                    )
            }
        }
    }

    fun deserialize(): AbiTopLevelDeclarations {
        val topLevels = ArrayList<AbiDeclaration>()

        for (fileIndex in 0 until library.fileCount()) {
            FileDeserializer(fileIndex).deserializeTo(topLevels)
        }

        return AbiTopLevelDeclarationsImpl(topLevels.compact())
    }

    private enum class VisibilityStatus(val isPubliclyVisible: Boolean) {
        PUBLIC(true), INTERNAL_PUBLISHED_API(true), NON_PUBLIC(false)
    }

    companion object {
        private val INIT_SUFFIX = "." + SpecialNames.INIT.asString()

        private val KOTLIN_COMPOUND_NAME = AbiCompoundName("kotlin")
        private val PUBLISHED_API_CONSTRUCTOR_QUALIFIED_NAME = AbiQualifiedName(KOTLIN_COMPOUND_NAME, AbiCompoundName("PublishedApi"))
        private val KOTLIN_ANY_QUALIFIED_NAME = AbiQualifiedName(KOTLIN_COMPOUND_NAME, AbiCompoundName("Any"))
        private val KOTLIN_UNIT_QUALIFIED_NAME = AbiQualifiedName(KOTLIN_COMPOUND_NAME, AbiCompoundName("Unit"))

        private fun isKotlinBuiltInType(type: AbiType, className: AbiQualifiedName, nullability: AbiTypeNullability): Boolean {
            if (type !is AbiType.Simple || type.nullability != nullability) return false
            return (type.classifierReference as? ClassReference)?.className == className
        }

        private inline fun CommonSignature.extractQualifiedName(transformRelativeName: (String) -> String = { it }): AbiQualifiedName =
            AbiQualifiedName(AbiCompoundName(packageFqName), AbiCompoundName(transformRelativeName(declarationFqName)))

        private fun Modality.toAbiModality(containingClassModality: AbiModality?): AbiModality = when (this) {
            Modality.FINAL -> AbiModality.FINAL
            Modality.OPEN -> if (containingClassModality == AbiModality.FINAL) AbiModality.FINAL else AbiModality.OPEN
            Modality.ABSTRACT -> AbiModality.ABSTRACT
            Modality.SEALED -> AbiModality.SEALED
        }

        private fun Variance.toAbiVariance(): AbiVariance = when (this) {
            Variance.INVARIANT -> AbiVariance.INVARIANT
            Variance.IN_VARIANCE -> AbiVariance.IN
            Variance.OUT_VARIANCE -> AbiVariance.OUT
        }
    }
}
