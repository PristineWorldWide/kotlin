/*
 * Copyright 2010-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE file.
 */

package org.jetbrains.kotlin.backend.konan.objcexport

import org.jetbrains.kotlin.backend.konan.*
import org.jetbrains.kotlin.backend.konan.descriptors.isInterface
import org.jetbrains.kotlin.backend.konan.driver.PhaseContext
import org.jetbrains.kotlin.backend.konan.llvm.CodeGenerator
import org.jetbrains.kotlin.backend.konan.llvm.objcexport.ObjCExportBlockCodeGenerator
import org.jetbrains.kotlin.backend.konan.llvm.objcexport.ObjCExportCodeGenerator
import org.jetbrains.kotlin.descriptors.CallableMemberDescriptor
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.descriptors.SourceFile
import org.jetbrains.kotlin.konan.exec.Command
import org.jetbrains.kotlin.konan.file.createTempFile
import java.io.File

internal class ObjCExportedInterface(
        val generatedClasses: Set<ClassDescriptor>,
        val categoryMembers: Map<ClassDescriptor, List<CallableMemberDescriptor>>,
        val topLevel: Map<SourceFile, List<CallableMemberDescriptor>>,
        val headerLines: List<String>,
        val namer: ObjCExportNamer,
        val stdlibNamer: ObjCExportStdlibNamer,
        val mapper: ObjCExportMapper
)

internal fun produceObjCExportInterface(
        headerGenerator: ObjCExportHeaderGenerator
): ObjCExportedInterface {

    headerGenerator.translateModule()
    return headerGenerator.buildInterface()
}

internal fun createObjCExportHeaderGenerator(
        context: PhaseContext,
        exportConfig: ObjCExportGlobalConfig,
        headerInfo: ObjCExportHeaderInfo,
        stdlibNamer: ObjCExportStdlibNamer,
): ObjCExportHeaderGenerator {
    require(headerInfo.modules.isNotEmpty())

    // TODO: emit RTTI to the same modules as classes belong to.
    //   Not possible yet, since ObjCExport translates the entire "world" API at once
    //   and can't do this per-module, e.g. due to global name conflict resolution.

    val unitSuspendFunctionExport = exportConfig.unitSuspendFunctionExport
    val mapper = ObjCExportMapper(exportConfig.frontendServices.deprecationResolver, unitSuspendFunctionExport = unitSuspendFunctionExport)
    val objcGenerics = exportConfig.objcGenerics
    val disableSwiftMemberNameMangling = exportConfig.disableSwiftMemberNameMangling
    val ignoreInterfaceMethodCollisions = exportConfig.ignoreInterfaceMethodCollisions
    val namer = ObjCExportNamerImpl(
            headerInfo.modules.toSet(),
            headerInfo.modules.first().module.builtIns,
            stdlibNamer,
            mapper,
            headerInfo.topLevelPrefix,
            local = false,
            objcGenerics = objcGenerics,
            disableSwiftMemberNameMangling = disableSwiftMemberNameMangling,
            ignoreInterfaceMethodCollisions = ignoreInterfaceMethodCollisions,
    )
    return ObjCExportHeaderGeneratorImpl(context, headerInfo.modules, mapper, namer, stdlibNamer, objcGenerics)
}

/**
 * Populate framework directory with headers, module and info.plist.
 */
internal fun createObjCFramework(
        config: KonanConfig,
        moduleDescriptor: ModuleDescriptor,
        exportedInterface: ObjCExportedInterface,
        frameworkDirectory: File
) {
    val frameworkName = frameworkDirectory.name.removeSuffix(".framework")
    val frameworkBuilder = FrameworkBuilder(
            config,
            infoPListBuilder = InfoPListBuilder(config),
            moduleMapBuilder = ModuleMapBuilder(),
            objCHeaderWriter = ObjCHeaderWriter(),
            mainPackageGuesser = MainPackageGuesser(),
    )
    frameworkBuilder.build(
            moduleDescriptor,
            frameworkDirectory,
            frameworkName,
            exportedInterface.headerLines,
            moduleDependencies = setOf("Foundation")
    )
}

// TODO: No need for such class in dynamic driver.
internal class ObjCExport(
        private val generationState: NativeGenerationState,
        moduleDescriptor: ModuleDescriptor,
        private val exportedInterface: ObjCExportedInterface?,
        private val codeSpec: ObjCExportCodeSpec?
) {
    private val config = generationState.config
    private val target get() = config.target
    private val topLevelNamePrefix get() = generationState.objCExportTopLevelNamePrefix

    val mapper: ObjCExportMapper = exportedInterface?.mapper ?: ObjCExportMapper(unitSuspendFunctionExport = config.unitSuspendFunctionObjCExport)

    val namer: ObjCExportNamer = exportedInterface?.namer ?: ObjCExportNamerImpl(
            setOf(ObjCExportModuleInfo(moduleDescriptor, true)),
            moduleDescriptor.builtIns,
            ObjCExportStdlibNamer.create(topLevelNamePrefix),
            mapper,
            topLevelNamePrefix,
            local = false
    )

    val stdlibNamer: ObjCExportStdlibNamer = exportedInterface?.stdlibNamer ?: ObjCExportStdlibNamer.create(
            topLevelNamePrefix
    )

    internal fun generate(codegen: CodeGenerator) {
        if (!target.family.isAppleFamily) return

        if (generationState.shouldDefineFunctionClasses) {
            ObjCExportBlockCodeGenerator(codegen).generate()
        }

        if (!config.isFinalBinary) return // TODO: emit RTTI to the same modules as classes belong to.

        val objCCodeGenerator = ObjCExportCodeGenerator(codegen, namer, mapper, stdlibNamer)

        exportedInterface?.generateWorkaroundForSwiftSR10177(generationState)

        objCCodeGenerator.generate(codeSpec)
        objCCodeGenerator.dispose()
    }
}

// See https://bugs.swift.org/browse/SR-10177
internal fun ObjCExportedInterface.generateWorkaroundForSwiftSR10177(generationState: NativeGenerationState) {
    // Code for all protocols from the header should get into the binary.
    // Objective-C protocols ABI is complicated (consider e.g. undocumented extended type encoding),
    // so the easiest way to achieve this (quickly) is to compile a stub by clang.

    val protocolsStub = listOf(
            "__attribute__((used)) static void __workaroundSwiftSR10177() {",
            buildString {
                append("    ")
                generatedClasses.forEach {
                    if (it.isInterface) {
                        val protocolName = namer.getClassOrProtocolName(it).objCName
                        append("@protocol($protocolName); ")
                    }
                }
            },
            "}"
    )

    val source = createTempFile("protocols", ".m").deleteOnExit()
    source.writeLines(headerLines + protocolsStub)

    val bitcode = createTempFile("protocols", ".bc").deleteOnExit()

    val clangCommand = generationState.config.clang.clangC(
            source.absolutePath,
            "-O2",
            "-emit-llvm",
            "-c", "-o", bitcode.absolutePath
    )

    val result = Command(clangCommand).getResult(withErrors = true)

    if (result.exitCode == 0) {
        generationState.llvm.additionalProducedBitcodeFiles += bitcode.absolutePath
    } else {
        // Note: ignoring compile errors intentionally.
        // In this case resulting framework will likely be unusable due to compile errors when importing it.
    }
}
