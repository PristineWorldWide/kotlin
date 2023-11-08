/*
 * Copyright 2010-2023 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.gradle.mpp.smoke

import org.jetbrains.kotlin.gradle.testbase.BuildOptions
import org.jetbrains.kotlin.gradle.testbase.KGPBaseTest
import org.jetbrains.kotlin.gradle.testbase.MppGradlePluginTests
import org.jetbrains.kotlin.gradle.testbase.OsCondition
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.condition.OS

@DisplayName("Basic incremental scenarios with Kotlin Multiplatform - K2")
@MppGradlePluginTests
open class BasicIncrementalCompilationIT : KGPBaseTest() {
    override val defaultBuildOptions: BuildOptions
        get() = super.defaultBuildOptions.copyEnsuringK2()

    //TODO type code

    /*
    for slightly faster execution, basic scenarios would be clumped together:

    Test1 {
        doFullBuild()
        icChange()
        rebuildAndAssert()
        icChange()
        rebuildAndAssert()
        icChange()
        rebuildAndAssert()
    }

    Test2 {
        //more of the same
    }
     */
}

/**
 * As an optimization, only use iOS agents for iOS builds, and only test iOS with K2
 *
 * For Basic test suit it makes sense to add K1 version, though.
 */
@DisplayName("Basic incremental scenarios with Kotlin Multiplatform - iOS - K2")
@OsCondition(supportedOn = [OS.MAC], enabledOnCI = [OS.MAC])
open class BasicIncrementalCompilationIosIT : BasicIncrementalCompilationIT() {

    //TODO type code
}

@DisplayName("Basic incremental scenarios with Kotlin Multiplatform - iOS - K1")
@OsCondition(supportedOn = [OS.MAC], enabledOnCI = [OS.MAC])
class BasicIncrementalCompilationIosK1IT : BasicIncrementalCompilationIosIT() {
    override val defaultBuildOptions: BuildOptions
        get() = super.defaultBuildOptions.copyEnsuringK1()
}

@DisplayName("Basic incremental scenarios with Kotlin Multiplatform - K1")
class BasicIncrementalCompilationK1IT : BasicIncrementalCompilationIT() {
    override val defaultBuildOptions: BuildOptions
        get() = super.defaultBuildOptions.copyEnsuringK1()
}