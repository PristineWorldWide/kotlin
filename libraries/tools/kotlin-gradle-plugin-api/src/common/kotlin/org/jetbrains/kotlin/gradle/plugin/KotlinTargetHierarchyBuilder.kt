// DO NOT EDIT MANUALLY! Generated by org.jetbrains.kotlin.generators.gradle.dsl.MppKotlinTargetHierarchyBuilderCodegenKt

package org.jetbrains.kotlin.gradle.plugin

import org.jetbrains.kotlin.gradle.ExperimentalKotlinGradlePluginApi
import org.jetbrains.kotlin.konan.target.DeprecatedTargetAPI

@KotlinTargetsDsl
@ExperimentalKotlinGradlePluginApi
interface KotlinTargetHierarchyBuilder {
    /* Declaring groups */
    fun common(build: KotlinTargetHierarchyBuilder.() -> Unit) = group("common", build)
    fun group(name: String, build: KotlinTargetHierarchyBuilder.() -> Unit = {})

    /* low-level APIs */
    fun addCompilations(predicate: (KotlinCompilation<*>) -> Boolean)
    fun removeCompilations(predicate: (KotlinCompilation<*>) -> Boolean)
    fun filterCompilations(predicate: (KotlinCompilation<*>) -> Boolean) =
        removeCompilations { !predicate(it) }

    /* Convenient groups */
    fun anyNative()
    fun anyApple()
    fun anyIos()
    fun anyWatchos()
    fun anyMacos()
    fun anyTvos()
    fun anyMingw()
    fun anyLinux()
    fun anyAndroidNative()
    fun anyJs()

    /* Actual targets */
    fun anyJvm()
    fun anyAndroid()
    fun anyAndroidNativeX64()
    fun anyAndroidNativeX86()
    fun anyAndroidNativeArm32()
    fun anyAndroidNativeArm64()

    @DeprecatedTargetAPI
    fun anyIosArm32()
    fun anyIosArm64()
    fun anyIosX64()
    fun anyIosSimulatorArm64()
    fun anyWatchosArm32()
    fun anyWatchosArm64()

    @DeprecatedTargetAPI
    fun anyWatchosX86()
    fun anyWatchosX64()
    fun anyWatchosSimulatorArm64()
    fun anyWatchosDeviceArm64()
    fun anyTvosArm64()
    fun anyTvosX64()
    fun anyTvosSimulatorArm64()
    fun anyLinuxX64()

    @DeprecatedTargetAPI
    fun anyMingwX86()
    fun anyMingwX64()
    fun anyMacosX64()
    fun anyMacosArm64()
    fun anyLinuxArm64()

    @DeprecatedTargetAPI
    fun anyLinuxArm32Hfp()

    @DeprecatedTargetAPI
    fun anyLinuxMips32()

    @DeprecatedTargetAPI
    fun anyLinuxMipsel32()

    @DeprecatedTargetAPI
    fun anyWasm32()
}
