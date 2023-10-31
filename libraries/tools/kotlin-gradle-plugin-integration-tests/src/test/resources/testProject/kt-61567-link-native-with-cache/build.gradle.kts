plugins {
    kotlin("multiplatform")
}

repositories {
    mavenLocal()
    mavenCentral()
}

kotlin {
    <SingleNativeTarget>()

    sourceSets {
        commonMain {
            dependencies {
                implementation("org.jetbrains.kotlinx:atomicfu:0.22.0")
            }
        }
    }
}
