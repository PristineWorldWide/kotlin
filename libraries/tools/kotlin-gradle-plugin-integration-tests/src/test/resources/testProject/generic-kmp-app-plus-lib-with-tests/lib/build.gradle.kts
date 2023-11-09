plugins {
    kotlin("multiplatform")
}

kotlin {
    jvm()
    js()
    <SingleNativeTarget>("native")
}

kotlin {
    sourceSets {
        val commonTest by getting {
            dependencies {
                implementation(kotlin("test"))
            }
        }
    }
}
