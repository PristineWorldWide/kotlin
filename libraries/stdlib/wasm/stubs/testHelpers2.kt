/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package kotlin
import kotlin.internal.OnlyInputTypes

fun assert(x: Boolean) {
    if (!x) throw AssertionError("Assertion failed")
}

/** Asserts that the [expected] value is equal to the [actual] value, with an optional [message]. */
fun <@OnlyInputTypes T> assertEquals(expected: T, actual: T, message: String? = null) {
    if (expected != actual) throw AssertionError("assertEquals failed: expected:$expected, actual:$actual, message=$message")
}