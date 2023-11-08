// FIR_IDENTICAL
// LANGUAGE: +ContextReceivers
// ISSUE: KT-53718

class A

context(A)
val x: Int
    get() = 1

val x: Int
    get() = 2

context(A)
fun foo() {}

fun foo() {}
