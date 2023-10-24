// TARGET_BACKEND: JVM
// IGNORE_BACKEND: ANDROID
//  ^ D8 merges method references with empty closure created by 'invokedynamic'
// IGNORE_K1_K2_ABI_DIFFERENCE: KT-62858
// FILE: test.kt

fun checkNotEqual(marker: String, x: Any, y: Any) {
    if (x == y || y == x) throw AssertionError("$marker: $x and $y should NOT be equal")
}

private fun id(f: Runnable): Any = f

fun target1() {}
fun target2() {}

fun adapted1(s: String? = null): String = s!!
fun adapted2(vararg s: String): String = s[0]

fun box(): String {
    // Since 1.0, SAM wrappers for Java do not implement equals/hashCode
    checkNotEqual("id(::target1), id(::target1)", id(::target1), id(::target1))
    checkNotEqual("id(::target1), target1FromOtherFile()", id(::target1), target1FromOtherFile())
    checkNotEqual("id(::target1), id(::target2)", id(::target1), id(::target2))

    checkNotEqual("id(::adapted1), id(::adapted1)", id(::adapted1), id(::adapted1))
    checkNotEqual("id(::adapted1), adapted1FromOtherFile()", id(::adapted1), adapted1FromOtherFile())
    checkNotEqual("id(::adapted2), id(::adapted2)", id(::adapted2), id(::adapted2))
    checkNotEqual("id(::adapted2), adapted2FromOtherFile()", id(::adapted2), adapted2FromOtherFile())
    checkNotEqual("id(::adapted1), id(::adapted2)", id(::adapted1), id(::adapted2))

    return "OK"
}

// FILE: fromOtherFile.kt

private fun id(f: Runnable): Any = f

fun target1FromOtherFile(): Any = id(::target1)
fun adapted1FromOtherFile(): Any = id(::adapted1)
fun adapted2FromOtherFile(): Any = id(::adapted2)
