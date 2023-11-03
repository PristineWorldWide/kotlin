package lowlevel

@Target(AnnotationTarget.TYPE)
annotation class Anno(val position: String)

const val prop = "str"

fun func<caret>tion(param: @Anno(prop) Type1<@Anno(prop) Type2>): @Anno(prop) Type3 {}