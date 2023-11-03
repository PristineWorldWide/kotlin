package lowlevel

@Target(AnnotationTarget.TYPE)
annotation class Anno(val position: String)

const val prop = "str"

fun func<caret>tion(): @Anno(prop) List<@Anno(prop) Type> {}