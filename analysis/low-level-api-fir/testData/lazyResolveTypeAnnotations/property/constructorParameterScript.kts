package properties

@Target(AnnotationTarget.TYPE)
annotation class Anno(val position: String)
const val constant = 0

class MyClass(val pr<caret>operty: @Anno("parameter type: $constant") List<@Anno("nested parameter type: $constant") List<@Anno("nested nested parameter type: $constant") Int>>)
