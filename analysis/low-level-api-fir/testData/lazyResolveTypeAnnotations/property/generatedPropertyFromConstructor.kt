// MEMBER_NAME_FILTER: property
package properties

@Target(AnnotationTarget.TYPE)
annotation class Anno(val position: String)
const val constant = 0

class My<caret>Class(val property: @Anno("parameter type: $constant") List<@Anno("nested parameter type: $constant") List<@Anno("nested nested parameter type: $constant") Int>>)
