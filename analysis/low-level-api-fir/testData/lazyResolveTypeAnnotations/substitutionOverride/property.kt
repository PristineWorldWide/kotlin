// MEMBER_NAME_FILTER: resolveMe
package second

@Target(AnnotationTarget.TYPE)
annotation class Anno(val position: String)
const val constant = "str"

abstract class S<caret>ubClass: AbstractClass<@Anno("type param: $constant") List<@Anno("nested type param: $constant") Collection<@Anno("nested nested type param: $constant")String>>>()

abstract class AbstractClass<T> {
    abstract var T.resolveMe: T
}
