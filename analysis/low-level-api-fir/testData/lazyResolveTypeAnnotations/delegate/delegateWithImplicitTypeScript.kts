// MEMBER_NAME_FILTER: implicitType

@Target(AnnotationTarget.TYPE, AnnotationTarget.FUNCTION)
annotation class Anno(val position: String)
const val prop = 0

class Der<caret>ived(i: OriginalInterface) : OriginalInterface by i

interface OriginalInterface {
    @Anno("implicitType $prop")
    fun @Anno("receiver type $prop") Collection<@Anno("nested receiver type $prop") List<@Anno("nested nested receiver type $prop")String>>.implicitType() = explicitType()
}

fun explicitType(): @Anno("explicitType return type $prop") List<@Anno("explicitType nested return type $prop") List<@Anno("explicitType nested nested return type $prop") Int>> = 1
