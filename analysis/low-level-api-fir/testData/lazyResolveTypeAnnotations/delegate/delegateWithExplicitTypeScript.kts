// MEMBER_NAME_FILTER: explicitType

@Target(AnnotationTarget.TYPE, AnnotationTarget.FUNCTION)
annotation class Anno(val position: String)
const val prop = 0

class Der<caret>ived(i: OriginalInterface) : OriginalInterface by i

interface OriginalInterface {
    @Anno("explicitType $prop")
    fun @Anno("receiver type $prop") Collection<@Anno("nested receiver type $prop") List<@Anno("nested nested receiver type $prop")String>>.explicitType(): @Anno("explicitType return type $prop") List<@Anno("explicitType nested return type $prop") List<@Anno("explicitType nested nested return type $prop") Int>> = 1
}
