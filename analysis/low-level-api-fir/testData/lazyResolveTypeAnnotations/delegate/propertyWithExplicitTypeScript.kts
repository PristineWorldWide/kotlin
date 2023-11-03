// SKIP_WHEN_OUT_OF_CONTENT_ROOT

import kotlin.properties.ReadWriteProperty
import kotlin.reflect.KProperty

@Target(AnnotationTarget.TYPE)
annotation class Anno(val position: String)
const val prop = 0

class Delegate : ReadWriteProperty<Any?, @Anno("super type ref $prop") List<@Anno("nested super type ref $prop") List<@Anno("nested nested super type ref $prop") Int>>> {
    override fun getValue(thisRef: Any?, property: KProperty<*>): @Anno("getValue type ref $prop") List<@Anno("getValue nested type ref $prop") List<@Anno("getValue nested nested type ref $prop") Int>> = 1
    override fun setValue(thisRef: Any?, property: KProperty<*>, value: @Anno("setValue type ref $prop") List<@Anno("setValue nested type ref $prop") List<@Anno("setValue nested nested type ref $prop") Int>>) {}
}

var property<caret>ToResolve by Delegate()
