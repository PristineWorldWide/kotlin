import kotlin.test.Test
import kotlin.test.assertEquals

class AdvancedTest {

    @Test
    fun jvmUtilReturns400() {
        assertEquals(400, libJvmPlatformUtil())
    }
}