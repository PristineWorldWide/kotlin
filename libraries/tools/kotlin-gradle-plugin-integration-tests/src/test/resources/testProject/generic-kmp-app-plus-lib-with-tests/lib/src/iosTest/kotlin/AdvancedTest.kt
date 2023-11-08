import kotlin.test.Test
import kotlin.test.assertEquals

class AdvancedTest {

    @Test
    fun iosUtilReturns800() {
        assertEquals(800, libIosPlatformUtil())
    }
}