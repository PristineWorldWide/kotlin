// IGNORE_K1_K2_ABI_DIFFERENCE: KT-62775

enum class A {
    X {
        val k = "K"

        val anonObject = object {
            inner class Inner {
                val x = "O" + k
            }

            val innerX = Inner().x

            override fun toString() = innerX
        }

        override val test = anonObject.toString()
    };

    abstract val test: String
}

fun box() = A.X.test
