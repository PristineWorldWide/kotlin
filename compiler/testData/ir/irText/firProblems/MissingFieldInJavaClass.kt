// FIR_IDENTICAL
// TARGET_BACKEND: JVM
// ISSUE: KT-61362
// DUMP_EXTERNAL_CLASS: J
// DUMP_EXTERNAL_CLASS: X
// DUMP_EXTERNAL_CLASS: J1
// DUMP_EXTERNAL_CLASS: X1

// FILE: J.java

class J {
    public int f = 0;
    public static int s = 0;
}

// FILE: X.java

class X extends J {
}

// FILE: J1.java

class J1<T> {
    public T f = null;
    public static T s = null;
}

// FILE: X1.java

class X1<T> extends J1<T> {
}