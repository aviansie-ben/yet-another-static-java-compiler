package sun.misc;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.security.ProtectionDomain;

public final class Unsafe {
    private static Unsafe theUnsafe = new Unsafe();

    public static final int INVALID_FIELD_OFFSET = -1;

    public static final int ARRAY_BOOLEAN_BASE_OFFSET = theUnsafe.arrayBaseOffset(boolean[].class);
    public static final int ARRAY_BYTE_BASE_OFFSET = theUnsafe.arrayBaseOffset(byte[].class);
    public static final int ARRAY_SHORT_BASE_OFFSET = theUnsafe.arrayBaseOffset(short[].class);
    public static final int ARRAY_CHAR_BASE_OFFSET = theUnsafe.arrayBaseOffset(char[].class);
    public static final int ARRAY_INT_BASE_OFFSET = theUnsafe.arrayBaseOffset(int[].class);
    public static final int ARRAY_LONG_BASE_OFFSET = theUnsafe.arrayBaseOffset(long[].class);
    public static final int ARRAY_FLOAT_BASE_OFFSET = theUnsafe.arrayBaseOffset(float[].class);
    public static final int ARRAY_DOUBLE_BASE_OFFSET = theUnsafe.arrayBaseOffset(double[].class);
    public static final int ARRAY_OBJECT_BASE_OFFSET = theUnsafe.arrayBaseOffset(Object[].class);

    public static final int ARRAY_BOOLEAN_INDEX_SCALE = theUnsafe.arrayIndexScale(boolean[].class);
    public static final int ARRAY_BYTE_INDEX_SCALE = theUnsafe.arrayIndexScale(byte[].class);
    public static final int ARRAY_SHORT_INDEX_SCALE = theUnsafe.arrayIndexScale(short[].class);
    public static final int ARRAY_CHAR_INDEX_SCALE = theUnsafe.arrayIndexScale(char[].class);
    public static final int ARRAY_INT_INDEX_SCALE = theUnsafe.arrayIndexScale(int[].class);
    public static final int ARRAY_LONG_INDEX_SCALE = theUnsafe.arrayIndexScale(long[].class);
    public static final int ARRAY_FLOAT_INDEX_SCALE = theUnsafe.arrayIndexScale(float[].class);
    public static final int ARRAY_DOUBLE_INDEX_SCALE = theUnsafe.arrayIndexScale(double[].class);
    public static final int ARRAY_OBJECT_INDEX_SCALE = theUnsafe.arrayIndexScale(Object[].class);

    public static final int ADDRESS_SIZE = theUnsafe.addressSize();

    public static Unsafe getUnsafe() {
        return theUnsafe;
    }

    private Unsafe() {}

    public native int addressSize();
    public native int pageSize();

    public native long allocateMemory(long len);
    public native long reallocateMemory(long addr, long len);
    public native void freeMemory(long addr);

    public native void copyMemory(long src, long dst, long len);
    public native void copyMemory(Object src, long srcOffset, Object dst, long dstOffset, long len);

    public native void setMemory(long addr, long len, byte val);
    public native void setMemory(Object obj, long offset, long len, byte val);

    public native int arrayBaseOffset(Class<?> clazz);
    public native int arrayIndexScale(Class<?> clazz);

    public Object staticFieldBase(Field f) {
        return f.getDeclaringClass();
    }

    public Object staticFieldBase(Class<?> clazz) {
        return clazz;
    }

    public long staticFieldOffset(Field f) {
        return f._MOCHA_getOffset();
    }

    public long objectFieldOffset(Field f) {
        return f._MOCHA_getOffset();
    }

    public int fieldOffset(Field f) {
        return (int) (Modifier.isStatic(f.getModifiers()) ? staticFieldOffset(f) : objectFieldOffset(f));
    }

    private native Object allocateInstanceInternal(Class<?> clazz);
    public Object allocateInstance(Class<?> clazz) throws InstantiationException {
        if (Modifier.isAbstract(clazz.getModifiers()) || Modifier.isInterface(clazz.getModifiers()) || clazz.isArray() || clazz.isPrimitive())
            throw new InstantiationException();

        ensureClassInitialized(clazz);
        return allocateInstanceInternal(clazz);
    }

    public native boolean tryMonitorEnter(Object obj);
    public native void monitorEnter(Object obj);
    public native void monitorExit(Object obj);

    public native void park(boolean isAbsolute, long time);
    public native void unpark(Object thread);

    public native void throwException(Throwable exception);

    public native boolean compareAndSwapInt(Object obj, long offset, int expectedVal, int newVal);
    public native boolean compareAndSwapLong(Object obj, long offset, long expectedVal, long newVal);
    public native boolean compareAndSwapObject(Object obj, long offset, Object expectedVal, Object newVal);

    public native int getAndAddInt(Object obj, long offset, int delta);
    public native long getAndAddLong(Object obj, long offset, long delta);

    public native int getAndSetInt(Object obj, long offset, int val);
    public native long getAndSetLong(Object obj, long offset, long val);
    public native Object getAndSetObject(Object obj, long offset, Object val);

    public native long getAddress(long addr);
    public native byte getByte(long addr);
    public native short getShort(long addr);
    public native char getChar(long addr);
    public native int getInt(long addr);
    public native long getLong(long addr);
    public native float getFloat(long addr);
    public native double getDouble(long addr);

    public native void putAddress(long addr, long val);
    public native void putByte(long addr, byte val);
    public native void putShort(long addr, short val);
    public native void putChar(long addr, char val);
    public native void putInt(long addr, int val);
    public native void putLong(long addr, long val);
    public native void putFloat(long addr, float val);
    public native void putDouble(long addr, double val);

    public native boolean getBoolean(Object obj, long offset);
    public native byte getByte(Object obj, long offset);
    public native short getShort(Object obj, long offset);
    public native char getChar(Object obj, long offset);
    public native int getInt(Object obj, long offset);
    public native long getLong(Object obj, long offset);
    public native float getFloat(Object obj, long offset);
    public native double getDouble(Object obj, long offset);
    public native Object getObject(Object obj, long offset);

    public native void putBoolean(Object obj, long offset, boolean val);
    public native void putByte(Object obj, long offset, byte val);
    public native void putShort(Object obj, long offset, short val);
    public native void putChar(Object obj, long offset, char val);
    public native void putInt(Object obj, long offset, int val);
    public native void putLong(Object obj, long offset, long val);
    public native void putFloat(Object obj, long offset, float val);
    public native void putDouble(Object obj, long offset, double val);
    public native void putObject(Object obj, long offset, Object val);

    public boolean getBoolean(Object obj, int offset) {
        return getBoolean(obj, (long)offset);
    }

    public byte getByte(Object obj, int offset) {
        return getByte(obj, (long)offset);
    }

    public short getShort(Object obj, int offset) {
        return getShort(obj, (long)offset);
    }

    public char getChar(Object obj, int offset) {
        return getChar(obj, (long)offset);
    }

    public int getInt(Object obj, int offset) {
        return getInt(obj, (long)offset);
    }

    public long getLong(Object obj, int offset) {
        return getLong(obj, (long)offset);
    }

    public float getFloat(Object obj, int offset) {
        return getFloat(obj, (long)offset);
    }

    public double getDouble(Object obj, int offset) {
        return getDouble(obj, (long)offset);
    }

    public Object getObject(Object obj, int offset) {
        return getObject(obj, (long)offset);
    }

    public void putBoolean(Object obj, int offset, boolean val) {
        putBoolean(obj, (long)offset, val);
    }

    public void putByte(Object obj, int offset, byte val) {
        putByte(obj, (long)offset, val);
    }

    public void putShort(Object obj, int offset, short val) {
        putShort(obj, (long)offset, val);
    }

    public void putChar(Object obj, int offset, char val) {
        putChar(obj, (long)offset, val);
    }

    public void putInt(Object obj, int offset, int val) {
        putInt(obj, (long)offset, val);
    }

    public void putLong(Object obj, int offset, long val) {
        putLong(obj, (long)offset, val);
    }

    public void putFloat(Object obj, int offset, float val) {
        putFloat(obj, (long)offset, val);
    }

    public void putDouble(Object obj, int offset, double val) {
        putDouble(obj, (long)offset, val);
    }

    public void putObject(Object obj, int offset, Object val) {
        putObject(obj, (long)offset, val);
    }

    public native boolean getBooleanVolatile(Object obj, long offset);
    public native byte getByteVolatile(Object obj, long offset);
    public native short getShortVolatile(Object obj, long offset);
    public native char getCharVolatile(Object obj, long offset);
    public native int getIntVolatile(Object obj, long offset);
    public native long getLongVolatile(Object obj, long offset);
    public native float getFloatVolatile(Object obj, long offset);
    public native double getDoubleVolatile(Object obj, long offset);
    public native Object getObjectVolatile(Object obj, long offset);

    public native void putBooleanVolatile(Object obj, long offset, boolean val);
    public native void putByteVolatile(Object obj, long offset, byte val);
    public native void putShortVolatile(Object obj, long offset, short val);
    public native void putCharVolatile(Object obj, long offset, char val);
    public native void putIntVolatile(Object obj, long offset, int val);
    public native void putLongVolatile(Object obj, long offset, long val);
    public native void putFloatVolatile(Object obj, long offset, float val);
    public native void putDoubleVolatile(Object obj, long offset, double val);
    public native void putObjectVolatile(Object obj, long offset, Object val);

    public native void putOrderedInt(Object obj, long offset, int val);
    public native void putOrderedLong(Object obj, long offset, long val);
    public native void putOrderedObject(Object obj, long offset, Object val);

    public Class<?> defineAnonymousClass(Class<?> hostClass, byte[] data, Object[] cpPatches) {
        throw new UnsupportedOperationException();
    }

    public Class<?> defineClass(String name, byte[] b, int off, int len) {
        throw new UnsupportedOperationException();
    }

    public Class<?> defineClass(String name, byte[] b, int off, int len, ClassLoader loader, ProtectionDomain protectionDomain) {
        throw new UnsupportedOperationException();
    }

    public native boolean shouldBeInitialized(Class<?> clazz);
    public native void ensureClassInitialized(Class<?> clazz);

    public int getLoadAverage(double[] loadAvg, int nElems) {
        throw new UnsupportedOperationException();
    }

    public native void loadFence();
    public native void storeFence();
    public native void fullFence();
}
