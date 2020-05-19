package java.lang.reflect;

import sun.misc.Unsafe;

import java.lang.annotation.Annotation;

public final class Field extends AccessibleObject implements Member {
    private final Class<?> clazz;
    private final int offset;

    private final String name;
    private final Class<?> type;
    private final int mod;

    private Field() {
        throw new UnsupportedOperationException();
    }

    public Class<?> getDeclaringClass() {
        return clazz;
    }

    public String getName() {
        return name;
    }

    public int getModifiers() {
        return mod;
    }

    public boolean isEnumConstant() {
        // TODO
        return false;
    }

    public boolean isSynthetic() {
        return Modifier.isSynthetic(mod);
    }

    public Class<?> getType() {
        return type;
    }

    public Type getGenericType() {
        throw new UnsupportedOperationException();
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        if (Modifier.isPublic(mod))
            sb.append("public ");
        else if (Modifier.isPrivate(mod))
            sb.append("private ");

        if (Modifier.isStatic(mod))
            sb.append("static ");

        if (Modifier.isFinal(mod))
            sb.append("final ");

        if (Modifier.isTransient(mod))
            sb.append("transient ");

        if (Modifier.isVolatile(mod))
            sb.append("volatile ");

        sb.append(type.getName());
        sb.append(" ");
        sb.append(clazz.getName());
        sb.append(".");
        sb.append(name);

        return sb.toString();
    }

    public String toGenericString() {
        throw new UnsupportedOperationException();
    }

    private Object getObjectTarget(Object obj) {
        if (Modifier.isStatic(mod)) {
            if (obj != null)
                throw new IllegalArgumentException();
            return clazz;
        } else {
            if (!clazz.isInstance(obj))
                throw new IllegalArgumentException();
            return obj;
        }
    }

    private long getOffset() {
        return offset;
    }

    public Object get(Object obj) {
        if (type == Boolean.TYPE) {
            return getBoolean(obj);
        } else if (type == Byte.TYPE) {
            return getByte(obj);
        } else if (type == Short.TYPE) {
            return getShort(obj);
        } else if (type == Character.TYPE) {
            return getChar(obj);
        } else if (type == Integer.TYPE) {
            return getInt(obj);
        } else if (type == Long.TYPE) {
            return getLong(obj);
        } else if (type == Float.TYPE) {
            return getFloat(obj);
        } else if (type == Double.TYPE) {
            return getDouble(obj);
        } else {
            return Unsafe.getUnsafe().getObject(getObjectTarget(obj), getOffset());
        }
    }

    public boolean getBoolean(Object obj) {
        if (type != Boolean.TYPE)
            throw new IllegalArgumentException();
        return Unsafe.getUnsafe().getBoolean(getObjectTarget(obj), getOffset());
    }

    public byte getByte(Object obj) {
        if (type != Byte.TYPE)
            throw new IllegalArgumentException();
        return Unsafe.getUnsafe().getByte(getObjectTarget(obj), getOffset());
    }

    public char getChar(Object obj) {
        if (type != Character.TYPE)
            throw new IllegalArgumentException();
        return Unsafe.getUnsafe().getChar(getObjectTarget(obj), getOffset());
    }

    public short getShort(Object obj) {
        if (type != Short.TYPE)
            throw new IllegalArgumentException();
        return Unsafe.getUnsafe().getShort(getObjectTarget(obj), getOffset());
    }

    public int getInt(Object obj) {
        if (type != Integer.TYPE)
            throw new IllegalArgumentException();
        return Unsafe.getUnsafe().getInt(getObjectTarget(obj), getOffset());
    }

    public long getLong(Object obj) {
        if (type != Long.TYPE)
            throw new IllegalArgumentException();
        return Unsafe.getUnsafe().getLong(getObjectTarget(obj), getOffset());
    }

    public float getFloat(Object obj) {
        if (type != Float.TYPE)
            throw new IllegalArgumentException();
        return Unsafe.getUnsafe().getFloat(getObjectTarget(obj), getOffset());
    }

    public double getDouble(Object obj) {
        if (type != Double.TYPE)
            throw new IllegalArgumentException();
        return Unsafe.getUnsafe().getDouble(getObjectTarget(obj), getOffset());
    }

    public void set(Object obj, Object value) {
        if (type == Boolean.TYPE) {
            if (!(obj instanceof Boolean))
                throw new IllegalArgumentException();
            setBoolean(obj, (Boolean)obj);
        } else if (type == Byte.TYPE) {
            if (!(obj instanceof Byte))
                throw new IllegalArgumentException();
            setByte(obj, (Byte)obj);
        } else if (type == Short.TYPE) {
            if (!(obj instanceof Short))
                throw new IllegalArgumentException();
            setShort(obj, (Short)obj);
        } else if (type == Character.TYPE) {
            if (!(obj instanceof Character))
                throw new IllegalArgumentException();
            setChar(obj, (Character)obj);
        } else if (type == Integer.TYPE) {
            if (!(obj instanceof Integer))
                throw new IllegalArgumentException();
            setInt(obj, (Integer)obj);
        } else if (type == Long.TYPE) {
            if (!(obj instanceof Long))
                throw new IllegalArgumentException();
            setLong(obj, (Long)obj);
        } else if (type == Float.TYPE) {
            if (!(obj instanceof Float))
                throw new IllegalArgumentException();
            setFloat(obj, (Float)obj);
        } else if (type == Double.TYPE) {
            if (!(obj instanceof Double))
                throw new IllegalArgumentException();
            setDouble(obj, (Double)obj);
        } else {
            if (value != null && !type.isInstance(value))
                throw new IllegalArgumentException();
            Unsafe.getUnsafe().putObject(getObjectTarget(obj), getOffset(), value);
        }
    }

    public void setBoolean(Object obj, boolean value) {
        if (type != Boolean.TYPE)
            throw new IllegalArgumentException();
        Unsafe.getUnsafe().putBoolean(getObjectTarget(obj), getOffset(), value);
    }

    public void setByte(Object obj, byte value) {
        if (type != Byte.TYPE)
            throw new IllegalArgumentException();
        Unsafe.getUnsafe().putByte(getObjectTarget(obj), getOffset(), value);
    }

    public void setChar(Object obj, char value) {
        if (type != Character.TYPE)
            throw new IllegalArgumentException();
        Unsafe.getUnsafe().putChar(getObjectTarget(obj), getOffset(), value);
    }

    public void setShort(Object obj, short value) {
        if (type != Short.TYPE)
            throw new IllegalArgumentException();
        Unsafe.getUnsafe().putShort(getObjectTarget(obj), getOffset(), value);
    }

    public void setInt(Object obj, int value) {
        if (type != Integer.TYPE)
            throw new IllegalArgumentException();
        Unsafe.getUnsafe().putInt(getObjectTarget(obj), getOffset(), value);
    }

    public void setLong(Object obj, long value) {
        if (type != Long.TYPE)
            throw new IllegalArgumentException();
        Unsafe.getUnsafe().putLong(getObjectTarget(obj), getOffset(), value);
    }

    public void setFloat(Object obj, float value) {
        if (type != Float.TYPE)
            throw new IllegalArgumentException();
        Unsafe.getUnsafe().putFloat(getObjectTarget(obj), getOffset(), value);
    }

    public void setDouble(Object obj, double value) {
        if (type != Double.TYPE)
            throw new IllegalArgumentException();
        Unsafe.getUnsafe().putDouble(getObjectTarget(obj), getOffset(), value);
    }

    public <T extends Annotation> T getAnnotation(Class<T> annotationClass) {
        throw new UnsupportedOperationException();
    }

    public <T extends Annotation> T[] getAnnotationsByType(Class<T> annotationClass) {
        throw new UnsupportedOperationException();
    }

    public Annotation[] getDeclaredAnnotations() {
        throw new UnsupportedOperationException();
    }

    public AnnotatedType getAnnotatedType() {
        throw new UnsupportedOperationException();
    }

    public int _MOCHA_getOffset() {
        return offset;
    }
}
