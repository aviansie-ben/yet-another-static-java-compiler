package java.lang.reflect;

import sun.reflect.ConstructorAccessor;
import sun.reflect.LangReflectAccess;
import sun.reflect.MethodAccessor;

public class ReflectAccess implements LangReflectAccess {
    public Field newField(Class<?> aClass, String s, Class<?> aClass1, int i, int i1, String s1, byte[] bytes) {
        throw new UnsupportedOperationException();
    }

    public Method newMethod(Class<?> aClass, String s, Class<?>[] classes, Class<?> aClass1, Class<?>[] classes1, int i, int i1, String s1, byte[] bytes, byte[] bytes1, byte[] bytes2) {
        throw new UnsupportedOperationException();
    }

    public <T> Constructor<T> newConstructor(Class<T> aClass, Class<?>[] classes, Class<?>[] classes1, int i, int i1, String s, byte[] bytes, byte[] bytes1) {
        throw new UnsupportedOperationException();
    }

    public MethodAccessor getMethodAccessor(Method method) {
        throw new UnsupportedOperationException();
    }

    public void setMethodAccessor(Method method, MethodAccessor methodAccessor) {
        throw new UnsupportedOperationException();
    }

    public ConstructorAccessor getConstructorAccessor(Constructor<?> constructor) {
        throw new UnsupportedOperationException();
    }

    public void setConstructorAccessor(Constructor<?> constructor, ConstructorAccessor constructorAccessor) {
        throw new UnsupportedOperationException();
    }

    public byte[] getExecutableTypeAnnotationBytes(Executable executable) {
        throw new UnsupportedOperationException();
    }

    public int getConstructorSlot(Constructor<?> constructor) {
        throw new UnsupportedOperationException();
    }

    public String getConstructorSignature(Constructor<?> constructor) {
        throw new UnsupportedOperationException();
    }

    public byte[] getConstructorAnnotations(Constructor<?> constructor) {
        throw new UnsupportedOperationException();
    }

    public byte[] getConstructorParameterAnnotations(Constructor<?> constructor) {
        throw new UnsupportedOperationException();
    }

    public Method copyMethod(Method method) {
        throw new UnsupportedOperationException();
    }

    public Field copyField(Field field) {
        throw new UnsupportedOperationException();
    }

    public <T> Constructor<T> copyConstructor(Constructor<T> constructor) {
        throw new UnsupportedOperationException();
    }
}
