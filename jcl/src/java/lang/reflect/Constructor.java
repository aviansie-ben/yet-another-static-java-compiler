package java.lang.reflect;

import java.lang.annotation.Annotation;

public class Constructor<T> extends Executable {
    private Constructor() {
        throw new UnsupportedOperationException();
    }

    public AnnotatedType getAnnotatedReceiverType() {
        throw new UnsupportedOperationException();
    }

    public AnnotatedType getAnnotatedReturnType() {
        throw new UnsupportedOperationException();
    }

    public Class<T> getDeclaringClass() {
        throw new UnsupportedOperationException();
    }

    public Class<?>[] getExceptionTypes() {
        throw new UnsupportedOperationException();
    }

    public Type[] getGenericExceptionTypes() {
        throw new UnsupportedOperationException();
    }

    public Type[] getGenericParameterTypes() {
        throw new UnsupportedOperationException();
    }

    public int getModifiers() {
        throw new UnsupportedOperationException();
    }

    public String getName() {
        throw new UnsupportedOperationException();
    }

    public Annotation[][] getParameterAnnotations() {
        throw new UnsupportedOperationException();
    }

    public int getParameterCount() {
        throw new UnsupportedOperationException();
    }

    public Class<?>[] getParameterTypes() {
        throw new UnsupportedOperationException();
    }

    public TypeVariable<Constructor<T>>[] getTypeParameters() {
        throw new UnsupportedOperationException();
    }

    public boolean isSynthetic() {
        throw new UnsupportedOperationException();
    }

    public boolean isVarArgs() {
        throw new UnsupportedOperationException();
    }

    public Object newInstance(Object... initArgs) {
        throw new UnsupportedOperationException();
    }

    public String toGenericString() {
        throw new UnsupportedOperationException();
    }
}
