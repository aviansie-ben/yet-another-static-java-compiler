package java.lang.reflect;

import java.lang.annotation.Annotation;

public abstract class Executable extends AccessibleObject implements Member, GenericDeclaration {
    Executable() {
        throw new UnsupportedOperationException();
    }

    public AnnotatedType[] getAnnotatedExceptionTypes() {
        throw new UnsupportedOperationException();
    }

    public AnnotatedType[] getAnnotatedParameterTypes() {
        throw new UnsupportedOperationException();
    }

    public AnnotatedType getAnnotatedReceiverType() {
        throw new UnsupportedOperationException();
    }

    public abstract AnnotatedType getAnnotatedReturnType();

    public abstract Class<?> getDeclaringClass();
    public abstract Class<?>[] getExceptionTypes();
    public abstract Type[] getGenericExceptionTypes();
    public abstract Type[] getGenericParameterTypes();
    public abstract int getModifiers();
    public abstract String getName();
    public abstract Annotation[][] getParameterAnnotations();

    public int getParameterCount() {
        throw new UnsupportedOperationException();
    }

    public Parameter[] getParameters() {
        throw new UnsupportedOperationException();
    }

    public abstract Class<?>[] getParameterTypes();
    public abstract TypeVariable<?>[] getTypeParameters();

    public boolean isSynthetic() {
        throw new UnsupportedOperationException();
    }

    public boolean isVarArgs() {
        throw new UnsupportedOperationException();
    }

    public abstract String toGenericString();
}
