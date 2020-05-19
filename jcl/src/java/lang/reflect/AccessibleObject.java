package java.lang.reflect;

import java.lang.annotation.Annotation;

public class AccessibleObject implements AnnotatedElement {
    static final boolean accessible = true;
    public static void setAccessible(AccessibleObject[] objects, boolean accessible) throws SecurityException {}

    protected AccessibleObject() {}

    public <T extends Annotation> T getAnnotation(Class<T> annotationClass) {
        throw new UnsupportedOperationException();
    }

    public Annotation[] getAnnotations() {
        throw new UnsupportedOperationException();
    }

    public <T extends Annotation> T[] getAnnotationsByType(Class<T> annotationClass) {
        throw new UnsupportedOperationException();
    }

    public <T extends Annotation> T getDeclaredAnnotation(Class<T> annotationClass) {
        throw new UnsupportedOperationException();
    }

    public Annotation[] getDeclaredAnnotations() {
        throw new UnsupportedOperationException();
    }

    public <T extends Annotation> T[] getDeclaredAnnotationsByType(Class<T> annotationClass) {
        throw new UnsupportedOperationException();
    }

    public boolean isAnnotationPresent(Class<? extends Annotation> aClass) {
        throw new UnsupportedOperationException();
    }

    public boolean isAccessible() {
        return true;
    }

    public void setAccessible(boolean accessible) throws SecurityException {}
}
