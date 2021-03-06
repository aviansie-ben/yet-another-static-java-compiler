package java.lang;

import java.io.InputStream;
import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.AnnotatedType;
import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.net.URL;
import java.security.ProtectionDomain;
import java.util.Map;

public final class Class<T> implements Serializable, GenericDeclaration, Type, AnnotatedElement {
    transient ClassValue.ClassValueMap classValueMap;

    // These fields will be filled in by the compiler when creating static Class objects. The order of these fields is
    // hardcoded, so the compiler will need to be updated if they're rearranged.

    // WARNING: The meaning of a "vtable address" is different between the static interpreter and the runtime. As a
    // result, this is the *only* field that should ever hold a vtable address. Caching this value in a location that
    // might be populated by the static interpreter then read at runtime will cause undefined behaviour.
    private transient final int vtableAddress;
    private transient final String canonicalName;

    private transient Field[] declaredFields;

    private Class() {
        throw new UnsupportedOperationException();
    }

    public static Class<?> forName(String className) throws ClassNotFoundException {
        return forName(className, true, null);
    }

    public static Class<?> forName(String className, boolean initialize, ClassLoader loader) throws ClassNotFoundException {
        throw new ClassNotFoundException(className);
    }

    static byte[] getExecutableTypeAnnotationBytes(Executable e) {
        throw new UnsupportedOperationException();
    }

    static native Class<?> getPrimitiveClass(String name);

    private static native Class<?> getClassForVTable(int vtable);

    private static native boolean isArray0(int vtable);
    private static native boolean isPrimitive0(int vtable);

    private static native int getComponentType0(int vtable);

    @SuppressWarnings("unchecked")
    public <U> Class<? extends U> asSubclass(Class<U> clazz) {
        if (!clazz.isAssignableFrom(this))
            throw new ClassCastException(this.toString());
        return (Class<? extends U>)this;
    }

    @SuppressWarnings("unchecked")
    public T cast(Object obj) {
        if (obj != null && !this.isInstance(obj))
            throw new ClassCastException(/* TODO */);
        return (T)obj;
    }

    public boolean desiredAssertionStatus() {
        return false;
    }

    public AnnotatedType[] getAnnotatedInterfaces() {
        throw new UnsupportedOperationException();
    }

    public AnnotatedType getAnnotatedSuperclass() {
        throw new UnsupportedOperationException();
    }

    public <A extends Annotation> A getAnnotation(Class<A> annotationClass) {
        throw new UnsupportedOperationException();
    }

    public Annotation[] getAnnotations() {
        throw new UnsupportedOperationException();
    }

    public <A extends Annotation> A[] getAnnotationsByType(Class<A> annotationClass) {
        throw new UnsupportedOperationException();
    }

    public String getCanonicalName() {
        return canonicalName;
    }

    public Class<?> getClasses() {
        throw new UnsupportedOperationException();
    }

    ClassLoader getClassLoader0() {
        return null;
    }

    public ClassLoader getClassLoader() {
        return getClassLoader0();
    }

    public Class<?> getComponentType() {
        if (isArray())
            return getClassForVTable(getComponentType0(vtableAddress));
        else
            return null;
    }

    public Constructor<T> getConstructor(Class<?>... paramClasses) {
        throw new UnsupportedOperationException();
    }

    public Constructor<T>[] getConstructors() {
        throw new UnsupportedOperationException();
    }

    public <A extends Annotation> A getDeclaredAnnotation(Class<A> annotationClass) {
        throw new UnsupportedOperationException();
    }

    public Annotation[] getDeclaredAnnotations() {
        throw new UnsupportedOperationException();
    }

    public <A extends Annotation> A[] getDeclaredAnnotationsByType(Class<A> annotationClass) {
        throw new UnsupportedOperationException();
    }

    public Class<?>[] getDeclaredClasses() {
        throw new UnsupportedOperationException();
    }

    public Constructor<T> getDeclaredConstructor(Class<?>... paramClasses) {
        throw new UnsupportedOperationException();
    }

    public Constructor<?>[] getDeclaredConstructors() {
        throw new UnsupportedOperationException();
    }

    private Field getDeclaredFieldOrNull(String name) {
        for (Field f : getDeclaredFields()) {
            if (f.getName().equals(name)) {
                return f;
            }
        }

        return null;
    }

    public Field getDeclaredField(String name) throws NoSuchFieldException {
        Field f = getDeclaredFieldOrNull(name);
        if (f == null)
            throw new NoSuchFieldException();

        return f;
    }

    private static native Field[] getDeclaredFields0(int vtable);

    public Field[] getDeclaredFields() {
        if (declaredFields == null)
            declaredFields = getDeclaredFields0(vtableAddress);

        return declaredFields;
    }

    public Method getDeclaredMethod(String name, Class<?>... paramClasses) {
        throw new UnsupportedOperationException();
    }

    public Method[] getDeclaredMethods() {
        throw new UnsupportedOperationException();
    }

    public Class<?> getDeclaringClass() {
        throw new UnsupportedOperationException();
    }

    public Class<?> getEnclosingClass() {
        throw new UnsupportedOperationException();
    }

    public Constructor<?> getEnclosingConstructor() {
        throw new UnsupportedOperationException();
    }

    public Method getEnclosingMethod() {
        throw new UnsupportedOperationException();
    }

    public T[] getEnumConstants() {
        throw new UnsupportedOperationException();
    }

    public Field getField(String name) throws NoSuchFieldException {
        Field f = getDeclaredFieldOrNull(name);
        if (f == null) {
            Class<?> superclass = getSuperclass();
            if (superclass == null)
                throw new NoSuchFieldException();

            return superclass.getField(name);
        }

        return f;
    }

    public Field[] getFields() {
        throw new UnsupportedOperationException();
    }

    public Type[] getGenericInterfaces() {
        throw new UnsupportedOperationException();
    }

    public Type getGenericSuperclass() {
        throw new UnsupportedOperationException();
    }

    public Class<?>[] getInterfaces() {
        throw new UnsupportedOperationException();
    }

    public Method getMethod(String name, Class<?>... paramClasses) {
        throw new UnsupportedOperationException();
    }

    public Method[] getMethods() {
        throw new UnsupportedOperationException();
    }

    public int getModifiers() {
        throw new UnsupportedOperationException();
    }

    public String getName() {
        return canonicalName.replace('/', '.');
    }

    public Package getPackage() {
        throw new UnsupportedOperationException();
    }

    public ProtectionDomain getProtectionDomain() {
        throw new UnsupportedOperationException();
    }

    public URL getResource(String name) {
        throw new UnsupportedOperationException();
    }

    public InputStream getResourceAsStream(String name) {
        throw new UnsupportedOperationException();
    }

    public Object[] getSigners() {
        throw new UnsupportedOperationException();
    }

    public String getSimpleName() {
        throw new UnsupportedOperationException();
    }

    private static native int getSuperclass0(int vtable);

    @SuppressWarnings("unchecked")
    public Class<? super T> getSuperclass() {
        return (Class<? super T>)getClassForVTable(getSuperclass0(vtableAddress));
    }

    public String getTypeName() {
        throw new UnsupportedOperationException();
    }

    public TypeVariable<Class<T>>[] getTypeParameters() {
        throw new UnsupportedOperationException();
    }

    public boolean isAnnotation() {
        throw new UnsupportedOperationException();
    }

    public boolean isAnnotationPresent(Class<? extends Annotation> annotationClass) {
        throw new UnsupportedOperationException();
    }

    public boolean isAnonymousClass() {
        throw new UnsupportedOperationException();
    }

    public boolean isArray() {
        return isArray0(vtableAddress);
    }

    public boolean isAssignableFrom(Class<?> clazz) {
        throw new UnsupportedOperationException();
    }

    public boolean isEnum() {
        throw new UnsupportedOperationException();
    }

    public boolean isInstance(Object obj) {
        if (obj == null)
            return false;

        // TODO Array types, interfaces?
        for (Class<?> c = obj.getClass(); c != null; c = c.getSuperclass()) {
            if (c == this)
                return true;
        }

        return false;
    }

    public boolean isInterface() {
        throw new UnsupportedOperationException();
    }

    public boolean isLocalClass() {
        throw new UnsupportedOperationException();
    }

    public boolean isMemberClass() {
        throw new UnsupportedOperationException();
    }

    public boolean isPrimitive() {
        return isPrimitive0(vtableAddress);
    }

    public boolean isSynthetic() {
        throw new UnsupportedOperationException();
    }

    public T newInstance() {
        throw new UnsupportedOperationException();
    }

    public String toGenericString() {
        throw new UnsupportedOperationException();
    }

    public String toString() {
        throw new UnsupportedOperationException();
    }

    Map<String, T> enumConstantDirectory() {
        throw new UnsupportedOperationException();
    }

    sun.reflect.ConstantPool getConstantPool() {
        throw new UnsupportedOperationException();
    }

    boolean casAnnotationType(sun.reflect.annotation.AnnotationType oldType, sun.reflect.annotation.AnnotationType newType) {
        throw new UnsupportedOperationException();
    }

    sun.reflect.annotation.AnnotationType getAnnotationType() {
        throw new UnsupportedOperationException();
    }

    Map<Class<? extends Annotation>, Annotation> getDeclaredAnnotationMap() {
        throw new UnsupportedOperationException();
    }

    byte[] getRawAnnotations() {
        throw new UnsupportedOperationException();
    }

    byte[] getRawTypeAnnotations() {
        throw new UnsupportedOperationException();
    }

    T[] getEnumConstantsShared() {
        throw new UnsupportedOperationException();
    }

    void setSigners(Object[] signers) {}
}
