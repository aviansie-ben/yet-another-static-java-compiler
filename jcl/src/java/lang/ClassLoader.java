package java.lang;

import java.io.InputStream;
import java.net.URL;
import java.nio.ByteBuffer;
import java.security.ProtectionDomain;
import java.util.Enumeration;

public abstract class ClassLoader {
    public static ClassLoader getSystemClassLoader() {
        throw new UnsupportedOperationException("Mocha does not support dynamic classloading");
    }

    public static URL getSystemResource(String name) {
        throw new UnsupportedOperationException("Mocha does not support dynamic classloading");
    }

    public static InputStream getSystemResourceAsStream(String name) {
        throw new UnsupportedOperationException("Mocha does not support dynamic classloading");
    }

    public static Enumeration<URL> getSystemResources(String name) {
        throw new UnsupportedOperationException("Mocha does not support dynamic classloading");
    }

    protected static boolean registerAsParallelCapable() {
        return false;
    }

    static void checkClassLoaderPermission(ClassLoader classLoader, Class<?> c) {}

    static ClassLoader getClassLoader(Class<?> c) {
        return null;
    }

    protected ClassLoader() {
        throw new UnsupportedOperationException("Mocha does not support dynamic classloading");
    }

    protected ClassLoader(ClassLoader parent) {
        throw new UnsupportedOperationException("Mocha does not support dynamic classloading");
    }

    public void clearAssertionStatus() {
        throw new UnsupportedOperationException();
    }

    protected Class<?> defineClass(byte[] b, int off, int len) {
        throw new UnsupportedOperationException();
    }

    protected Class<?> defineClass(String name, byte[] b, int off, int len) {
        throw new UnsupportedOperationException();
    }

    protected Class<?> defineClass(String name, byte[] b, int off, int len, ProtectionDomain protectionDomain) {
        throw new UnsupportedOperationException();
    }

    protected Class<?> defineClass(String name, ByteBuffer b, ProtectionDomain protectionDomain) {
        throw new UnsupportedOperationException();
    }

    protected Package definePackage(String name, String specTitle, String specVersion, String specVendor, String implTitle, String implVersion, String implVendor, URL sealBase) {
        throw new UnsupportedOperationException();
    }

    protected Class<?> findClass(String name) {
        throw new UnsupportedOperationException();
    }

    protected String findLibrary(String libname) {
        throw new UnsupportedOperationException();
    }

    protected Class<?> findLoadedClass(String name) {
        throw new UnsupportedOperationException();
    }

    protected URL findResource(String name) {
        throw new UnsupportedOperationException();
    }

    protected Enumeration<URL> findResources(String name) {
        throw new UnsupportedOperationException();
    }

    protected Class<?> findSystemClass(String name) {
        throw new UnsupportedOperationException();
    }

    protected Object getClassLoadingLock(String className) {
        throw new UnsupportedOperationException();
    }

    protected Package getPackage(String name) {
        throw new UnsupportedOperationException();
    }

    protected Package[] getPackages() {
        throw new UnsupportedOperationException();
    }

    public ClassLoader getParent() {
        throw new UnsupportedOperationException();
    }

    public URL getResource(String name) {
        throw new UnsupportedOperationException();
    }

    public InputStream getResourceAsStream(String name) {
        throw new UnsupportedOperationException();
    }

    public Enumeration<URL> getResources(String name) {
        throw new UnsupportedOperationException();
    }

    public Class<?> loadClass(String name) {
        throw new UnsupportedOperationException();
    }

    protected Class<?> loadClass(String name, boolean resolve) {
        throw new UnsupportedOperationException();
    }

    protected void resolveClass(Class<?> c) {
        throw new UnsupportedOperationException();
    }

    public void setClassAssertionStatus(String className, boolean enabled) {
        throw new UnsupportedOperationException();
    }

    public void setDefaultAssertionStatus(boolean enabled) {
        throw new UnsupportedOperationException();
    }

    public void setPackageAssertionStatus(String packageName, boolean enabled) {
        throw new UnsupportedOperationException();
    }

    protected void setSigners(Class<?> c, Object[] signers) {
        throw new UnsupportedOperationException();
    }
}
