package java.lang;

import java.io.*;
import java.nio.channels.*;
import java.nio.channels.spi.*;
import java.util.*;

public class System {
    public static final InputStream in = null /*new FileInputStream(FileDescriptor.in)*/;
    public static final PrintStream out = null /*new PrintStream(new FileOutputStream(FileDescriptor.out))*/;
    public static final PrintStream err = null /*new PrintStream(new FileOutputStream(FileDescriptor.err))*/;

    private static final Properties props = null /*new Properties()*/;

    public static void setIn(InputStream in) {
        throw new UnsupportedOperationException("Mocha does not support System.setIn");
    }

    public static void setOut(PrintStream out) {
        throw new UnsupportedOperationException("Mocha does not support System.setOut");
    }

    public static void setErr(PrintStream err) {
        throw new UnsupportedOperationException("Mocha does not support System.setErr");
    }

    public static Console console() {
        throw new UnsupportedOperationException("Mocha does not support System.console");
    }

    public static Channel inheritedChannel() throws IOException {
        return SelectorProvider.provider().inheritedChannel();
    }

    public static void setSecurityManager(SecurityManager securityManager) {
        throw new UnsupportedOperationException("Mocha does not support SecurityManager");
    }

    public static SecurityManager getSecurityManager() {
        return null;
    }

    public static native long currentTimeMillis();
    public static native long nanoTime();

    public static native void arraycopy(Object src, int srcPos, Object dst, int dstPos, int len);
    public static native int identityHashCode(Object o);

    public static Properties getProperties() {
        return props;
    }

    public static void setProperties(Properties props) {
        throw new UnsupportedOperationException("Mocha does not support System.setProperties");
    }

    public static String getProperty(String prop) {
        return props.getProperty(prop);
    }

    public static String getProperty(String prop, String defaultValue) {
        return props.getProperty(prop, defaultValue);
    }

    public static String setProperty(String prop, String val) {
        return (String)props.setProperty(prop, val);
    }

    public static String clearProperty(String prop) {
        return (String)props.remove(prop);
    }

    public static String lineSeparator() {
        return "\n";
    }

    public static String getenv(String envVar) {
        return ProcessEnvironment.getenv(envVar);
    }

    public static Map<String, String> getenv() {
        return ProcessEnvironment.getenv();
    }

    public static void exit(int code) {
        Runtime.getRuntime().exit(code);
    }

    public static void gc() {}
    public static void runFinalization() {}
    public static void runFinalizersOnExit(boolean value) {}

    public static native void load(String library);
    public static native void loadLibrary(String library);
    public static native String mapLibraryName(String library);

    static {}
}