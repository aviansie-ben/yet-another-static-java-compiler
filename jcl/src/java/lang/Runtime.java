package java.lang;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class Runtime {
    private static final Runtime instance = new Runtime();

    public static Runtime getRuntime() {
        return instance;
    }

    private Runtime() {}

    public void exit(int status) {
        throw new UnsupportedOperationException();
    }

    public void addShutdownHook(Thread hook) {
        throw new UnsupportedOperationException();
    }

    public void removeShutdownHook(Thread hook) {
        throw new UnsupportedOperationException();
    }

    public void halt(int status) {
        throw new UnsupportedOperationException();
    }

    public void runFinalizersOnExit(boolean value) {
        throw new UnsupportedOperationException();
    }

    public Process exec(String command) throws IOException {
        throw new UnsupportedOperationException();
    }

    public Process exec(String command, String[] envp) throws IOException {
        throw new UnsupportedOperationException();
    }

    public Process exec(String command, String[] envp, File dir) throws IOException {
        throw new UnsupportedOperationException();
    }

    public Process exec(String[] cmdarray) throws IOException {
        throw new UnsupportedOperationException();
    }

    public Process exec(String[] cmdarray, String[] envp) throws IOException {
        throw new UnsupportedOperationException();
    }

    public Process exec(String[] cmdarray, String[] envp, File dir) throws IOException {
        throw new UnsupportedOperationException();
    }

    public int availableProcessors() {
        return 1;
    }

    public long freeMemory() {
        throw new UnsupportedOperationException();
    }

    public long totalMemory() {
        throw new UnsupportedOperationException();
    }

    public long maxMemory() {
        throw new UnsupportedOperationException();
    }

    public void gc() {}

    public void runFinalization() {
        throw new UnsupportedOperationException();
    }

    public void traceInstructions(boolean on) {
        throw new UnsupportedOperationException();
    }

    public void traceMethodCalls(boolean on) {
        throw new UnsupportedOperationException();
    }

    public void load(String filename) {
        throw new UnsupportedOperationException();
    }

    public void loadLibrary(String libname) {
        throw new UnsupportedOperationException();
    }

    public InputStream getLocalizedInputStream(InputStream in) {
        throw new UnsupportedOperationException();
    }

    public OutputStream getLocalizedOutputStream(OutputStream out) {
        throw new UnsupportedOperationException();
    }
}
