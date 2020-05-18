package java.lang;

import java.util.Map;

public class Thread {
    public static final int MAX_PRIORITY = 1;
    public static final int MIN_PRIORITY = 1;
    public static final int NORM_PRIORITY = 1;

    public static int activeCount() {
        return 1;
    }

    public static Thread currentThread() {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public static void dumpStack() {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public static int enumerate(Thread[] tarray) {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public static Map<Thread, StackTraceElement[]> getAllStackTraces() {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public static UncaughtExceptionHandler getDefaultUncaughtExceptionHandler() {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public static boolean holdsLock(Object obj) {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public static boolean interrupted() {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public static void setDefaultUncaughtExceptionHandler(UncaughtExceptionHandler eh) {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public static void sleep(long millis) {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public static void sleep(long millis, int nanos) {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public static void yield() {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    ThreadLocal.ThreadLocalMap threadLocals;
    ThreadLocal.ThreadLocalMap inheritableThreadLocals;

    public Thread() {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public Thread(Runnable target) {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public Thread(Runnable target, String name) {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public Thread(String name) {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public Thread(ThreadGroup group, Runnable target) {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public Thread(ThreadGroup group, Runnable target, String name) {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public Thread(ThreadGroup group, Runnable target, String name, long stackSize) {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public Thread(ThreadGroup group, String name) {
        throw new UnsupportedOperationException("Mocha does not currently support multithreading");
    }

    public void checkAccess() {}

    public int countStackFrames() {
        throw new UnsupportedOperationException();
    }

    public void destroy() {
        throw new UnsupportedOperationException();
    }

    public ClassLoader getContextClassLoader() {
        throw new UnsupportedOperationException();
    }

    public long getId() {
        throw new UnsupportedOperationException();
    }

    public String getName() {
        throw new UnsupportedOperationException();
    }

    public int getPriority() {
        throw new UnsupportedOperationException();
    }

    public StackTraceElement[] getStackTrace() {
        throw new UnsupportedOperationException();
    }

    public State getState() {
        throw new UnsupportedOperationException();
    }

    public ThreadGroup getThreadGroup() {
        throw new UnsupportedOperationException();
    }

    public UncaughtExceptionHandler getUncaughtExceptionHandler() {
        throw new UnsupportedOperationException();
    }

    public void interrupt() {
        throw new UnsupportedOperationException();
    }

    public boolean isAlive() {
        throw new UnsupportedOperationException();
    }

    public boolean isDaemon() {
        throw new UnsupportedOperationException();
    }

    public boolean isInterrupted() {
        throw new UnsupportedOperationException();
    }

    public void join() {
        throw new UnsupportedOperationException();
    }

    public void join(long millis) {
        throw new UnsupportedOperationException();
    }

    public void join(long millis, int nanos) {
        throw new UnsupportedOperationException();
    }

    public void resume() {
        throw new UnsupportedOperationException();
    }

    public void run() {
        throw new UnsupportedOperationException();
    }

    public void setContextClassLoader(ClassLoader classLoader) {
        throw new UnsupportedOperationException();
    }

    public void setDaemon(boolean on) {
        throw new UnsupportedOperationException();
    }

    public void setName(String name) {
        throw new UnsupportedOperationException();
    }

    public void setPriority(int newPriority) {
        throw new UnsupportedOperationException();
    }

    public void setUncaughtExceptionHandler(UncaughtExceptionHandler eh) {
        throw new UnsupportedOperationException();
    }

    public void start() {
        throw new UnsupportedOperationException();
    }

    public void stop() {
        throw new UnsupportedOperationException();
    }

    public void stop(Throwable obj) {
        throw new UnsupportedOperationException();
    }

    public void suspend() {
        throw new UnsupportedOperationException();
    }

    public enum State {
        NEW, RUNNABLE, BLOCKED, WAITING, TIMED_WAITING, TERMINATED
    }

    public interface UncaughtExceptionHandler {
        void uncaughtException(Thread t, Throwable e);
    }
}
