package java.util.concurrent.locks;

public class LockSupport {
    public static void unpark(Thread thread) {
        throw new UnsupportedOperationException();
    }

    public static void park() {
        throw new UnsupportedOperationException();
    }

    public static void park(Object blocker) {
        throw new UnsupportedOperationException();
    }

    public static void parkNanos(long nanos) {
        throw new UnsupportedOperationException();
    }

    public static void parkNanos(Object blocker, long nanos) {
        throw new UnsupportedOperationException();
    }

    public static void parkUntil(long deadline) {
        throw new UnsupportedOperationException();
    }

    public static void parkUntil(Object blocker, long deadline) {
        throw new UnsupportedOperationException();
    }

    public static void getBlocker(Thread thread) {
        throw new UnsupportedOperationException();
    }
}
