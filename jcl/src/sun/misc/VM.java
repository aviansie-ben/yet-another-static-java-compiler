package sun.misc;

import java.util.Properties;

public class VM {
    public static final int STATE_GREEN = 1;
    public static final int STATE_YELLOW = 2;
    public static final int STATE_RED = 3;

    public static boolean threadsSuspended() {
        return false;
    }

    public static boolean allowThreadSuspension(ThreadGroup g, boolean b) {
        return false;
    }

    public static boolean suspendThreads() {
        return false;
    }

    public static void unsuspendThreads() {}
    public static void unsuspendSomeThreads() {}

    public static int getState() {
        return STATE_GREEN;
    }

    @Deprecated
    public static void registerVMNotification(VMNotification n) {}

    public static void asChange(int as_old, int as_new) {}
    public static void asChange_otherthread(int as_old, int as_new) {}

    public static void booted() {}
    public static boolean isBooted() {
        return true;
    }
    public static void awaitBooted() throws InterruptedException {}

    public static long maxDirectMemory() {
        return Long.MAX_VALUE;
    }

    public static boolean isDirectMemoryPageAligned() {
        return false;
    }

    public static boolean allowArraySyntax() {
        return false;
    }

    public static boolean isSystemDomainLoader(ClassLoader loader) {
        return true;
    }

    public static String getSavedProperty(String key) {
        return null;
    }

    public static void saveAndRemoveProperties(Properties props) {
        throw new UnsupportedOperationException();
    }

    public static void initializeOSEnvironment() {}

    public static int getFinalRefCount() {
        return 0;
    }

    public static int getPeakFinalRefCount() {
        return 0;
    }

    public static void addFinalRefCount(int n) {
        throw new UnsupportedOperationException();
    }

    public static Thread.State toThreadState(int threadStatus) {
        // TODO
        return Thread.State.RUNNABLE;
    }

    public static ClassLoader latestUserDefinedLoader() {
        return null;
    }
}
