package java.security;

public final class AccessController {
    private AccessController() {}

    public static void checkPermission(Permission p) {}

    public static <T> T doPrivileged(PrivilegedAction<T> action) {
        return action.run();
    }

    public static <T> T doPrivileged(PrivilegedAction<T> action, AccessControlContext context) {
        return doPrivileged(action);
    }

    public static <T> T doPrivileged(PrivilegedAction<T> action, AccessControlContext context, Permission... perms) {
        return doPrivileged(action);
    }

    public static <T> T doPrivileged(PrivilegedExceptionAction<T> action) throws PrivilegedActionException {
        try {
            return action.run();
        } catch (Error e) {
            throw e;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new PrivilegedActionException(e);
        }
    }

    public static <T> T doPrivileged(PrivilegedExceptionAction<T> action, AccessControlContext context) throws PrivilegedActionException {
        return doPrivileged(action);
    }

    public static <T> T doPrivileged(PrivilegedExceptionAction<T> action, AccessControlContext context, Permission... perms) throws PrivilegedActionException {
        return doPrivileged(action);
    }

    public static <T> T doPrivilegedWithCombiner(PrivilegedAction<T> action) {
        return doPrivileged(action);
    }

    public static <T> T doPrivilegedWithCombiner(PrivilegedAction<T> action, AccessControlContext context, Permission... perms) {
        return doPrivileged(action);
    }

    public static <T> T doPrivilegedWithCombiner(PrivilegedExceptionAction<T> action) throws PrivilegedActionException {
        return doPrivileged(action);
    }

    public static <T> T doPrivilegedWithCombiner(PrivilegedExceptionAction<T> action, AccessControlContext context, Permission... perms) throws PrivilegedActionException {
        return doPrivileged(action);
    }

    public static AccessControlContext getContext() {
        return new AccessControlContext(null, null, true);
    }

    static AccessControlContext getInheritedAccessControlContext() {
        return null;
    }
}
