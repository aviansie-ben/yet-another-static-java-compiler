package java.lang;

import java.io.FileDescriptor;
import java.net.InetAddress;
import java.security.Permission;

public class SecurityManager {
    protected boolean inCheck = false;

    public SecurityManager() {
        throw new UnsupportedOperationException("Mocha does not support SecurityManager");
    }

    public void checkAccept(String host, int port) {}
    public void checkAccess(Thread t) {}
    public void checkAccess(ThreadGroup g) {}
    public void checkAwtEventQueueAccess() {}
    public void checkConnect(String host, int port) {}
    public void checkConnect(String host, int port, Object context) {}
    public void checkCreateClassLoader() {}
    public void checkDelete(String file) {}
    public void checkExec(String cmd) {}
    public void checkExit(int status) {}
    public void checkLink(String lib) {}
    public void checkListen(int port) {}
    public void checkMemberAccess(Class<?> clazz, int which) {}
    public void checkMulticast(InetAddress maddr) {}
    public void checkMulticast(InetAddress maddr, byte ttl) {}
    public void checkPackageAccess(String pkg) {}
    public void checkPackageDefinition(String pkg) {}
    public void checkPermission(Permission perm) {}
    public void checkPermission(Permission perm, Object ctx) {}
    public void checkPrintJobAccess() {}
    public void checkPropertiesAccess() {}
    public void checkPropertyAccess(String key) {}
    public void checkRead(FileDescriptor fd) {}
    public void checkRead(String file) {}
    public void checkRead(String file, Object ctx) {}
    public void checkSecurityAccess(String target) {}
    public void checkSetFactory() {}
    public void checkSystemClipboardAccess() {}
    public void checkTopLevelWindow(Object window) {}
    public void checkWrite(FileDescriptor fd) {}
    public void checkWrite(String file) {}

    protected int classDepth(String name) {
        throw new UnsupportedOperationException();
    }

    protected int classLoaderDepth() {
        throw new UnsupportedOperationException();
    }

    protected ClassLoader currentClassLoader() {
        throw new UnsupportedOperationException();
    }

    protected Class<?> currentLoadedClass() {
        throw new UnsupportedOperationException();
    }

    protected Class[] getClassContext() {
        throw new UnsupportedOperationException();
    }

    public boolean getInCheck() {
        return false;
    }

    public Object getSecurityContext() {
        throw new UnsupportedOperationException();
    }

    public ThreadGroup getThreadGroup() {
        return Thread.currentThread().getThreadGroup();
    }

    protected boolean inClass(String name) {
        throw new UnsupportedOperationException();
    }

    protected boolean inClassLoader() {
        throw new UnsupportedOperationException();
    }
}
