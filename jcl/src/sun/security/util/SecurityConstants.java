package sun.security.util;

import java.net.*;
import java.security.*;

public final class SecurityConstants {
    private SecurityConstants () {}

    public static final String FILE_DELETE_ACTION = null;
    public static final String FILE_EXECUTE_ACTION = null;
    public static final String FILE_READ_ACTION = null;
    public static final String FILE_WRITE_ACTION = null;
    public static final String FILE_READLINK_ACTION = null;
    public static final String SOCKET_RESOLVE_ACTION = null;
    public static final String SOCKET_CONNECT_ACTION = null;
    public static final String SOCKET_LISTEN_ACTION = null;
    public static final String SOCKET_ACCEPT_ACTION = null;
    public static final String SOCKET_CONNECT_ACCEPT_ACTION = null;
    public static final String PROPERTY_RW_ACTION = null;
    public static final String PROPERTY_READ_ACTION = null;
    public static final String PROPERTY_WRITE_ACTION = null;

    public static final AllPermission ALL_PERMISSION = null;
    public static final NetPermission SPECIFY_HANDLER_PERMISSION = null;
    public static final NetPermission SET_PROXYSELECTOR_PERMISSION = null;
    public static final NetPermission GET_PROXYSELECTOR_PERMISSION = null;
    public static final NetPermission SET_COOKIEHANDLER_PERMISSION = null;
    public static final NetPermission GET_COOKIEHANDLER_PERMISSION = null;
    public static final NetPermission SET_RESPONSECACHE_PERMISSION = null;
    public static final NetPermission GET_RESPONSECACHE_PERMISSION = null;
    public static final NetPermission SET_SOCKETIMPL_PERMISSION = null;
    public static final RuntimePermission CREATE_CLASSLOADER_PERMISSION = null;
    public static final RuntimePermission CHECK_MEMBER_ACCESS_PERMISSION = null;
    public static final RuntimePermission MODIFY_THREAD_PERMISSION = null;
    public static final RuntimePermission MODIFY_THREADGROUP_PERMISSION = null;
    public static final RuntimePermission GET_PD_PERMISSION = null;
    public static final RuntimePermission GET_CLASSLOADER_PERMISSION = null;
    public static final RuntimePermission STOP_THREAD_PERMISSION = null;
    public static final RuntimePermission GET_STACK_TRACE_PERMISSION = null;
    public static final SecurityPermission CREATE_ACC_PERMISSION = null;
    public static final SecurityPermission GET_COMBINER_PERMISSION = null;
    public static final SecurityPermission GET_POLICY_PERMISSION = null;
    public static final SocketPermission LOCAL_LISTEN_PERMISSION = null;

    public static class AWT {
        private AWT() {}

        public static final Permission TOPLEVEL_WINDOW_PERMISSION = null;
        public static final Permission ACCESS_CLIPBOARD_PERMISSION = null;
        public static final Permission CHECK_AWT_EVENTQUEUE_PERMISSION = null;
        public static final Permission TOOLKIT_MODALITY_PERMISSION = null;
        public static final Permission READ_DISPLAY_PIXELS_PERMISSION = null;
        public static final Permission CREATE_ROBOT_PERMISSION = null;
        public static final Permission WATCH_MOUSE_PERMISSION = null;
        public static final Permission SET_WINDOW_ALWAYS_ON_TOP_PERMISSION = null;
        public static final Permission ALL_AWT_EVENTS_PERMISSION = null;
        public static final Permission ACCESS_SYSTEM_TRAY_PERMISSION = null;
    }
}
