package sun.reflect;

import java.lang.reflect.*;

public class Reflection {
    public static native Class<?> getCallerClass();
    public static native Class<?> getCallerClass(int depth);
    public static native int getClassAccessFlags(Class<?> c);

    public static boolean quickCheckMemberAccess(Class<?> memberClass, int modifiers) {
        return Modifier.isPublic(getClassAccessFlags(memberClass) & modifiers);
    }

    public static void ensureMemberAccess(Class<?> currentClass, Class<?> memberClass, Object target, int modifiers) {
        // TODO
    }

    public static boolean verifyMemberAccess(Class<?> currentClass, Class<?> memberClass, Object target, int modifiers) {
        // TODO
        return true;
    }

    public static void registerMethodsToFilter(Class<?> clazz, String... methods) {}
    public static void registerFieldsToFilter(Class<?> clazz, String... methods) {}

    public static Field[] filterFields(Class<?> clazz, Field[] fields) {
        return fields;
    }

    public static Method[] filterMethods(Class<?> clazz, Method[] methods) {
        return methods;
    }

    public static boolean isCallerSensitive(Method m) {
        // TODO
        return false;
    }

    static boolean isSubclassOf(Class<?> queryClass, Class<?> ofClass) {
        while (queryClass != null && queryClass != ofClass) {
            queryClass = queryClass.getSuperclass();
        }

        return queryClass != null;
    }
}
