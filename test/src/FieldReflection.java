import org.mocha.test.Output;

import java.lang.reflect.Field;

public class FieldReflection {
    private static class Test {
        public static final Field A_FIELD;
        public static final Field B_FIELD;
        public static final Field C_FIELD;

        static {
            try {
                A_FIELD = Test.class.getDeclaredField("a");
                B_FIELD = Test.class.getDeclaredField("b");
                C_FIELD = Test.class.getDeclaredField("c");
            } catch (NoSuchFieldException e) {
                throw new AssertionError(e);
            }
        }

        public static int a;

        public int b;
        public long c;
    }

    private static final Test t = new Test();

    static {
        try {
            Test.A_FIELD.setInt(null, 999);
            Test.B_FIELD.setInt(t, 123);
            Test.C_FIELD.setLong(t, 81985529216486895L);
        } catch (IllegalAccessException e) {
            throw new AssertionError(e);
        }
    }

    public static void main(String[] args) {
        try {
            Output.writeString("== AFTER CLINIT REFLECTION ==\n");
            Output.writeString("Test.a = ");
            Output.writeInt(Test.a);
            Output.writeString(" (");
            Output.writeInt(Test.A_FIELD.getInt(null));
            Output.writeString(")\nt.b = ");
            Output.writeInt(t.b);
            Output.writeString(" (");
            Output.writeInt(Test.B_FIELD.getInt(t));
            Output.writeString(")\nt.c = ");
            Output.writeLong(t.c);
            Output.writeString(" (");
            Output.writeLong(Test.C_FIELD.getLong(t));
            Output.writeString(")\n");

            Test.A_FIELD.setInt(null, 666);
            Test.B_FIELD.setInt(t, 321);
            Test.C_FIELD.setLong(t, -81985529216486895L);

            Output.writeString("== AFTER RUNTIME REFLECTION ==\n");
            Output.writeString("Test.a = ");
            Output.writeInt(Test.a);
            Output.writeString(" (");
            Output.writeInt(Test.A_FIELD.getInt(null));
            Output.writeString(")\nt.b = ");
            Output.writeInt(t.b);
            Output.writeString(" (");
            Output.writeInt(Test.B_FIELD.getInt(t));
            Output.writeString(")\nt.c = ");
            Output.writeLong(t.c);
            Output.writeString(" (");
            Output.writeLong(Test.C_FIELD.getLong(t));
            Output.writeString(")\n");
        } catch (IllegalAccessException e) {
            throw new AssertionError(e);
        }
    }
}
