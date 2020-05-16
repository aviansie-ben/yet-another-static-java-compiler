import org.mocha.test.Output;

public class InterfaceCall {
    private interface TestInterface {
        int a();
    }

    private static class ImplA implements TestInterface {
        public int a() {
            Output.writeString("Hello from ImplA.a()\n");
            return -1;
        }
    }

    private static class ImplB implements TestInterface {
        private int x;

        public ImplB(int x) {
            this.x = x;
        }

        public int a() {
            Output.writeString("Hello from ImplB.a()\n");
            return x;
        }
    }

    private static void test(TestInterface obj) {
        int a = obj.a();

        Output.writeString("obj.a() = ");
        Output.writeInt(a);
        Output.writeString("\n");
    }

    public static void main(String[] args) {
        test(new ImplA());
        test(new ImplB(1));
        test(new ImplB(666));
    }
}
