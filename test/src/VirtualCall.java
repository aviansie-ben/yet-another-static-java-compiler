import org.mocha.test.Output;

public class VirtualCall {
    private static class A {
        public int a() {
            Output.writeString("Hello from A.a()\n");
            return -1;
        }
    }

    private static class B extends A {
        private int x;

        public B(int x) {
            this.x = x;
        }

        public int a() {
            Output.writeString("Hello from B.a()\n");
            return x;
        }
    }

    private static void test(A obj) {
        int a = obj.a();

        Output.writeString("obj.a() = ");
        Output.writeInt(a);
        Output.writeString("\n");
    }

    public static void main(String[] args) {
        test(new A());
        test(new B(1));
        test(new B(666));
    }
}
