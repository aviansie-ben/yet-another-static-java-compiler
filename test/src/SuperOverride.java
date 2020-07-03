import org.mocha.test.Output;

public class SuperOverride {
    public interface I {
        void a();
    }

    public static class A {
        public void a() {
            Output.writeString("A.a() called\n");
        }
    }

    public static class B extends A implements I {}

    public static void main(String[] args) {
        ((I)new B()).a();
    }
}
