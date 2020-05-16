import org.mocha.test.Output;

public class InterfaceCallNoImplementer {
    public interface TestInterface {
        int a();
    }

    private static void test(TestInterface obj) {
        obj.a();
    }

    public static void main(String[] args) {
        try {
            test(null);
            Output.writeString("FAILURE: Expected NPE");
        } catch (NullPointerException e) {
            Output.writeString("SUCCESS\n");
        }
    }
}
