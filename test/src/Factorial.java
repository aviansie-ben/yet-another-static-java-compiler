import org.mocha.test.Output;

public class Factorial {
    private static int factorial(int n) {
        int result = 1;

        while (n > 0) {
            result *= n--;
        }

        return result;
    }

    private static int fac0 = factorial(0);
    private static int fac5 = factorial(5);
    private static int fac10 = factorial(10);

    public static void main(String[] args) {
        Output.writeString("## COMPILE TIME ##\n0! = ");
        Output.writeInt(fac0);
        Output.writeString("\n5! = ");
        Output.writeInt(fac5);
        Output.writeString("\n10! = ");
        Output.writeInt(fac10);

        Output.writeString("\n\n## RUNTIME ##\n0! = ");
        Output.writeInt(factorial(0));
        Output.writeString("\n5! = ");
        Output.writeInt(factorial(5));
        Output.writeString("\n10! = ");
        Output.writeInt(factorial(10));
        Output.writeString("\n\n");
    }
}
