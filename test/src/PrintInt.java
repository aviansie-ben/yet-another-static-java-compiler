public class PrintInt {
    private static int VARIANT_INT = 1234;
    private static final int INVARIANT_INT = 1234;

    public static void main(String[] args) {
        System.out.println(INVARIANT_INT);
        System.out.println(VARIANT_INT);
    }
}
