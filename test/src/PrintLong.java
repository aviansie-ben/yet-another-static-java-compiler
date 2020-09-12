public class PrintLong {
    private static long VARIANT_LONG = 1234567890L;
    private static final long INVARIANT_LONG = 1234567890L;

    public static void main(String[] args) {
        System.out.println(INVARIANT_LONG);
        System.out.println(VARIANT_LONG);
    }
}
