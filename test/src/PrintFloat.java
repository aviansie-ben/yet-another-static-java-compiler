public class PrintFloat {
    private static final float INVARIANT_FLOAT = 999.125f;
    private static float VARIANT_FLOAT = 999.125f;

    public static void main(String[] args) {
        System.out.println(INVARIANT_FLOAT);
        System.out.println(VARIANT_FLOAT);
    }
}
