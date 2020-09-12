import java.text.DecimalFormat;

public class FormatFloat {
    private static final DecimalFormat FORMAT = new DecimalFormat("0.0000");

    private static final float INVARIANT_FLOAT = 999.125f;
    private static float VARIANT_FLOAT = 999.125f;

    public static void main(String[] args) {
        System.out.println(FORMAT.format(INVARIANT_FLOAT));
        System.out.println(FORMAT.format(VARIANT_FLOAT));
    }
}
