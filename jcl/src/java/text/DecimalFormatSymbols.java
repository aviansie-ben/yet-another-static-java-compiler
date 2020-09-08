package java.text;

import java.util.Currency;
import java.util.Locale;

public class DecimalFormatSymbols {
    private char zeroDigit = '0';
    private char groupingSeparator = ',';
    private char decimalSeparator = '.';
    private char perMill = '\u2030';
    private char percent = '%';
    private char digit = '#';
    private char patternSeparator = ';';
    private String infinity = "\u221e";
    private String NaN = "NaN";
    private char minusSign = '-';
    private String currencySymbol = "$";
    private String internationalCurrencySymbol = "USD";
    private char monetaryDecimalSeparator = '.';
    private String exponentSeparator = "E";

    public DecimalFormatSymbols() {}
    public DecimalFormatSymbols(Locale locale) {}

    public char getZeroDigit() {
        return zeroDigit;
    }

    public void setZeroDigit(char zeroDigit) {
        this.zeroDigit = zeroDigit;
    }

    public char getGroupingSeparator() {
        return groupingSeparator;
    }

    public void setGroupingSeparator(char groupingSeparator) {
        this.groupingSeparator = groupingSeparator;
    }

    public char getDecimalSeparator() {
        return decimalSeparator;
    }

    public void setDecimalSeparator(char decimalSeparator) {
        this.decimalSeparator = decimalSeparator;
    }

    public char getPerMill() {
        return perMill;
    }

    public void setPerMill(char perMill) {
        this.perMill = perMill;
    }

    public char getPercent() {
        return percent;
    }

    public void setPercent(char percent) {
        this.percent = percent;
    }

    public char getDigit() {
        return digit;
    }

    public void setDigit(char digit) {
        this.digit = digit;
    }

    public char getPatternSeparator() {
        return patternSeparator;
    }

    public void setPatternSeparator(char patternSeparator) {
        this.patternSeparator = patternSeparator;
    }

    public String getInfinity() {
        return infinity;
    }

    public void setInfinity(String infinity) {
        this.infinity = infinity;
    }

    public String getNaN() {
        return NaN;
    }

    public void setNaN(String NaN) {
        this.NaN = NaN;
    }

    public char getMinusSign() {
        return minusSign;
    }

    public void setMinusSign(char minusSign) {
        this.minusSign = minusSign;
    }

    public String getCurrencySymbol() {
        return currencySymbol;
    }

    public void setCurrencySymbol(String currencySymbol) {
        this.currencySymbol = currencySymbol;
    }

    public String getInternationalCurrencySymbol() {
        return internationalCurrencySymbol;
    }

    public void setInternationalCurrencySymbol(String internationalCurrencySymbol) {
        this.internationalCurrencySymbol = internationalCurrencySymbol;
    }

    public char getMonetaryDecimalSeparator() {
        return monetaryDecimalSeparator;
    }

    public void setMonetaryDecimalSeparator(char monetaryDecimalSeparator) {
        this.monetaryDecimalSeparator = monetaryDecimalSeparator;
    }

    public String getExponentSeparator() {
        return exponentSeparator;
    }

    public void setExponentSeparator(String exponentSeparator) {
        this.exponentSeparator = exponentSeparator;
    }

    public Currency getCurrency() {
        throw new UnsupportedOperationException();
    }

    public void setCurrency(Currency currency) {
        currencySymbol = currency.getSymbol();
        internationalCurrencySymbol = currency.getCurrencyCode();
    }

    public static Locale[] getAvailableLocales() {
        return new Locale[] { Locale.ENGLISH };
    }

    public static DecimalFormatSymbols getInstance() {
        return new DecimalFormatSymbols();
    }

    public static DecimalFormatSymbols getInstance(Locale locale) {
        return new DecimalFormatSymbols();
    }
}
