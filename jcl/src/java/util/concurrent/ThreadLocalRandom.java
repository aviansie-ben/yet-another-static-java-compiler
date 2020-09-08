package java.util.concurrent;

import java.util.Random;

public class ThreadLocalRandom extends Random {
    private static final ThreadLocalRandom instance = new ThreadLocalRandom();

    private ThreadLocalRandom() {
        super(0);
    }

    public static ThreadLocalRandom current() {
        return instance;
    }

    static void localInit() {
        throw new UnsupportedOperationException();
    }

    static int nextSecondarySeed() {
        throw new UnsupportedOperationException();
    }

    static int getProbe() {
        throw new UnsupportedOperationException();
    }

    static int advanceProbe(int i) {
        throw new UnsupportedOperationException();
    }
}
