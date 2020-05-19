package java.lang;

import java.util.Arrays;

public class Object {
    public Object() {}

    protected void finalize() {}

    protected Object clone() throws CloneNotSupportedException {
        Class<?> clazz = getClass();

        if (clazz.isArray()) {
            if (this instanceof boolean[]) {
                boolean[] self = (boolean[])this;
                return Arrays.copyOf(self, self.length);
            } else if (this instanceof byte[]) {
                byte[] self = (byte[])this;
                return Arrays.copyOf(self, self.length);
            } else if (this instanceof char[]) {
                char[] self = (char[])this;
                return Arrays.copyOf(self, self.length);
            } else if (this instanceof double[]) {
                double[] self = (double[])this;
                return Arrays.copyOf(self, self.length);
            } else if (this instanceof float[]) {
                float[] self = (float[])this;
                return Arrays.copyOf(self, self.length);
            } else if (this instanceof int[]) {
                int[] self = (int[])this;
                return Arrays.copyOf(self, self.length);
            } else if (this instanceof long[]) {
                long[] self = (long[])this;
                return Arrays.copyOf(self, self.length);
            } else if (this instanceof short[]) {
                short[] self = (short[])this;
                return Arrays.copyOf(self, self.length);
            } else {
                Object[] self = (Object[])this;
                return Arrays.copyOf(self, self.length);
            }
        } else {
            throw new UnsupportedOperationException();
        }
    }

    public boolean equals(Object o) {
        return this == o;
    }

    public native final Class<?> getClass();

    public int hashCode() {
        return System.identityHashCode(this);
    }

    public void notify() {
        throw new UnsupportedOperationException();
    }

    public void notifyAll() {
        throw new UnsupportedOperationException();
    }

    public String toString() {
        throw new UnsupportedOperationException();
    }

    public void wait() throws InterruptedException {
        throw new UnsupportedOperationException();
    }

    public void wait(long time) throws InterruptedException {
        throw new UnsupportedOperationException();
    }

    public void wait(long time, int frac) throws InterruptedException {
        throw new UnsupportedOperationException();
    }
}
