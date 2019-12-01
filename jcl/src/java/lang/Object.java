package java.lang;

public class Object {
    public Object() {}

    protected void finalize() {}

    protected Object clone() throws CloneNotSupportedException {
        throw new UnsupportedOperationException();
    }

    public boolean equals(Object o) {
        return this == o;
    }

    public native final Class<?> getClass();

    public int hashCode() {
        throw new UnsupportedOperationException();
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
