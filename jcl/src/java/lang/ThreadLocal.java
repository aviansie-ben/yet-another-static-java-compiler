package java.lang;

import java.util.function.Supplier;

public class ThreadLocal<T> {
    private boolean init;
    private T value;

    public ThreadLocal() {}

    public T get() {
        if (!init) {
            value = initialValue();
            init = true;
        }

        return value;
    }

    protected T initialValue() {
        return null;
    }

    public void remove() {
        init = false;
        value = null;
    }

    public void set(T value) {
        init = true;
        this.value = value;
    }

    public static <S> ThreadLocal<S> withInitial(Supplier<? extends S> supplier) {
        return new SupplierThreadLocal<S>(supplier);
    }

    static ThreadLocalMap createInheritedMap(ThreadLocalMap map) {
        return new ThreadLocalMap();
    }

    private static class SupplierThreadLocal<T> extends ThreadLocal<T> {
        private final Supplier<? extends T> supplier;

        public SupplierThreadLocal(Supplier<? extends T> supplier) {
            this.supplier = supplier;
        }

        protected T initialValue() {
            return supplier.get();
        }
    }

    static class ThreadLocalMap {
        ThreadLocalMap() {}
        ThreadLocalMap(ThreadLocal<?> key, Object value) {}
    }
}
