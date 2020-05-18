package java.lang.ref;

public abstract class Reference<T> {
    private T obj;
    volatile ReferenceQueue<? super T> queue;
    volatile Reference<?> next;

    public T get() {
        return obj;
    }

    public void clear() {
        obj = null;
    }

    public boolean isEnqueued() {
        return false;
    }

    public boolean enqueue() {
        return false;
    }

    Reference(T obj) {
        this(obj, null);
    }

    Reference(T obj, ReferenceQueue<? super T> queue) {
        this.obj = obj;
        this.queue = queue;
    }
}
