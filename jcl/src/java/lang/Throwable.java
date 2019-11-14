package java.lang;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Serializable;

public class Throwable implements Serializable {
    private StackTraceElement[] stackTrace;
    private String detailMessage;
    private Throwable cause;

    public Throwable() {
        fillInStackTrace();
    }

    public Throwable(String detailMessage, Throwable cause) {
        this();
        this.detailMessage = detailMessage;
        this.cause = cause;
    }

    public Throwable(String detailMessage) {
        this(detailMessage, null);
    }

    public Throwable(Throwable cause) {
        this(cause != null ? cause.toString() : null, cause);
    }

    protected Throwable(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        this(message, cause);
    }

    public void addSuppressed(Throwable exception) {
        throw new UnsupportedOperationException();
    }

    public Throwable fillInStackTrace() {
        return this;
    }

    public Throwable getCause() {
        return cause;
    }

    public String getLocalizedMessage() {
        return detailMessage;
    }

    public String getMessage() {
        return detailMessage;
    }

    public StackTraceElement[] getStackTrace() {
        return stackTrace.clone();
    }

    int getStackTraceDepth() {
        return 0; // TODO
    }

    StackTraceElement getStackTraceElement(int i) {
        return null; // TODO
    }

    public Throwable[] getSuppressed() {
        return new Throwable[0];
    }

    public Throwable initCause(Throwable cause) {
        this.cause = cause;
        return this;
    }

    public void printStackTrace() {
        printStackTrace(System.err);
    }

    public void printStackTrace(PrintStream s) {
        // TODO
    }

    public void printStackTrace(PrintWriter s) {
        // TODO
    }

    public void setStackTrace(StackTraceElement[] stackTrace) {
        this.stackTrace = stackTrace;
    }

    public String toString() {
        String message = getLocalizedMessage();

        if (message != null) {
            return getClass().getName() + ": " + message;
        } else {
            return getClass().getName();
        }
    }
}
