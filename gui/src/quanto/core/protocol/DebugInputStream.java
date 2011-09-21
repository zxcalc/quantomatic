/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import java.io.IOException;
import java.io.InputStream;

/**
 *
 * @author alemer
 */
class DebugInputStream extends InputStream
{
    private boolean debuggingActive = false;
    private InputStream internal;

    public DebugInputStream(InputStream internal) {
        this.internal = internal;
    }

    public void setDebuggingActive(boolean debuggingActive) {
        this.debuggingActive = debuggingActive;
    }

    public boolean isDebuggingActive() {
        return debuggingActive;
    }

    private void dbgOutputChar(int ch) {
        if (ch == '\u001b') {
            System.out.append('\u00a4');
        } else {
            System.out.append((char)ch);
        }
    }

    @Override
    public int read() throws IOException {
        int ch = internal.read();
        if (ch != -1 && debuggingActive)
            dbgOutputChar(ch);
        return ch;
    }

    @Override
    public int available() throws IOException {
        return internal.available();
    }

    @Override
    public void close() throws IOException {
        internal.close();
    }

    @Override
    public long skip(long n) throws IOException {
        // we don't log skipped chars
        // (not that we expect anything to be skipped)
        return internal.skip(n);
    }

    @Override
    public int read(byte[] b) throws IOException {
        int count = internal.read(b);
        if (debuggingActive) {
            for (int i = 0; i < count; ++i) {
                dbgOutputChar(b[i]);
            }
        }
        return count;
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        int count = internal.read(b, off, len);
        if (debuggingActive) {
            for (int i = off; i < (off + count); ++i) {
                dbgOutputChar(b[i]);
            }
        }
        return count;
    }
}
