/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import java.io.IOException;
import java.io.OutputStream;

/**
 *
 * @author alemer
 */
public class DebugOutputStream extends OutputStream
{
    private boolean debuggingActive = false;
    private OutputStream internal;

    public DebugOutputStream(OutputStream internal) {
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
    public void write(int b) throws IOException {
        if (debuggingActive)
            dbgOutputChar(b);
        internal.write(b);
    }

    @Override
    public void write(byte[] b) throws IOException {
        if (debuggingActive) {
            for (int i = 0; i < b.length; ++i) {
                dbgOutputChar(b[i]);
            }
        }
        internal.write(b);
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        if (debuggingActive) {
            for (int i = off; i < (off + len); ++i) {
                dbgOutputChar(b[i]);
            }
        }
        internal.write(b, off, len);
    }

    @Override
    public void flush() throws IOException {
        internal.flush();
    }

    @Override
    public void close() throws IOException {
        internal.close();
    }
}
