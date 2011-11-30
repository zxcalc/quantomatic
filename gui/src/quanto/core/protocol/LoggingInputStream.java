/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import java.io.CharArrayWriter;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author alemer
 */
class LoggingInputStream extends FilterInputStream
{
    @SuppressWarnings("NonConstantLogger")
    private final Logger logger;
    private final CharArrayWriter logStream = new CharArrayWriter(256);

    public LoggingInputStream(InputStream internal, String logArea) {
        super(internal);
        logger = Logger.getLogger(logArea);
    }

    public void writeLog(Level level) {
        if (logger.isLoggable(level)) {
            logger.log(level, "{0}",
                    logStream.toString().replace('\u001b', '\u00a4'));
        }
        logStream.reset();
    }

    public void writeLog(Level level, String message) {
        if (logger.isLoggable(level)) {
            logger.log(level, "{0}: {1}",
                    new Object[] {
                        message,
                        logStream.toString().replace('\u001b', '\u00a4')
                    });
        }
        logStream.reset();
    }

    @Override
    public int read() throws IOException {
        int ch = in.read();
        if (ch != -1)
            logStream.append((char)ch);
        return ch;
    }

    @Override
    public int read(byte[] b) throws IOException {
        int count = in.read(b);
        for (int i = 0; i < count; ++i) {
            logStream.append((char)b[i]);
        }
        return count;
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        int count = in.read(b, off, len);
        for (int i = off; i < (off + count); ++i) {
            logStream.append((char)b[i]);
        }
        return count;
    }
}
