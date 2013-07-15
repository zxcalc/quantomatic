/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.util;

import java.io.CharArrayWriter;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author alemer
 */
public class LoggingOutputStream extends FilterOutputStream
{
    @SuppressWarnings("NonConstantLogger")
    private final Logger logger;
    private final CharArrayWriter logStream = new CharArrayWriter(256);

    public LoggingOutputStream(OutputStream internal, String logArea) {
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
    public void write(int b) throws IOException {
        logStream.append((char)b);
        out.write(b);
    }

    @Override
    public void write(byte[] b) throws IOException {
        for (int i = 0; i < b.length; ++i) {
            logStream.append((char)b[i]);
        }
        out.write(b);
    }

    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        for (int i = 0; i < b.length; ++i) {
            logStream.append((char)b[i]);
        }
        out.write(b, off, len);
    }
}
