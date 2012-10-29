/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author alemer
 */
class StreamRedirector extends Thread
{
    private final static Logger logger = Logger.getLogger("quanto.core.protocol.streamredirector");
    private InputStream from;
    private OutputStream to;

    public StreamRedirector(InputStream from, OutputStream to) {
        super("IO stream redirector");
        this.from = from;
        this.to = to;
    }

    @Override
    public void run() {
        try {
            byte[] buffer = new byte[200];
            int count = from.read(buffer);
            while (count != -1) {
                to.write(buffer, 0, count);
                to.flush();
                count = from.read(buffer);
            }
        } catch (IOException ex) {
            logger.log(Level.WARNING, "Failed to redirect stderr", ex);
        }
    }
}
