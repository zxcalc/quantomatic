/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;
import quanto.util.LoggingInputStream;
import quanto.util.LoggingOutputStream;

/**
 *
 * @author alek
 */
public class CoreTalker {
    private final static Logger logger = Logger.getLogger("quanto.core");
    private InputStream input;
    private OutputStream output;
    
    public CoreTalker() {
    }

	public void connect(InputStream input, OutputStream output) throws IOException {
		this.input = new LoggingInputStream(input, "quanto.core");
		this.output = new LoggingOutputStream(output, "quanto.core");
	}

	public void disconnect() {
        try {
			input.close();
			output.close();
		} catch (IOException ex) {
			logger.log(Level.WARNING, "Failed to close communication channels to the core", ex);
		}
		input = null;
		output = null;
	}
}
