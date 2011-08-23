/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.logging.Level;
import java.util.logging.Logger;
import quanto.core.protocol.ProtocolManager;

/**
 * Simulates a console interface to the core backend
 * 
 * @author alemer
 */
public class ConsoleInterface {
	private final static Logger logger = Logger
			.getLogger("quanto.core");

	public interface ResponseListener {
		public void responseReceived(String response);
	}

	private ProtocolManager core;
	private ResponseListener responseListener;
	private Completer completer;

	public ConsoleInterface(ProtocolManager core) {
		this.core = core;

		completer = new Completer();

		try {
                        logger.finest("Retrieving commands...");
                        String[] commands = core.consoleCommandList();
			for (String cmd : commands) {
                                completer.addWord(cmd);
                        }
			logger.finest("Commands retrieved successfully");
		} catch (CoreException ex) {
			logger.log(Level.WARNING, "Failed to retreive commands for completion", ex);
		}

	}

	public void setResponseListener(ResponseListener responseListener) {
		this.responseListener = responseListener;
	}

	public ResponseListener getResponseListener() {
		return responseListener;
	}

	public Completer getCompleter() {
		return completer;
	}

	/**
	 * Execute the command asynchronously, depending on the response listener to
	 * deal with the reply.
	 * 
	 * Note: currently, this is a fake - it just calls inputCommandSync.
	 * 
	 * @param input
	 * @throws quanto.gui.QuantoCore.CoreException
	 */
	public void inputCommandAsync(String input) throws CoreException,
			ParseException {
		inputCommandSync(input, true);
	}

	public String inputCommandSync(String input, boolean notify)
			throws CoreException, ParseException {
		String ret;
                ret = core.consoleCommand(input);
		if (notify && (responseListener != null)) {
			responseListener.responseReceived(ret);
		}
		return ret;
	}
}
