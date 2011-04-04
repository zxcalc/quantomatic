/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.util.LinkedList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Simulates a console interface to the core backend
 * 
 * @author alemer
 */
public class ConsoleInterface {
	private final static Logger logger = LoggerFactory
			.getLogger(ConsoleInterface.class);

	public interface ResponseListener {
		public void responseReceived(String response);
	}

	private CoreTalker core;
	private ResponseListener responseListener;
	private Completer completer;

	public ConsoleInterface(CoreTalker core) {
		this.core = core;

		// Construct the completion engine from the output of the help command.
		completer = new Completer();
		logger.info("Retrieving commands...");

		try {
			// FIXME: we need a command_list command
			String result = core.commandAsRaw("help");
			BufferedReader reader = new BufferedReader(new StringReader(result));
			// eat a couple of lines of description
			reader.readLine();
			reader.readLine();
			for (String ln = reader.readLine(); ln != null; ln = reader
					.readLine())
				if (!ln.equals(""))
					completer.addWord(ln);
			logger.info("Commands retrieved successfully");
		} catch (IOException ex) {
			logger.error("Failed to retreive commands for completion", ex);
		} catch (CoreException ex) {
			logger.error("Failed to retreive commands for completion", ex);
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
		try {
			ret = core.console_command(input);
		} catch (CommandException ex) {
			ret = String.format("Error: %1$\n", ex.getMessage());
		}
		if (notify && (responseListener != null)) {
			responseListener.responseReceived(ret);
		}
		return ret;
	}
}
