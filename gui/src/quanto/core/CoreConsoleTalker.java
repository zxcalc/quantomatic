/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * 
 * @author alemer
 */
public class CoreConsoleTalker extends CoreTalker {
	private final static Logger logger = Logger
			.getLogger("quanto.core.console");

	private BufferedReader from_backEnd;
	private BufferedWriter to_backEnd;

	public CoreConsoleTalker() throws CoreException {
		super(true);
		try {
			from_backEnd = new BufferedReader(new InputStreamReader(
					getInputStream()));
			to_backEnd = new BufferedWriter(new OutputStreamWriter(
					getOutputStream()));

			logger.finest("Synchonising console...");
			// sync the console
			send("garbage_2039483945;");
			send("HELO;");
			while (!receive().contains("HELO")) {
			}
			logger.finest("Console synchronised successfully");

			// eat prompt
			receive();
		} catch (CoreCommunicationException e) {
			forceDestroy();
			logger.log(Level.SEVERE,
					   "Failed to set up communication with core process", e);
			throw e;
		}
	}

	/**
	 * Sends a command to the core process
	 * 
	 * @param command
	 *            The command to send, with no newline
	 * @throws quanto.gui.QuantoCore.CoreCommunicationException
	 * @throws IllegalStateException
	 *             The core has already been closed
	 */
	private void send(String command) throws CoreCommunicationException {
		if (isClosed()) {
			logger.warning("Tried to send after core had been closed");
			throw new IllegalStateException("Tried to send after core had been closed");
		}
		try {
			if (DEBUG) {
				System.out.print(">>> ");
				System.out.println(command);
				System.out.flush();
			}
			to_backEnd.write(command);
			to_backEnd.newLine();
			to_backEnd.flush();
		} catch (IOException e) {
			writeFailure(e);
		}
	}

	/**
	 * Retrieves a response from the core process
	 * 
	 * If no command has been sent since the last receive, this may block
	 * indefinitely.
	 * 
	 * @return The response
	 * @throws quanto.gui.QuantoCore.CoreCommunicationException
	 * @throws IllegalStateException
	 *             The core has already been closed
	 */
	private String receive() throws CoreCommunicationException {
		synchronized (this) {
			if (isClosed()) {
				logger.warning("Tried to receive after core had been closed");
				throw new IllegalStateException("Tried to receive after core had been closed");
			}
			StringBuilder message = new StringBuilder();
			try {
				if (DEBUG) {
					System.out.print("<<< ");
				}
				// end of text is marked by " "+BACKSPACE (ASCII 8)

				int c = from_backEnd.read();
				while (c != 8) {
					if (c == -1)
						throw new IOException();
					message.append((char) c);
					if (DEBUG)
						System.out.print((char) c);
					c = from_backEnd.read();
				}

				// delete the trailing space
				message.deleteCharAt(message.length() - 1);
				if (DEBUG) {
					System.out.println();
					System.out.flush();
				}
			} catch (IOException e) {
				if (DEBUG) {
					System.out.println();
					System.out.flush();
				}

				logger.log(Level.SEVERE, "Failed to read from core process", e);
				if (message.length() > 0) {
					logger.log(Level.INFO,
							"Received partial message before read failure: {0}",
							message);
				}

				readFailure(e);
			} catch (java.lang.NullPointerException e) {
				if (DEBUG) {
					System.out.println();
					System.out.flush();
				}

				logger.log(Level.SEVERE, "Failed to read from core process", e);
				if (message.length() > 0) {
					logger.log(Level.INFO,
							"Received partial message before read failure: {0}",
							message);
				}

				readFailure(e);
			}

			return message.toString();
		}
	}

	/**
	 * Retrieves a response from the core process
	 * 
	 * If the core process returns an error, a CoreReturnedErrorException will
	 * be thrown.
	 * 
	 * If no command has been sent since the last receive, this may block
	 * indefinitely.
	 * 
	 * @return The response
	 * @throws quanto.gui.QuantoCore.CoreCommunicationException
	 * @throws quanto.gui.QuantoCore.CoreReturnedErrorException
	 * @throws IllegalStateException
	 *             The core has already been closed
	 */
	private String receiveOrFail() throws CoreException {
		String rcv = receive();

		if (rcv.startsWith("!!!")) {
			String error = rcv.substring(4);
			if (error.startsWith("Unknown command")) {
				String command = error.substring("Unknown command: ".length(),
						error.indexOf('('));
				throw new UnknownCommandException(error, command);
			} else if (error.startsWith("Wrong number of args")) {
				String command = error
						.substring("Wrong number of args in ".length(),
								error.indexOf('('));
				throw new CommandArgumentsException(error, command);
			} else {
				throw new CommandException(error);
			}
		}
		return rcv;
	}

	public String quote(Arg arg) {
		String raw = arg.getStringValue();
		if (arg.getType() == Arg.Type.Integer)
			return raw;
		else
			return "\"" + raw.replace("\\", "\\\\").replace("\"", "\\\"")
					+ "\"";
	}

	/**
	 * Send a command with the given arguments. All of the args should be
	 * objects which implement HasName and have non-null names
	 */
	public String commandAsRaw(String name, Arg... args) throws CoreException {
		synchronized (this) {
			StringBuilder cmd = new StringBuilder(name);
			for (Arg arg : args) {
				cmd.append(' ');
				cmd.append(quote(arg));
			}
			cmd.append(';');

			send(cmd.toString());

			String ret;
			try {
				ret = receiveOrFail();
			} finally {
				receive(); // eat the prompt
			}
			return ret.trim();
		}
	}
	
	@Override
	public String console_command(String command) throws CoreException {
		send(command);
		String ret;
		try {
			ret = receive();
		} finally {
			receive(); // eat the prompt
		}
		return ret.trim();
	}

	@Override
	public void command(String name, Arg... args) throws CoreException {
		commandAsRaw(name, args);
	}

	@Override
	public String commandAsName(String name, Arg... args) throws CoreException {
		return commandAsRaw(name, args);
	}

	@Override
	public String[] commandAsList(String name, Arg... args)
			throws CoreException {
		return commandAsRaw(name, args).split("\r\n|\n|\r");
	}

	@Override
	protected void quit() throws CoreCommunicationException {
		send("quit");
	}
}
