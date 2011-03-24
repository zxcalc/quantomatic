package quanto.core;

/**
 * The command was given the wrong arguments
 */
public class CommandArgumentsException extends CommandException {

	private static final long serialVersionUID = 1232814923748927383L;

	public CommandArgumentsException(String command) {
		super("Bad arguments for command \"" + command + "\"", command);
	}

	public CommandArgumentsException(String message, String command) {
		super(message, command);
	}
}
