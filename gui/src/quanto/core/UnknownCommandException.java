package quanto.core;

/**
 * The command was not recognised by the core
 */
public class UnknownCommandException extends CommandException {

	private static final long serialVersionUID = 1232814923748927383L;

	public UnknownCommandException(String command) {
		super("Unknown command \"" + command + "\"", command);
	}

	public UnknownCommandException(String message, String command) {
		super(message, command);
	}
}
