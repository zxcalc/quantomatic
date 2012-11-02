package quanto.oldcore.protocol;

/**
 * The command was given the wrong arguments
 */
public class CommandArgumentsException extends CommandException {

	private static final long serialVersionUID = 1232814923748927383L;

	public CommandArgumentsException(String message) {
		super("BADARGS", message);
	}
}
