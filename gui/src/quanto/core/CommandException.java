package quanto.core;

/**
 * A command failed
 */
public class CommandException extends CoreException {

	private static final long serialVersionUID = 1232814923748927383L;
	private String command;

	public CommandException(String command) {
		super("Could not run command \"" + command + "\"");
		this.command = command;
	}

	public CommandException(String message, String command) {
		super(message);
		this.command = command;
	}

	public String getCommand() {
		return command;
	}
}
