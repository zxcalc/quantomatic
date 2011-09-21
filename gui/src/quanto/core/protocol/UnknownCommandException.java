package quanto.core.protocol;

import quanto.core.protocol.CommandException;

/**
 * The command was not recognised by the core
 */
public class UnknownCommandException extends CommandException {

	private static final long serialVersionUID = 1232814923748927383L;

        private String command;

	public UnknownCommandException(String command) {
		super("BADCOMMAND", "Unknown command \"" + command + "\"");
                this.command = command;
	}

        public String getCommand() {
                return command;
        }
}
