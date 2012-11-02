package quanto.oldcore.protocol;

import quanto.core.CoreException;

/**
 * A command failed
 */
public class CommandException extends CoreException {

	private static final long serialVersionUID = 1232814923748927383L;
	private String code;

	public CommandException(String code, String message) {
		super(message);
		this.code = code;
	}

	public String getCode() {
		return code;
	}
}
