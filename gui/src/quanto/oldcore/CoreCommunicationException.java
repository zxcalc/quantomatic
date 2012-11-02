package quanto.oldcore;

import quanto.core.CoreException;

/**
 * Indicates an issue in communicating with the backend.
 * 
 * Generally, exceptions of this type are unrecoverable.  It indicates that
 * the core process terminated, or communication with the core was disrupted
 * for another reason, or the core sent invalid data 
 */
public class CoreCommunicationException extends CoreException {

	private static final long serialVersionUID = 1053659906558198953L;

	public CoreCommunicationException() {
		super("Failed to communicate with the core process");
	}

	public CoreCommunicationException(String msg) {
		super(msg);
	}

	public CoreCommunicationException(Throwable cause) {
		super("Failed to communicate with the core process", cause);
	}

	public CoreCommunicationException(String msg, Throwable cause) {
		super(msg, cause);
	}
}
