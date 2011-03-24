package quanto.core;

/**
 * Could not communicate with the backend
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
