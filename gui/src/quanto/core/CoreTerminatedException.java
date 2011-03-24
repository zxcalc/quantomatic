package quanto.core;

/**
 * Could not communicate with the backend, because it has terminated
 */
public class CoreTerminatedException extends CoreCommunicationException {

	private static final long serialVersionUID = -234829037423847923L;

	public CoreTerminatedException() {
		super("The core process terminated unexpectedly");
	}

	public CoreTerminatedException(String msg) {
		super(msg);
	}

	public CoreTerminatedException(Throwable cause) {
		super("The core process terminated unexpectedly", cause);
	}

	public CoreTerminatedException(String msg, Throwable cause) {
		super(msg, cause);
	}
}
