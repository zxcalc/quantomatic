package quanto.core;

/**
 * Could not communicate with the backend, because it failed to start
 */
public class CoreExecutionException extends CoreException {

	private static final long serialVersionUID = 1053659906558198953L;

	public CoreExecutionException() {
		super("The core process could not be executed");
	}

	public CoreExecutionException(String msg) {
		super(msg);
	}

	public CoreExecutionException(Throwable cause) {
		super("The core process could not be executed", cause);
	}

	public CoreExecutionException(String msg, Throwable cause) {
		super(msg, cause);
	}
}
