/*
 * This exception happens if there is a problem communicating with the core,
 * e.g. the core won't start or it sent/received bad JSON.
 */

package quanto.core;

/**
 *
 * @author alek
 */
public class CoreProtocolException extends CoreException {
    public CoreProtocolException() {
		super();
	}
	public CoreProtocolException(String msg) {
		super(msg);
	}
	public CoreProtocolException(Throwable cause) {
		super(cause);
	}
	public CoreProtocolException(String msg, Throwable cause) {
		super(msg, cause);
	}
}
