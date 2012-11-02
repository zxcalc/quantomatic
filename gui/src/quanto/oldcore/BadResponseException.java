package quanto.oldcore;

import quanto.core.CoreException;

/**
 * The core returned an unexpected response
 */
public class BadResponseException extends CoreException {

	private static final long serialVersionUID = 1034523458932534589L;
	private String response;

	public BadResponseException(String msg, String response) {
		super(msg);
		this.response = response;
	}

	public BadResponseException(String response) {
		this.response = response;
	}

	public String getResponse() {
		return response;
	}
}
