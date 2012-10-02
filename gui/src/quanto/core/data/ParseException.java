/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.data;

import quanto.core.CoreCommunicationException;

/**
 *
 * @author alemer
 */
public class ParseException extends CoreCommunicationException {

	public ParseException() {
		super("The core sent invalid data");
	}

	public ParseException(String msg) {
		super(msg);
	}

	public ParseException(Throwable cause) {
		super("The core sent invalid data", cause);
	}

	public ParseException(String msg, Throwable cause) {
		super(msg, cause);
	}
	
}
