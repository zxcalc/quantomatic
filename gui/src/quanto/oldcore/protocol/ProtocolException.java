/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.oldcore.protocol;

import quanto.oldcore.CoreCommunicationException;

/**
 *
 * @author alex
 */
public class ProtocolException extends CoreCommunicationException {

	public ProtocolException() {
		super("Invalid data was received from the core");
	}

	public ProtocolException(String string) {
		super(string);
	}

	public ProtocolException(Throwable thrwbl) {
		super("Invalid data was received from the core", thrwbl);
	}

	public ProtocolException(String string, Throwable thrwbl) {
		super(string, thrwbl);
	}

}
