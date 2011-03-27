/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

/**
 *
 * @author alex
 */
public class ParseException extends Exception {
	private static final long serialVersionUID = 2342374892173482937L;
	public ParseException() { }
	public ParseException(String message) {
		super(message);
	}
	public ParseException(Throwable cause) {
		super(cause);
	}
	public ParseException(String message, Throwable cause) {
		super(message, cause);
	}
}
