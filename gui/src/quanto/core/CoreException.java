/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

/**
 *
 * @author alex
 */
public class CoreException extends Exception {
	private static final long serialVersionUID = 1053659906558198953L;
	public CoreException() {
		super();
	}
	public CoreException(String msg) {
		super(msg);
	}
	public CoreException(Throwable cause) {
		super(cause);
	}
	public CoreException(String msg, Throwable cause) {
		super(msg, cause);
	}
}
