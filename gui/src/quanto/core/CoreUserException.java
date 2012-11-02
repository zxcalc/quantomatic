/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

/**
 *
 * @author alek
 */
public class CoreUserException extends CoreException {

    public CoreUserException(String msg, Throwable cause) {
        super(msg, cause);
    }

    public CoreUserException(Throwable cause) {
        super(cause);
    }

    public CoreUserException(String msg) {
        super(msg);
    }

    public CoreUserException() {
        super();
    }

}
