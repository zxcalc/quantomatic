/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui;

/**
 *
 * @author alex
 */
public class DuplicateTheoryException extends Exception {
    private String theoryName;

    public DuplicateTheoryException(String theoryName) {
        super("There is already a theory called \"" + theoryName + "\"");
        this.theoryName = theoryName;
    }

    public String getTheoryName() {
        return theoryName;
    }
}
