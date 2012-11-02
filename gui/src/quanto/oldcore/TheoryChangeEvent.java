/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.oldcore;

import java.util.EventObject;

/**
 *
 * @author alemer
 */
public class TheoryChangeEvent extends EventObject {
    private final Theory oldTheory;
    private final Theory newTheory;

    public TheoryChangeEvent(Object source, Theory oldTheory, Theory newTheory) {
        super(source);
        this.oldTheory = oldTheory;
        this.newTheory = newTheory;
    }

    public Theory getOldTheory() {
        return oldTheory;
    }

    public Theory getNewTheory() {
        return newTheory;
    }
}
