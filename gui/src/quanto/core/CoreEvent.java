/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core;

import java.util.EventObject;

/**
 *
 * @author alemer
 */
public class CoreEvent extends EventObject {
    private final Theory theory;

    public CoreEvent(Object source, Theory theory) {
        super(source);
        this.theory = theory;
    }

    public Theory getTheory() {
        return theory;
    }
}
