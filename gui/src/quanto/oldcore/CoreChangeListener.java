/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.oldcore;

import java.util.EventListener;

/**
 *
 * @author alemer
 */
public interface CoreChangeListener extends EventListener {
    void theoryAboutToChange(TheoryChangeEvent evt);
    void theoryChanged(TheoryChangeEvent evt);
}
