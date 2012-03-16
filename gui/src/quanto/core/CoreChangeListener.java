/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core;

import java.util.EventListener;

/**
 *
 * @author alemer
 */
public interface CoreChangeListener extends EventListener {
    void theoryChanged(CoreEvent evt);
}
