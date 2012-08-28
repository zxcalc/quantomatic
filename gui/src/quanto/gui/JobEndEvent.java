/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui;

import java.util.EventObject;

/**
 *
 * @author alemer
 */
public class JobEndEvent extends EventObject
{
	private boolean aborted = false;

	public JobEndEvent(Object source) {
		super(source);
	}

	public JobEndEvent(Object source, boolean aborted) {
		super(source);
		this.aborted = aborted;
	}

	public boolean jobWasAborted() {
		return aborted;
	}
    
}
