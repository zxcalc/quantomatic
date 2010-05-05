/**
 * @(#)StateChangeAction.java	1.6 03/04/16
 *
 * Copyright 2003 Sun Microsystems, Inc. All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * - Redistribution in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials
 * provided with the distribution.
 * 
 * Neither the name of Sun Microsystems, Inc. or the names of
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * 
 * This software is provided "AS IS," without a warranty of any
 * kind. ALL EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND
 * WARRANTIES, INCLUDING ANY IMPLIED WARRANTY OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT, ARE HEREBY
 * EXCLUDED. SUN AND ITS LICENSORS SHALL NOT BE LIABLE FOR ANY
 * DAMAGES OR LIABILITIES SUFFERED BY LICENSEE AS A RESULT OF OR
 * RELATING TO USE, MODIFICATION OR DISTRIBUTION OF THIS SOFTWARE OR
 * ITS DERIVATIVES. IN NO EVENT WILL SUN OR ITS LICENSORS BE LIABLE
 * FOR ANY LOST REVENUE, PROFIT OR DATA, OR FOR DIRECT, INDIRECT,
 * SPECIAL, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER
 * CAUSED AND REGARDLESS OF THE THEORY OF LIABILITY, ARISING OUT OF
 * THE USE OF OR INABILITY TO USE THIS SOFTWARE, EVEN IF SUN HAS
 * BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 * 
 * You acknowledge that this software is not designed, licensed or
 * intended for use in the design, construction, operation or
 * maintenance of any nuclear facility.
 * 
 */

package com.sun.jaf.ui;

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

/**
 * Extends DelegateActions by adding support for ItemEvents. This class
 * will forward the ItemEvent to the registered itemlisteners
 *
 * @author  Mark Davidson
 */
class StateChangeAction extends DelegateAction
    implements ItemListener {

    protected boolean selected = false;

    // ItemListener delegate. itemSelected Events are forwarded
    // to this listener for handling.
    private ItemListener listener;

    /**
     * @return true if the action is in the selected state
     */
    public boolean isSelected()  {
        return selected;
    }

    /**
     * Changes the state of the action
     * @param newValue true to set the action as selected of the action.
     */
    public synchronized void setSelected(boolean newValue) {
        boolean oldValue = this.selected;
	if (oldValue != newValue) {
	    this.selected = newValue;
	    firePropertyChange("selected", Boolean.valueOf(oldValue),
			       Boolean.valueOf(newValue));
	}
    }

    /**
     * Sets the ItemListener to delegate this action to.
     */
    public void addItemListener(ItemListener listener) {
	this.listener = listener;
    }

    public ItemListener[] getItemListeners() {
	return new ItemListener[] { listener };
    }

    public void removeItemListener(ItemListener listener) {
	this.listener = null;
    }

    public void itemStateChanged(ItemEvent evt) {
        // Update all objects that share this item
	boolean newValue;
	boolean oldValue = this.selected;

        if (evt.getStateChange() == ItemEvent.SELECTED) {
	    newValue = true;
	} else {
	    newValue = false;
	}

	if (oldValue != newValue) {
	    setSelected(newValue);

	    // Forward the event to the delgate for handling.
	    if (listener != null) {
		listener.itemStateChanged(evt);
	    }
	}
    }
}
