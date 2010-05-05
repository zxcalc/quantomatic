/**
 * @(#)ToggleActionPropertyChangeListener.java	1.5 02/10/07
 *
 * Copyright 2002 Sun Microsystems, Inc. All Rights Reserved.
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

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

import javax.swing.AbstractButton;

/**
 * Added to the Toggle type buttons and menu items so that various components
 * which have been created from a single StateChangeAction can be in synch
 */
class ToggleActionPropertyChangeListener implements PropertyChangeListener {

    // XXX - Should be a WeakRef since it's unreachable! 
    // this is a potential memory leak but we don't really have to
    // worry about it since the most of the time the buttons will be
    // loaded for the lifetime of the application. Should make it
    // weak referenced for a general purpose toolkit.
    private AbstractButton button; 

    public ToggleActionPropertyChangeListener(AbstractButton button) {
	this.button = button;
    }

    public void propertyChange(PropertyChangeEvent evt) {
	String propertyName = evt.getPropertyName();

	if (propertyName.equals("selected")) {
	    Boolean selected = (Boolean)evt.getNewValue();
	    button.setSelected(selected.booleanValue());
	}
    }
}
