/**
 * @(#)ActionList.java	1.5 02/10/08
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

import java.util.*;

/**
 * Represents a list of action ids, ActionList elements or null. The null
 * element may represent a separation of elements.
 * 
 * This should be regarded as a "friend" class since it is created by
 * the ActionManager for the UIFactory only. This class is not meant
 * for general public consumption.
 */
class ActionList {
	
    private String id;
    private String actionref;
    private HashMap<String, String> groupMap;
    private List<Object> list;
	
    public ActionList(String id, String actionref) {
	this.id = id;
	this.actionref = actionref;
	this.list = new ArrayList<Object>();
    }
	
    /**
     * Retuns the action-list id that this class represents.
     */
    public String getID() {
	return id;
    }

    /**
     * Returns the ActionRef an action id that should be used for the
     * action which represents this list item.
     */
    public String getActionRef() {
	return actionref;
    }

    /**
     * Returns the group id for the current action id
     * @param id action id
     * @return the group id for action or null if it doens't exist.
     */
    public String getGroup(String id) {
	if (groupMap == null) {
	    return null;
	}
	return groupMap.get(id);
    }

    /**
     * Maps the group with the action id for this ActionList
     * @param id action id
     * @param group the group id
     */
    public void setGroup(String id, String group) {
	if (groupMap == null) {
	    groupMap = new HashMap<String, String>();
	}
	groupMap.put(id, group);
    }

    // 
    // List implementation
    //

    public int size() {
	return list.size();
    }

    public boolean add(Object o) {
	return list.add(o);
    }

    public Iterator<Object> iterator() {
	return list.iterator();
    }

    /*
    public boolean isEmpty() {
	return list.isEmpty();
    }

    public boolean contains(Object o) {
	return list.contains(o);
    }


    public Object[] toArray() {
	return list.toArray();
    }

    public Object[] toArray(Object[] a) {
	return list.toArray(a);
	} 
    */

}
    
