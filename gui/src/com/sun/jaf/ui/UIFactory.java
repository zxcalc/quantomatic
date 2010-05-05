/**
 * @(#)UIFactory.java	1.13 03/04/16
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

import java.util.HashMap;
import java.util.Iterator;

import javax.swing.*;

// NB: modified from original source: most removing enforcement of
//     the singleton pattern

/** 
 * Creates user interface elements based on action-lists managed
 * in an ActionManagaer.
 * @see ActionManager
 */
public class UIFactory {

    private static UIFactory INSTANCE;

    private ActionManager manager;
    private HashMap<Integer,ButtonGroup> groupMap;
    
    public UIFactory() {
    }

    public UIFactory(ActionManager manager) {
	this.manager = manager;
    }

    /**
     * Return the instance of the UIFactory if this will be used 
     * as a singleton. The instance will be created if it hasn't 
     * previously been set.
     *
     * @return the UIFactory instance.
     * @see #setInstance
     */
    public static UIFactory getInstance() {
	if (INSTANCE == null) {
	    INSTANCE = new UIFactory();
	}
	return INSTANCE;
    }

    /**
     * Gets the ActionManager instance. If the ActionManager has not been explicitly
     * set then the default ActionManager instance will be used.
     *
     * @return the ActionManager used by the UIFactory.
     * @see #setActionManager
     */
    public ActionManager getActionManager() {
	if (manager == null) {
	    manager = ActionManager.getInstance();
	}
	return manager;
    }
    
    /**
     * Sets the ActionManager instance that will be used by this UIFactory
     */
    public void setActionManager(ActionManager manager) {
	this.manager = manager;
    }


    /**
     * Constructs a toolbar from an action-list id. By convention, 
     * the identifier of the main toolbar should be "main-toolbar"
     *
     * @param id action-list id which should be used to construct the toolbar.
     * @return the toolbar or null
     */
    public JToolBar createToolBar(String id) {
	ActionList list = getActionManager().getActionList(id);
	if (list == null) {
	    return null;
	}

	JToolBar toolbar = new JToolBar();
	Iterator iter = list.iterator();
	while(iter.hasNext()) {
	    Object element = iter.next();
	    
	    if (element == null) {
		toolbar.addSeparator();
	    } else if (element instanceof String) {
		toolbar.add(createButton((String)element,
					 list.getGroup((String)element),
					 toolbar));
	    } 
	}
	return toolbar;
    }

    /**
     * Constructs a popup menu from an action-list id.
     *
     * @param id action-list id which should be used to construct the popup.
     * @return the popup or null
     */
    public JPopupMenu createPopup(String id) {
	ActionList list = getActionManager().getActionList(id);
	if (list == null) {
	    return null;
	}

	JPopupMenu popup = new JPopupMenu();
	Iterator iter = list.iterator();
	while(iter.hasNext()) {
	    Object element = iter.next();
	    
	    if (element == null) {
		popup.addSeparator();
	    } else if (element instanceof String) {
		popup.add(createMenuItem((String)element,
					 list.getGroup((String)element), 
					 popup));
	    } 
	}
	return popup;
    }

    /**
     * Constructs a menu tree from an action-list id. The 
     * elements of the action-list will represent menus. By convention, the 
     * top level menu bar action-list id should be "main-menu"
     *
     * @param id the action-list id which represents the root item
     * @return a menu bar which represents the xml tree or null if the 
     * tree cannot be found.
     */
    public JMenuBar createMenuBar(String id) {
	ActionList list = getActionManager().getActionList(id);
	if (list == null) {
	    return null;
	}

	JMenuBar menubar = new JMenuBar();
	JMenu menu = null;

	Iterator iter = list.iterator();
	while(iter.hasNext()) {
	    Object element = iter.next();
	    
	    if (element == null) {
		if (menu != null) {
		    menu.addSeparator();
		}
	    } else if (element instanceof String) {
		if (menu != null) {
		    menu.add(createMenuItem((String)element, 
					    list.getGroup((String)element),
					    menu));
		}
	    } else if (element instanceof ActionList) {
		menu = createMenu((ActionList)element);
		if (menu != null) {
		    menubar.add(menu);
		}
	    }
	}
	return menubar;
    }
	

    /**
     * Creates and returns a menu from an action-list id. The menu
     * constructed will have the attributes from the action id the ActionList.
     * 
     * @param id the action-list id that may represents a menu.
     * @return the constructed JMenu or null if an action-list cannot be found.
     */
    public JMenu createMenu(String id ) {
	ActionList list = getActionManager().getActionList(id);
	if (list == null) {
	    return null;
	}
	return createMenu(list);
    }

    /**
     * Creates and returns a menu from an ActionList class. The menu
     * constructed will have the attributes from the action id the ActionList.
     * 
     * @param list an ActionList that may represents a menu.
     * @return the constructed JMenu or null if the ActionList doesn't have an 
     *         action id
     */
    private JMenu createMenu(ActionList list) {
	Action action = getAction(list.getActionRef());
	if (action == null) {
	    return null;
	}
	JMenu menu = new JMenu(action);

	Iterator iter = list.iterator();
	while(iter.hasNext()) {
	    Object element = iter.next();
	    if (element == null) {
		menu.addSeparator();
	    } else if (element instanceof String) {
		menu.add(createMenuItem((String)element, 
					list.getGroup((String)element),
					menu));
	    } else if (element instanceof ActionList) {
		JMenu newMenu = createMenu((ActionList)element);
		if (newMenu != null) {
		    menu.add(newMenu);
		}
	    }
	}
	return menu;
    }


    /**
     * Convenience method to get the action from an ActionManager.
     */
    private Action getAction(String id) {
	Action action = getActionManager().getAction(id);
	if (action == null) {
	    throw new RuntimeException("ERROR: No Action for " + id);
	}
	return action;
    }

    /**
     * Returns the button group corresponding to the groupid
     * 
     * @param groupid the value of the groupid attribute for the action element
     */
    private ButtonGroup getGroup(String groupid, JComponent container) {
	if (groupMap == null) {
	    groupMap = new HashMap<Integer,ButtonGroup>();
	}
	Integer hashCode = new Integer(groupid.hashCode() ^ container.hashCode());

	ButtonGroup group = groupMap.get(hashCode);
	if (group == null) {
	    group = new ButtonGroup();
	    groupMap.put(hashCode, group);
	}
	return group;
    }

    /**
     * Creates a menu item based on the attributes of the action element.
     * Will return a JMenuItem, JRadioButtonMenuItem or a JCheckBoxMenuItem
     * depending on the context of the type and groupid attibute.
     * 
     * @return a JMenuItem or subclass depending on type.
     */
    private JMenuItem createMenuItem(String id, String groupid, 
				     JComponent container) {
	// swich on type dictated by the type of action ie., 
	// StateChangeAction vs. DelegateAction
	Action action = getAction(id);
	JMenuItem menuItem;
	if (action instanceof StateChangeAction) {
	    // If this action has a groupid attribute then it's a 
	    // GroupAction
	    if (groupid != null) {
		menuItem = createRadioButtonMenuItem(getGroup(groupid, container), 
						     (StateChangeAction)action);
	    } else {
		menuItem = createCheckBoxMenuItem((StateChangeAction)action);
	    }
	} else {
	    menuItem = createMenuItem(action);
	}
	return menuItem;
    }


    /**
     * Creates a button based on the attributes of the action element.
     * Will return a JButton or a JToggleButton.
     */
    private AbstractButton createButton(String id, String groupid, 
					JComponent container) {

	// swich on type dictated by the type of action ie., 
	// StateChangeAction vs. DelegateAction
	Action action = getAction(id);
	if (action == null) {
	    // XXX - 
	    System.out.println("ERROR: Action doesn't exist for " + id);
	    return null;
	}

	AbstractButton button;
	if (action instanceof StateChangeAction) {
	    // If this action has a groupid attribute then it's a 
	    // GroupAction
	    if (groupid == null) {
		button = createToggleButton((StateChangeAction)action);
	    } else {
		button = createToggleButton((StateChangeAction)action,
					    getGroup(groupid, container));
	    }
	} else {
	    button = createButton(action);
	}
	return button;
    }

    /** 
     * Adds and configures the button based on the action. 
     */
    private JButton createButton(Action action)  {
        JButton button = new JButton(action);

        configureButton(button, action);
	return button;
    }
    
    /** 
     * Adds and configures a toggle button.
     * @param a an abstraction of a toggle action.
     */
    private JToggleButton createToggleButton(StateChangeAction a)  {
	return createToggleButton(a, null);
    }

    /** 
     * Adds and configures a toggle button.
     * @param a an abstraction of a toggle action.
     * @param group the group to add the toggle button or null
     */
    private JToggleButton createToggleButton(StateChangeAction a, ButtonGroup group)  {
        JToggleButton button = new JToggleButton(a);
	button.addItemListener(a);
        button.setSelected(a.isSelected());
	if (group != null) {
	    group.add(button);
	}        
        configureToggleButton(button, a);
	return button;
    }

    /** 
     * This method will be called after toggle buttons are created.
     * Override for custom configuration but the overriden method should be called
     * first.
     *
     * @param button the button to be configured
     * @param action the action used to construct the menu item.
     */
    protected void configureToggleButton(JToggleButton button, Action action) {
	configureButton(button, action);
	
	// The PropertyChangeListener that gets added
	// to the Action doesn't know how to handle the "selected" property change
	// in the meantime, the corect thing to do is to add another PropertyChangeListener
	// to the StateChangeAction until this is fixed.
	action.addPropertyChangeListener(new ToggleActionPropertyChangeListener(button));
    }


    /** 
     * This method will be called after buttons created from an action.
     * Override for custom configuration.
     *
     * @param button the button to be configured
     * @param action the action used to construct the menu item.
     */
    protected void configureButton(AbstractButton button, Action action)  {
	if (action.getValue(Action.SHORT_DESCRIPTION) == null) {
	    button.setToolTipText((String)action.getValue(Action.NAME));
	}

        // Don't show the text under the toolbar buttons.
        button.setText("");
    }



    /**
     * This method will be called after toggle type menu items (like
     * JRadioButtonMenuItem and JCheckBoxMenuItem) are created.
     * Override for custom configuration but the overriden method should be called
     * first.
     *
     * @param menuItem the menu item to be configured
     * @param action the action used to construct the menu item.
     */
    protected void configureToggleMenuItem(JMenuItem menuItem, Action action) {
	configureMenuItem(menuItem, action);

	// The PropertyChangeListener that gets added
	// to the Action doesn't know how to handle the "selected" property change
	// in the meantime, the corect thing to do is to add another PropertyChangeListener
	// to the StateChangeAction until this is fixed.
	action.addPropertyChangeListener(new ToggleActionPropertyChangeListener(menuItem));
    }


    /**
     * This method will be called after menu items are created.
     * Override for custom configuration.
     *
     * @param menuItem the menu item to be configured
     * @param action the action used to construct the menu item.
     */
    protected void configureMenuItem(JMenuItem menuItem, Action action) {
    }


    /**
     * Helper method to configure each item consistantly
     */
    private JMenuItem createMenuItem(Action action)  {
        JMenuItem mi = new JMenuItem(action);
        configureMenuItem(mi, action);
	return mi;
    }

    /**
     * Helper method to add a checkbox menu item
     */
    private JCheckBoxMenuItem createCheckBoxMenuItem(StateChangeAction a)  {
        return createCheckBoxMenuItem(a, false);
    }

    /**
     * Helper method to add a checkbox menu item.
     */
    private JCheckBoxMenuItem createCheckBoxMenuItem(StateChangeAction a,
						       boolean selected)  {
        JCheckBoxMenuItem mi = new JCheckBoxMenuItem(a);
        mi.addItemListener(a);
        mi.setSelected(selected);

        configureToggleMenuItem(mi, a);
	return mi;
    }

    /**
     * Helper method to add a radio button menu item.
     */
    private JRadioButtonMenuItem createRadioButtonMenuItem(ButtonGroup group, 
					     StateChangeAction a)  {
       return createRadioButtonMenuItem(group, a, false);
    }

    /**
     * Helper method to add a radio button menu item.
     */
    private JRadioButtonMenuItem createRadioButtonMenuItem(ButtonGroup group,
					  StateChangeAction a, boolean selected)  {
        JRadioButtonMenuItem mi = new JRadioButtonMenuItem(a);
        mi.addItemListener(a);
        mi.setSelected(selected);
	if (group != null) {
	    group.add(mi);
	}
        configureToggleMenuItem(mi, a);
	return mi;
    }
}
