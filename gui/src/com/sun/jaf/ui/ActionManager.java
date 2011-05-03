/**
 * %W% %E%
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
 */

// NB: some edits made: search for EDIT, also all generics usage

package com.sun.jaf.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import java.beans.EventHandler;
import java.beans.Statement;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;

import java.util.*;

import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.KeyStroke;

import javax.xml.parsers.SAXParserFactory;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import org.xml.sax.helpers.DefaultHandler;

/**
 * The ActionManager manages sets of <code>javax.swing.Action</code>. 
 * The Actions are speficied in an XML configuration file and will be lazily created
 * when referenced. The schema for the XML document contains three major 
 * elements. 
 * <ul>
 *   <li><b>action</b> Represents the properties of an Action.
 *   <li><b>action-list</b> Represents lists and trees of actions which can be used to 
 * construct user interface components like toolbars, menus and popups. 
 *   <li><b>action-set</b> The document root which contains a set of action-lists and actions.
 * </ul>
 * <p>
 * All of these elements have a unique id tag which is used by the ActionManager to reference
 * the element. Refer to <a href="doc-files/action-set.dtd">action-set.dtd</a> 
 * for details on the elements and attributes. 
 * <p>
 * The order of an action in an action-list will reflect to the order of the Action
 * based component in the container. A tree is represented as an action-list that
 * contains one or more action-lists.
 * <p>
 * Once the Actions have been created you
 * need to register callback methods that perform the logic of the 
 * associated Action. A typical use case of the ActionManager is:
 * <p>
 *  <pre>
 *   ActionManager manager = ActionManager.getInstance();
 *   // Load an action-set XML document
 *
 *   manager.loadActions(new URL("my-actions.xml));
 *   // Register a callback for a particular Action
 *
 *   manager.registerCallback("new-action", actionHandler, "handleNewCommand");
 * 
 *   // Change the state of the action:
 *   manager.setEnabled("new-action", newState);
 * </pre>
 * 
 * The ActionManager also supports Actions that can have a selected state
 * associated with them. These Actions are typically represented by a
 * JCheckBox or similar widget. For such actions the registered method is
 * invoked with an additional parameter indicating the selected state of
 * the widget. For example, for the callback handler:
 *<p>
 * <pre> 
 *  public class Handler {
 *      public void stateChanged(boolean newState);
 *   }
 * </pre>
 * The registration method would look similar:
 * <pre>
 *  manager.registerCallback("select-action", new Handler(), "stateChanged");
 * </pre>
 *<p>
 * The stateChanged method would be invoked as the selected state of
 * the widget changed. Additionally if you need to change the selected
 * state of the Action use the ActionManager method <code>setSelected</code>.
 * <p>
 * The UIFactory uses the managed Actions in the ActionManager to create 
 * user interface components. For example, to create a JMenu based on an 
 * action-list id:
 * <pre>
 * JMenu file = UIFactory.getInstance().createMenu("file-menu");
 * </pre>
 *
 * @see UIFactory
 * @see <a href="doc-files/action-set.dtd">action-set.dtd</a>
 * @author Mark Davidson
 */
public class ActionManager {
    
    // Elements in the action-set.dtd
    private final static String ACTION_SET_ELEMENT="action-set";
    private final static String ACTION_ELEMENT="action";
    private final static String ACTION_LIST_ELEMENT="action-list";
    private final static String EMPTY_ELEMENT="empty";
    private final static String GROUP_ELEMENT="group";

    private final static String ACCEL_ATTRIBUTE = "accel";
    private final static String DESC_ATTRIBUTE = "desc";
    private final static String ICON_ATTRIBUTE = "icon";
    private final static String ID_ATTRIBUTE = "id";
    private final static String ACTIONREF_ATTRIBUTE = "actionref";
    private final static String IDREF_ATTRIBUTE = "idref";
    private final static String MNEMONIC_ATTRIBUTE = "mnemonic";
    private final static String NAME_ATTRIBUTE = "name";
    private final static String SMICON_ATTRIBUTE = "smicon";
    private final static String TYPE_ATTRIBUTE = "type";

    // Indexes into the ActionAttributes array.
    private final static int ACCEL_INDEX = 0;
    private final static int DESC_INDEX = 1;
    private final static int ICON_INDEX = 2;
    private final static int ID_INDEX = 3;
    private final static int MNEMONIC_INDEX = 4;
    private final static int NAME_INDEX = 5;
    private final static int SMICON_INDEX = 6;
    private final static int TYPE_INDEX = 7;
    
    private static SAXParserFactory parserfactory;
    private ActionHandler handler;

    /**
     * A simple class that holds the parsed XML Attributes.
     */
    class ActionAttributes {

	private String[] array;

	public ActionAttributes(Attributes attrs) {
	    // Populate the array with the objects that map to the 
	    // attributes
	    array = new String[8];
	    array[0] = attrs.getValue(ACCEL_ATTRIBUTE);
	    array[1] = attrs.getValue(DESC_ATTRIBUTE);
	    array[2] = attrs.getValue(ICON_ATTRIBUTE);
	    array[3] = attrs.getValue(ID_ATTRIBUTE);
	    array[4] = attrs.getValue(MNEMONIC_ATTRIBUTE);
	    array[5] = attrs.getValue(NAME_ATTRIBUTE);
	    array[6] = attrs.getValue(SMICON_ATTRIBUTE);
	    array[7] = attrs.getValue(TYPE_ATTRIBUTE);
	}
	
	/**
	 * Retrieves the Attribute value.
	 * @param index one of ActionManager.._INDEX
	 */
	public String getValue(int index) {
	    return array[index];
	}

	public void setValue(int index, String value) {
	    if (index < array.length && value != null) {
		array[index] = value;
	    }
	}

	public void setAttributes(Attributes attrs) {
	    setValue(0, attrs.getValue(ACCEL_ATTRIBUTE));
	    setValue(1, attrs.getValue(DESC_ATTRIBUTE));
	    setValue(2, attrs.getValue(ICON_ATTRIBUTE));
	    // Don't set the ID_ATTRIBUTE since it should be immutable
	    //	    setValue(3, attrs.getValue(ID_ATTRIBUTE));
	    setValue(4, attrs.getValue(MNEMONIC_ATTRIBUTE));
	    setValue(5, attrs.getValue(NAME_ATTRIBUTE));
	    setValue(6, attrs.getValue(SMICON_ATTRIBUTE));
	    setValue(7, attrs.getValue(TYPE_ATTRIBUTE));
	}
    }


    // Internal data structures which manage the actions.

    // The attrMap is an association between keys (value of ID_ATTRIBUTE)
    // and the associated Action properties in an ActionAttributes class.
    private Map<String, ActionAttributes> attributeMap;

    // key: value of ID_ATTRIBUTE, value instanceof AbstractAction
    private Map<String,Action> actionMap;

    // A mapping between the action-set id and a list of action ids.
    private Map<String,List<String>> actionSetMap;

    // A mapping between the action-list id and an ActionList
    private Map<String,ActionList> actionListMap;

    /**
     * Shared instance of the singleton ActionManager.
     */
    private static ActionManager INSTANCE;

    // To enable debugging:
    //   Pass -Ddebug=true to the vm on start up.
    // or
    //   set System.setProperty("debug", "true"); before constructing this Object

    private static boolean DEBUG = false;

    /**
     * Creates the action manager
     */
    public ActionManager() {
	// XXX - System.getProperty is not allowed by the applet security model.
	// DEBUG = Boolean.valueOf(System.getProperty("debug")).booleanValue();
    }
    
    /**
     * Return the instance of the ActionManger. If this has not been explicity
     * set then it will be created.
     *
     * @return the ActionManager instance.
     * @see #setInstance
     */
    public static ActionManager getInstance() {
	if (INSTANCE == null) {
	    INSTANCE = new ActionManager();
	}
        return INSTANCE;
    }

    /**
     * Sets the ActionManager instance.
     */
    public static void setInstance(ActionManager manager) {
	INSTANCE = manager;
    }

    // The SAX Attributes are stored in a lightweight data structure. 
    // The corresponding Action will be constructed lazily  the first time that 
    // they are requested from the public method getAction.

    /**
     * Adds the values represented in the SAX Attrributes structure
     * to a lightweight internal data strucure.
     *
     * @param attrs the Attributes for an action
     * @param actionset the parent action-set id for the action 
     */
    private void addAttributes(Attributes attrs, String actionset) {
	if (attributeMap == null) {
	    attributeMap = new HashMap<String, ActionAttributes>();
	}
	attributeMap.put(attrs.getValue(ID_ATTRIBUTE), 
			 new ActionAttributes(attrs));

	// Add this action id to the actionset
	if (actionset != null && !actionset.equals("")) {
	    List<String> list = getActionSet(actionset);
	    if (list == null) {
		list = new ArrayList<String>();
	    }
	    list.add(attrs.getValue(ID_ATTRIBUTE));
	    addActionSet(actionset, list);
	}
    }

    /**
     * Return the attributes for an action id.
     * @param action id
     */
    private ActionAttributes getAttributes(String key) {
	if (attributeMap == null) {
	    return null;
	}
	// EDIT (AHM): remove cast
	return attributeMap.get(key);
    }

    /**
     * Unloads the actions that are assocated with the action-set from the
     * action manager. 
     *
     * Note: This method removes the actions and references from 
     * the action manager. References to the 
     * action may still be held in component that were created from 
     * actions.
     *
     * @param actionset the action-set id
     */
    public void unloadActions(String actionset) {
	if (DEBUG) {
	    System.out.println("unloadActions("+actionset+")");
	}
	List<String> list = getActionSet(actionset.toString());
	if (list == null) {
	    return;
	}

	// Remove all Actions and ActionAttributes
	String key = null;
	Iterator<String> iter = list.iterator();
	while (iter.hasNext()) {
	    key = iter.next();
	    if (attributeMap != null) {
		attributeMap.remove(key);
	    }
	    if (actionMap != null) {
		actionMap.remove(key);
	    }
	}
	if (actionSetMap != null) {
	    actionSetMap.remove(actionset);
	}
    }


    /**
     * Returns the ids for all the managed actions. 
     * <p>
     * An action id is a unique idenitfier which can
     * be used to retrieve the corrspondng Action from the ActionManager. 
     * This identifier can also
     * be used to set the properties of the action through the action
     * manager like setting the state of the enabled or selected flags.
     * 
     * @return a set which represents all the action ids
     */
    public Set<String> getActionIDs() {
	if (attributeMap == null) {
	    return null;
	}
	return attributeMap.keySet();
    }

    /**
     * Retrieve the ids for all the managed actions-sets.
     * <p>
     * An action set is an association between an action-set id and the 
     * action ids that it contains. For example, the actions-core.xml 
     * action-set document has the action-set id: "core-actions" that
     * contains the actions: new-command, open-command, save-command, etc...
     * 
     * @return a set which represents all the action-set ids
     */
    public Set<String> getActionSetIDs() {
	if (actionSetMap == null) {
	    actionSetMap = new HashMap<String,List<String>>();
	}
	return actionSetMap.keySet();
    }

    /**
     * Return a List of action ids for an action-set id.
     * @param id the action-set id
     * @return a List of action ids in the set
     */
    private List<String> getActionSet(String id) {
	if (actionSetMap == null) {
	    actionSetMap = new HashMap<String,List<String>>();
	}
	return actionSetMap.get(id);
    }

    private void addActionSet(String key, List<String> set) {
	if (actionSetMap == null) {
	    actionSetMap = new HashMap<String,List<String>>();
	}
	actionSetMap.put(key, set);
    }

    /**
     * Retrieve the ids for all the managed action-lists.
     * <p>
     * An action-list is an ordered collection of actions,
     * action-lists and empty elements which could represent
     * containers of actions. These action-list ids can be 
     * used in factory classes to construct ui action containers
     * like menus, toolbars and popups.
     *
     * @return a set which represents all the action-list ids
     */
    public Set<String> getActionListIDs() {
	if (actionListMap == null) {
	    actionListMap = new HashMap<String,ActionList>();
	}
	return actionListMap.keySet();
    }

    /**
     * Return an ActionList for an action-list id.
     * @param id the action-list id
     * @return an ActionList List of action ids in the set
     */
    ActionList getActionList(String id) {
	if (actionListMap == null) {
	    return null;
	}
	return actionListMap.get(id);
    }

    private void addActionList(String id, ActionList list) {
	if (actionListMap == null) {
	    actionListMap = new HashMap<String,ActionList>();
	}
	actionListMap.put(id, list);
    }
    

    /** 
     * Adds an action to the ActionManager
     * @param id value of the action id
     */
    private void addAction(String id, Action action)  {
        if (actionMap == null) {
	    actionMap = new HashMap<String,Action>();
	}
	actionMap.put(id, action);
    }

    
    /** 
     * Retrieves the action corresponding to an action id.
     * 
     * @param id value of the action id
     * @return an Action or null if id 
     */
    public Action getAction(String id)  {
	if (actionMap == null) {
	    actionMap = new HashMap<String,Action>();
	}
	Action action = actionMap.get(id);
	if (action == null) {
	    action = createAction(getAttributes(id));
	}

	return action;
    }
    
    /**
     * Convenience method for returning the DelegateAction
     *
     * @param id value of the action id
     * @return the DelegateAction referenced by the named id or null
     */
    private DelegateAction getDelegateAction(String id) {
	Action a = getAction(id);
	if (a instanceof DelegateAction) {
	    return (DelegateAction)a;
	}
	return null;
    }

    /**
     * Convenience method for returning the StateChangeAction
     *
     * @param id value of the action id
     * @return the StateChangeAction referenced by the named id or null
     */
    private StateChangeAction getStateChangeAction(String id) {
	Action a = getAction(id);
	if (a instanceof StateChangeAction) {
	    return (StateChangeAction)a;
	}
	return null;
    }

    /**
     * Enables or disables the state of the Action corresponding to the 
     * action id. This method should be used
     * by application developers to ensure that all components created from an
     * action remain in synch with respect to their enabled state.
     *
     * @param id value of the action id
     * @param enabled true if the action is to be enabled; otherwise false
     */
    public void setEnabled(String id, boolean enabled) {
	Action action = getAction(id);
	if (action != null) {
	    action.setEnabled(enabled);
	}
    }


    /**
     * Returns the enabled state of the <code>Action</code>. When enabled,
     * any component associated with this object is active and
     * able to fire this object's <code>actionPerformed</code> method.
     *
     * @param id value of the action id
     * @return true if this <code>Action</code> is enabled; false if the 
     *         action doesn't exist or disabled.
     */
    public boolean isEnabled(String id) {
	Action action = getAction(id);
	if (action != null) {
	    return action.isEnabled();
	}
	return false;
    }

    /**
     * Sets the selected state of a toggle action. If the id doesn't
     * correspond to a toggle action then it will fail silently.
     * 
     * @param id the value of the action id
     * @param selected true if the action is to be selected; otherwise false.
     */
    public void setSelected(String id, boolean selected) {
	StateChangeAction action = getStateChangeAction(id);
	if (action != null) {
	    action.setSelected(selected);
	}
    }

    /**
     * Gets the selected state of a toggle action. If the id doesn't
     * correspond to a toggle action then it will fail silently.
     * 
     * @param id the value of the action id
     * @return true if the action is selected; false if the action
     *         doesn't exist or is disabled.
     */
    public boolean isSelected(String id) {
	StateChangeAction action = getStateChangeAction(id);
	if (action != null) {
	    return action.isSelected();
	}
	return false;
    }

    /**
     * Adds the set of actions and action-lists 
     * from an action-set document into the ActionManager.
     * A call to this method usually takes the form:
     * ActionManager.getInstance().loadActions(getClass().getResource("myActions.xml"));
     *
     * @param url URL pointing to an actionSet document
     * @throws IOException If there is an error in parsing
     */
    public void loadActions(URL url) throws IOException {
	if (DEBUG) {
	    System.out.println("loadActions("+url+")");
	}
        InputStream stream = url.openStream();
        try {
            loadActions(stream);
        }
        finally {
            try {
                stream.close();
            } catch (IOException ie) {
            }
        }
    }

    /**
     * Adds the set of actions and action-lists 
     * from an action-set document into the ActionManager.
     *
     * @param stream InputStream containing an actionSet document
     * @throws IOException If there is an error in parsing
     */
    public void loadActions(InputStream stream) throws IOException {
	if (DEBUG) {
	    System.out.println("loadActions("+stream+")");
	}

	if (parserfactory == null) {
	    parserfactory = SAXParserFactory.newInstance();
	    parserfactory.setValidating(true);
	}

	if (handler == null) {
	    handler = new ActionHandler();
	}

        try {
            SAXParser parser = parserfactory.newSAXParser();
            // Append a '/' as otherwise sax will look in this directory
            // vs the lib directory.
	    parser.parse(stream, handler, getClass().getResource("resources").toString() + "/");
	    //           parser.parse(stream, handler, "/");
	} catch (SAXException ex) {
	    printException("SAXException: " + ex.getMessage(), ex);
            throw new IOException("Error parsing: " + ex.getMessage());
	} catch (IOException ex2) {
	    printException("IOException: " + ex2.getMessage(), ex2);
            throw ex2;
	} catch (ParserConfigurationException ex3) {
	    printException("ParserConfigurationException: " + ex3.getMessage(), ex3);
            throw new IOException("Error parsing: " + ex3.getMessage());
	}
    }

    // Helper method to print exceptions.
    // TODO: should probabaly use the logger API.
    private void printException(String message, Exception ex) {
	System.out.println(message);
	if (DEBUG) {
	    ex.printStackTrace();
	}
    }


    /**
     * Returns the Icon associated with the name of the resource.
     * TODO: This should be exposed as a public utility method.
     */
    Icon getIcon(String imagePath) {
	if (imagePath != null && !imagePath.equals("")) {
	    URL url = this.getClass().getResource(imagePath);
	    if (url != null) {
		return new ImageIcon(url);
	    }
	}
	return null;
    }


    /**
     * A diagnostic which prints the Attributes of an action
     * on the printStream
     */
    static void printAction(PrintStream stream, Action action) {
	stream.println("Attributes for " + action.getValue(Action.ACTION_COMMAND_KEY));

	if (action instanceof AbstractAction) {
	    Object[] keys = ((AbstractAction)action).getKeys();

	    for (int i = 0; i < keys.length; i++) {
		stream.println("\tkey: " + keys[i] + "\tvalue: " + 
			       action.getValue((String)keys[i]));
	    }
	}
    }

    /**
     * A diagnostic which prints the all the contained info from the ActionManager
     * on the printStream
     */
    static void printActionManager(PrintStream stream, ActionManager manager) {
	// Print out the action sets.
	Set<String> keys = manager.getActionSetIDs();
	int numItems = keys.size();
	stream.println("Num action-sets: " + numItems);
	Iterator<String> iter = keys.iterator();
	while (iter.hasNext()) {
	    stream.println("\t" + iter.next());
	}
	
	// Print out the action lists.
	keys = manager.getActionListIDs();
	if (keys != null) {
	    numItems = keys.size();
	    stream.println("\nNum action-lists: " + numItems);
	    iter = keys.iterator();
	    while (iter.hasNext()) {
		stream.println("\t" + iter.next());
	    }
	}

	// Key dump
	keys = manager.getActionIDs();
	if (keys != null) {
	    numItems = keys.size();
	    stream.println("\nNum actions: " + numItems);
	    iter = keys.iterator();
	    while (iter.hasNext()) {
		printAction(stream, manager.getAction(iter.next()));
	    }
	}
	
    }
	

    // Implementation methods which create the action from the Attributes

    private Action createAction(ActionAttributes attr) {
	Action action = null;
	if (attr != null) {
	    String type = attr.getValue(TYPE_INDEX);
	    if ("toggle".equals(type)) {
		// Multi state action.
		action = new StateChangeAction();
	    } else {
		// Single state action by default.
		action = new DelegateAction();
	    }
	    configureAction(action, attr);

	    addAction(attr.getValue(ID_INDEX), action);
	}
	return action;
    }

    private boolean controlConvertedToMeta = false;

    public void setControlConvertedToMeta(boolean controlConvertedToMeta) {
        this.controlConvertedToMeta = controlConvertedToMeta;
    }

    public boolean isControlConvertedToMeta() {
        return controlConvertedToMeta;
    }

    /**
     * Configures an action from the attributes.
     * @param action the action to configure
     * @param attr attributes to use in the configuration.
     */
    private void configureAction(Action action, ActionAttributes attr) {
	    
	action.putValue(Action.NAME, attr.getValue(NAME_INDEX));
	// EDIT (AHM): use SMICON_INDEX instead of ICON_INDEX
	action.putValue(Action.SMALL_ICON, getIcon(attr.getValue(SMICON_INDEX)));
	action.putValue(Action.ACTION_COMMAND_KEY, attr.getValue(ID_INDEX));
	action.putValue(Action.SHORT_DESCRIPTION, attr.getValue(DESC_INDEX));
	action.putValue(Action.LONG_DESCRIPTION, attr.getValue(DESC_INDEX));

	String mnemonic = attr.getValue(MNEMONIC_INDEX);
	if (mnemonic != null && !mnemonic.equals("")) {
	    action.putValue(Action.MNEMONIC_KEY, new Integer(mnemonic.charAt(0)));
	}
	String accel = attr.getValue(ACCEL_INDEX);
	if (accel != null && !accel.equals("")) {
	    if (controlConvertedToMeta)
	    {
		accel = accel.replaceFirst("control ", "meta ");
	    }
	    action.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(accel));
	}
    }

    /**
     * Registers a callback method when the Action corresponding to any of
     * the action ids is invoked. When a Component that was constructed from the
     * Action identified by the action id invokes actionPerformed then the method
     * named will be invoked on the handler Object.
     * <p>
     * The method should take a String as an argument, which will be the
     * id of the activated command.
     * <p>
     * If the Action represented by the action id is a StateChangeAction, then
     * the method passed should also take an int as an argument. The value of
     * getStateChange() on the ItemEvent object will be passed as the parameter.
     *
     * @param id value of the action id
     * @param handler the object which will be perform the action
     * @param method the name of the method on the handler which will be called.
     */
    // EDIT (AHM): added this method
    public void registerGenericCallback(Collection<String> ids, Object handler, String method) {
	for (String id : ids) {
	    StateChangeAction sa = getStateChangeAction(id);
	    if (sa != null) {
		// Create a handler for toogle type actions.
		sa.addItemListener(new CommandInvocationHandler(handler, method, id));
	    }
	    else {
		DelegateAction a = getDelegateAction(id);
		if (a != null) {
		    // Create a new ActionListener using the dynamic proxy api.
		    a.addActionListener(new CommandInvocationHandler(handler, method, id));
		}
	    }
	}
    }


    /**
     * The callback for generic methods
     *
     * TODO: should reimplement this class as something that can be persistable.
     */
    // EDIT (AHM): added this class
    private class CommandInvocationHandler implements ActionListener, ItemListener {

	private Object target;
	private String methodName;
	private String commandId;

	public CommandInvocationHandler(Object target, String methodName, String commandId) {
	    this.target = target;
	    this.methodName = methodName;
	    this.commandId = commandId;
	}

	public void itemStateChanged(ItemEvent evt) {
	    Boolean value = Boolean.TRUE;
	    if (evt.getStateChange() == ItemEvent.DESELECTED) {
		value = Boolean.FALSE;
	    }

	    // Build the Statement representing the method to be invoked on the target
	    Statement statement = new Statement(target, methodName, new Object[] { commandId, value });
	    try {
		statement.execute();
	    } catch (Exception ex) {
		ex.printStackTrace();
	    }
	}

	public void actionPerformed(ActionEvent e) {
	    // Build the Statement representing the method to be invoked on the target
	    Statement statement = new Statement(target, methodName, new Object[] { commandId });
	    try {
		statement.execute();
	    } catch (Exception ex) {
		ex.printStackTrace();
	    }
	}
    }


    /**
     * Registers a callback method when the Action corresponding to
     * the action id is invoked. When a Component that was constructed from the
     * Action identified by the action id invokes actionPerformed then the method
     * named will be invoked on the handler Object.
     * <p>
     * If the Action represented by the action id is a StateChangeAction, then
     * the method passed should take an int as an argument. The value of 
     * getStateChange() on the ItemEvent object will be passed as the parameter.
     * 
     * @param id value of the action id
     * @param handler the object which will be perform the action
     * @param method the name of the method on the handler which will be called.
     */
    public void registerCallback(String id, Object handler, String method) {
	StateChangeAction sa = getStateChangeAction(id);
	if (sa != null) {
	    // Create a handler for toogle type actions.
	    sa.addItemListener(new BooleanInvocationHandler(handler, method));
	    return;
	}

	DelegateAction a = getDelegateAction(id);
	if (a != null) {
	    // Create a new ActionListener using the dynamic proxy api.
	    a.addActionListener(EventHandler.create(ActionListener.class,
						    handler, method));
	}
    }


    /**
     * The callback for the StateChangeAction that invokes a method with a
     * boolean argument on a target. 
     * 
     * TODO: should reimplement this class as something that can be persistable.
     */
    private class BooleanInvocationHandler implements ItemListener {
	
	private Object target;
	private String methodName;

	public BooleanInvocationHandler(Object target, String methodName) {
	    this.target = target;
	    this.methodName = methodName;
	}

	public void itemStateChanged(ItemEvent evt) {
	    Boolean value = Boolean.TRUE;
	    if (evt.getStateChange() == ItemEvent.DESELECTED) {
		value = Boolean.FALSE;
	    }
	    
	    // Build the Statement representing the method to be invoked on the target
	    Statement statement = new Statement(target, methodName, new Object[] { value });
	    try {
		statement.execute();
	    } catch (Exception ex) {
		ex.printStackTrace();
	    }
	}
    }


    /**
     * Determines if the Action corresponding to the action id is a state changed
     * action (toggle, group type action).
     *
     * @param id value of the action id
     * @return true if the action id represents a multi state action; false otherwise
     */
    public boolean isStateChangeAction(String id) {
	return (getStateChangeAction(id) != null);
    }

    /**
     * Implemenation of the SAX event handler which acts on elements
     * and attributes defined in the action-set.dtd.
     * 
     * This class creates the lightweight data structures which encapsulate
     * the parsed xml data that can be used to contruct Actions
     * and UI elements from Actions.
     */
    class ActionHandler extends DefaultHandler {

	private Stack<ActionList> actionListStack; // keep track of nested action-lists.
	private Stack<String> actionSetStack; // keep track of nested action-sets.
	
	private String actionset;      // current action-set id
	private ActionList actionlist; // current action-list id
	private String action;         // current action id
	private String group;          // current group id

	@Override
	public void startDocument() {
	    actionListStack = new Stack<ActionList>();
	    actionSetStack = new Stack<String>();

	    actionset = null;
	    actionlist = null;
	    action = null;
	    group = null;
	}
    
	// Overloaded DefaultHandler methods.
	

	@Override
	public void startElement(String nameSpace, String localName, 
				 String name, Attributes attributes) {
	    if (DEBUG) {
		System.out.println("startElement("+nameSpace+","
				   +localName+","+name+",...)");
	    }

	    if (ACTION_SET_ELEMENT.equals(name)) {
		String newSet = attributes.getValue(ID_ATTRIBUTE);
		if (actionset != null) {
		    actionSetStack.push(actionset);
		}
		actionset = newSet;
	    } 
	    else if (ACTION_LIST_ELEMENT.equals(name)) {
		String id = attributes.getValue(ID_ATTRIBUTE);
		action = attributes.getValue(IDREF_ATTRIBUTE);
		if (action == null) {
		    action = id;
		}			
		ActionAttributes actionAtts = getAttributes(action);
		if (actionAtts == null) {
		    addAttributes(attributes, actionset);
		} else {
		    // See if some attributes are redefined
		    actionAtts.setAttributes(attributes);
		}

		ActionList newList = new ActionList(id, action);
		if (actionlist != null) {
		    actionlist.add(newList);
		    actionListStack.push(actionlist);
		}
		addActionList(id, newList);

		actionlist = newList;
	    } 
	    else if (ACTION_ELEMENT.equals(name)) {
		action = attributes.getValue(IDREF_ATTRIBUTE);
		if (action == null) {
		    action = attributes.getValue(ID_ATTRIBUTE);
		}
		ActionAttributes actionAtts = getAttributes(action);
		if (actionAtts == null) {
		    addAttributes(attributes, actionset);
		} else {
		    // See if some attributes are redefined
		    actionAtts.setAttributes(attributes);
		}
		    
		
		// If this action is within an action-list element then add 
		// it to the list.
		if (actionlist != null) {
		    actionlist.add(action);

		    // If this action is within a group element then associate
		    // it with the current action id.
		    if (group != null) {
			actionlist.setGroup(action, group);
		    }
		}
	    } 
	    else if (GROUP_ELEMENT.equals(name)) {
		group = attributes.getValue(ID_ATTRIBUTE);
	    }
	    else if (EMPTY_ELEMENT.equals(name)) {
		if (actionlist != null) {
		    actionlist.add(null);
		}
	    }
	}

	@Override
	public void endElement(String nameSpace, String localName, String name) {
	    if (DEBUG) {
		System.out.println("endElement("+nameSpace+","
				   +localName+","+name+")");
	    }

	    if (ACTION_SET_ELEMENT.equals(name)) {
		try {
		    actionset = (String)actionSetStack.pop();
		} catch (EmptyStackException ex) {
		    actionset = null;
		}
	    }
	    else if (ACTION_LIST_ELEMENT.equals(name)) {
		try {
		    actionlist = (ActionList)actionListStack.pop();
		} catch (EmptyStackException ex) {
		    actionlist = null;
		}
	    } 
	    else if (GROUP_ELEMENT.equals(name)) {
		group = null;
	    }
	}

	@Override
	public void endDocument() {
	    actionListStack = new Stack<ActionList>();
	    actionSetStack = new Stack<String>();

	    actionset = null;
	    actionlist = null;
	    action = null;
	    group = null;
	}

	//
	// Overloaded ErrorHandler methods for Validating parser.
	//

	@Override
	public void error(SAXParseException ex) throws SAXException {
	    System.out.println("**** validation error");
	    reportException(ex);
	}

	@Override
	public void warning(SAXParseException ex) throws SAXException {
	    System.out.println("**** validation warning");
	    reportException(ex);
	}

	@Override
	public void fatalError(SAXParseException ex) throws SAXException {
	    System.out.println("**** validation fatalError");
	    reportException(ex);
	}

	private void reportException(SAXParseException ex) {
	    System.out.println(ex.getLineNumber() + ":" + ex.getColumnNumber() + " " + ex.getMessage());
	    System.out.println("Public ID: " + ex.getPublicId() + "\t" + 
			       "System ID: " + ex.getSystemId());
	    if (DEBUG) {
		ex.printStackTrace();
	    }
	}

    } // end class ActionHandler
}
