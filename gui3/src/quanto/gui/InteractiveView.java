package quanto.gui;

import java.util.List;

import javax.swing.JMenu;

/**
 * An interactive view port, which is pretty much self-sufficient. Implementors should
 * extend a JPanel-like component.
 */
public interface InteractiveView {
	/**
	 * Get a list of view-specific JMenu's.
	 * @return
	 */
	List<JMenu> getMenus();
	
	/**
	 * Titles should be unique, as they may be used to address a view.
	 */
	String getTitle();
	

	/** 
	 * Called when this view gains focus.  Used to activate and deactivate menu items for example.
	 */
	public void gainFocus();
	
	/** 
	 * Called when this view loses focus.  Used to activate and deactivate menu items for example.
	 */
	public void loseFocus();
	
	/** 
	 * Determine if this view has a parent (i.e. is currently being displayed).
	 */
	public boolean hasParent();
}
