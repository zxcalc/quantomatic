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
	 * If an InteractiveView wishes to spawn other views, add them to the given view holder.
	 * The expected behaviour if this is not called is that new views go into their own window.
	 * @param h
	 */
	void setViewHolder(Holder h);
	
	public interface Holder {
		void addView(InteractiveView v);
	}
}
