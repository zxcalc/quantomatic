package quanto.gui;

/**
 * An interactive view port, which is pretty much self-sufficient. Meant to be held in
 * a global table like emacs buffers.
 */
public interface InteractiveView {
	/** 
	 * Called when this view gains focus.  Used to activate and deactivate menu items for example.
	 */
	void gainFocus(ViewPort vp);
	
	/** 
	 * Called when this view loses focus.  Used to activate and deactivate menu items for example.
	 */
	void loseFocus(ViewPort vp);
	
	/** 
	 * Determine if this view has a parent (i.e. is currently being displayed).
	 */
	boolean hasParent();
}
