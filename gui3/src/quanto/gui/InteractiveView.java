package quanto.gui;

/**
 * An interactive view port, which is pretty much self-sufficient. Meant to be held in
 * a global table like emacs buffers.
 */
public interface InteractiveView {
	/** 
	 * Called when this view gains focus.  Used to activate and deactivate menu items for example.
	 */
	void viewFocus(ViewPort vp);
	
	/** 
	 * Called when this view loses focus.  Used to activate and deactivate menu items for example.
	 */
	void viewUnfocus(ViewPort vp);
	
	/**
	 * Called when view is killed to do clean-up.
	 */
	void viewKill(ViewPort vp);
	
	/** 
	 * Determine if this view has a parent (i.e. is currently being displayed).
	 */
	boolean viewHasParent();
}
