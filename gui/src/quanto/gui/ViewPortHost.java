/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui;

/**
 * The interface ViewPort expects its host to provide.
 *
 * @author alex
 */
public interface ViewPortHost {
	/**
	 * Opens the selected view.
	 *
	 * This may be in a new window, or in the same view port
	 *
	 * @param view The view to open
	 */
	void openView(InteractiveView view);

	/**
	 * ViewPort can use this to prevent the host from closing the
	 * current view.
	 *
	 * This may disable a menu item, for example.
	 *
	 * @param allowed  Whether the current view can be closed.
	 */
	void setViewAllowedToClose(boolean allowed);

	void setCommandEnabled(String command, boolean enabled);
	boolean isCommandEnabled(String command);
	void setCommandStateSelected(String command, boolean selected);
	boolean isCommandStateSelected(String command);
}
