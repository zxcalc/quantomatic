package quanto.gui;

import java.awt.LayoutManager;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import quanto.core.CoreException;

/**
 * An interactive view, which is pretty much self-sufficient. Meant to be
 * held in a global table like emacs buffers.
 *
 * InteractiveViews are tied in very closely with ViewPort, but should be
 * independent of QuantoFrame, and the aim is to make them independent of
 * QuantoApp.
 *
 * A note about the design of the menu/command system:
 *
 * Each view type needs to register the actions it provides with each view
 * port.  ViewPort.createPredefinedActions() is where this is done - you
 * should create a static createActions(ViewPort) method in your subclass
 * of InteractiveView that is called from ViewPort.createPredefinedActions().
 *
 * If you want to add the command to the menu and/or toolbar, this should be
 * done in QuantoFrame.initMenuBar() (assuming you are using QuantoFrame to
 * display the ViewPort).
 *
 * Your view needs to react to commands in commandTriggered().  See
 * InteractiveGraphView for how to do this efficiently when you have a
 * lot of commands.
 */
public abstract class InteractiveView extends JPanel {

	private String title;
	private InteractiveViewManager viewManager = null;
	private ViewPort viewPort = null;

	public InteractiveView() {
		this("");
	}

	public InteractiveView(LayoutManager layout) {
		this(layout, "");
	}

	public InteractiveView(String title) {
		this.title = title;
	}

	public InteractiveView(LayoutManager layout, String title) {
		super(layout);
		this.title = title;
	}

	public InteractiveViewManager getViewManager() {
		return viewManager;
	}

	void setViewManager(InteractiveViewManager viewManager) {
		this.viewManager = viewManager;
	}

	public ViewPort getViewPort() {
		return viewPort;
	}

	void setViewPort(ViewPort viewPort) {
		if (this.viewPort != viewPort) {
			if (this.viewPort != null) {
				detached(this.viewPort);
			}
			this.viewPort = viewPort;
			if (viewPort != null) {
				attached(viewPort);
			}
		}
	}

	public void setTitle(String title) {
		String oldTitle = this.title;
		this.title = title;
		firePropertyChange("title", oldTitle, title);
	}

	public String getTitle() {
		return title;
	}

	/** 
	 * Called when this view is attached to a view port.
	 *
	 * Used to activate and deactivate menu items for example.
	 */
	abstract protected void attached(ViewPort vp);

	/** 
	 * Called when this view detached from a view port.
	 *
	 * Used to activate and deactivate menu items for example.
	 */
	abstract protected void detached(ViewPort vp);

	/**
	 * Determine if this view has a parent (i.e. is currently being displayed).
	 */
	public boolean isAttached() {
		return viewPort != null;
	}

	/**
	 * Called when view is killed to do clean-up.
	 */
	abstract public void cleanUp();

	/**
	 * Return false if there are changes that need to be saved.
	 */
	abstract public boolean isSaved();

	/**
	 * Called when view is killed to do clean-up.
	 */
	abstract public void commandTriggered(String command);

	/**
	 * Checks whether the view can be closed.
	 *
	 * Checks isSaved(), and if that returns @c false, asks the
	 * user whether they want to close the view anyway, using the
	 * message provided by getUnsavedClosingMessage().
	 *
	 * @return @c true if the view can be closed, @c false otherwise
	 */
	public boolean checkCanClose() {
		if (!isSaved()) {
			int dialogRet = JOptionPane.showConfirmDialog(this,
					getUnsavedClosingMessage(),
					"Unsaved changes", JOptionPane.YES_NO_OPTION);
			return (dialogRet == JOptionPane.YES_OPTION);
		}
		return true;
	}

	/**
	 * Provides a message asking whether to close the view, even though
	 * it has unsaved changes.
	 *
	 * @return "{view title} is unsaved.  Close anyway?"
	 */
	protected String getUnsavedClosingMessage() {
		return getTitle() + " is unsaved.  Close anyway?";
	}

	abstract public void refresh();

	/**
	 * Display an error message without getting in the way.
	 * 
	 * This is intended for situations where the problem was not directly
	 * caused by the user clicking something.
	 *
	 * @param msg  the message
	 */
	protected void errorMessage(String message) {
		// FIXME: this should be non-modal
		errorDialog(message);
	}

	/**
	 * Display an error message from the core without getting in the way.
	 *
	 * This is intended for situations where the problem was not directly
	 * caused by the user clicking something.
	 *
	 * @param msg  a short message explaining what could not be done
	 * @param ex  the exception thrown by the core
	 */
	protected void coreErrorMessage(String msg, CoreException ex) {
		// FIXME: this should be non-modal
		DetailedErrorDialog.showCoreErrorDialog(this, msg, ex);
	}

	/**
	 * Display an error message, with extra detail, without getting in the way.
	 *
	 * This is intended for situations where the problem was not directly
	 * caused by the user clicking something.
	 *
	 * @param title  a title for the dialog
	 * @param msg  a short message explaining what could not be done
	 * @param details  a more detailed message explaining why it could not be done
	 */
	protected void detailedErrorMessage(String title, String msg, String details) {
		DetailedErrorDialog.showDetailedErrorDialog(this, title, msg, details);
	}

	/**
	 * Display an error message, with extra detail, without getting in the way.
	 *
	 * This is intended for situations where the problem was not directly
	 * caused by the user clicking something.
	 *
	 * @param title  a title for the dialog
	 * @param msg  a short message explaining what could not be done
	 * @param ex  an exception detailing the error
	 */
	protected void detailedErrorMessage(String title, String msg, Throwable ex) {
		DetailedErrorDialog.showDetailedErrorDialog(this, title, msg, ex.getLocalizedMessage());
	}

	/**
	 * Display a modal error message to the user.
	 * 
	 * Consider whether errorMessage might be less annoying.
	 *
	 * @param msg  the error message
	 */
	protected void errorDialog(String msg) {
		errorDialog("Error", msg);
	}

	/**
	 * Display a modal error message from the core.
	 * 
	 * Consider whether coreErrorMessage might be less annoying.
	 *
	 * @param msg  a short message explaining what could not be done
	 * @param ex  the exception thrown by the core
	 */
	protected void coreErrorDialog(String msg, CoreException ex) {
		DetailedErrorDialog.showCoreErrorDialog(this, msg, ex);
	}

	/**
	 * Display a modal error message, with extra detail.
	 * 
	 * Consider whether detailedErrorMessage might be less annoying.
	 *
	 * @param title  a title for the dialog
	 * @param msg  a short message explaining what could not be done
	 * @param details  a more detailed message explaining why it could not be done
	 */
	protected void detailedErrorDialog(String title, String msg, String details) {
		DetailedErrorDialog.showDetailedErrorDialog(this, title, msg, details);
	}

	/**
	 * Display a modal error message, with extra detail.
	 * 
	 * Consider whether detailedErrorMessage might be less annoying.
	 *
	 * @param title  a title for the dialog
	 * @param msg  a short message explaining what could not be done
	 * @param ex  an exception detailing the error
	 */
	protected void detailedErrorDialog(String title, String msg, Throwable ex) {
		DetailedErrorDialog.showDetailedErrorDialog(this, title, msg, ex.getLocalizedMessage());
	}

	/**
	 * Display a modal error message to the user.
	 * 
	 * Consider whether errorMessage might be less annoying.
	 *
	 * @param title  a title for the message
	 * @param msg  the error message
	 */
	protected void errorDialog(String title, String msg) {
		JOptionPane.showMessageDialog(this, msg, title, JOptionPane.ERROR_MESSAGE);
	}

	protected void infoDialog(String msg) {
		JOptionPane.showMessageDialog(this, msg);
	}

	protected void infoDialog(String title, String msg) {
		JOptionPane.showMessageDialog(this, msg, title, JOptionPane.INFORMATION_MESSAGE);
	}
}
