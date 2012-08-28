package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.UIManager;
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
	private JPanel panelContainer;
	private List<Job> activeJobs = new LinkedList<Job>();

	public InteractiveView() {
		this("");
	}

	public InteractiveView(String title) {
		super(new BorderLayout());
		this.title = title;
		panelContainer = new JPanel();
		panelContainer.setLayout(new BoxLayout(panelContainer, BoxLayout.PAGE_AXIS));
		add(panelContainer, BorderLayout.PAGE_END);
	}
	
	protected void setMainComponent(Component c) {
		add(c, BorderLayout.CENTER);
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
				this.viewPort.setCommandEnabled(CommandManager.Command.Abort, false);
				detached(this.viewPort);
			}
			this.viewPort = viewPort;
			if (viewPort != null) {
				viewPort.setCommandEnabled(CommandManager.Command.Abort,
						!activeJobs.isEmpty());
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
	protected void attached(ViewPort vp) {}

	/** 
	 * Called when this view detached from a view port.
	 *
	 * Used to activate and deactivate menu items for example.
	 */
	protected void detached(ViewPort vp) {}

	/**
	 * Determine if this view has a parent (i.e. is currently being displayed).
	 */
	public boolean isAttached() {
		return viewPort != null;
	}

	/**
	 * Called when view is killed to do clean-up.
	 */
	public void cleanUp() {}

	/**
	 * Return false if there are changes that need to be saved.
	 */
	public boolean isSaved() { return true; }

	/**
	 * Called when view is killed to do clean-up.
	 */
	public void commandTriggered(String command) {
		if(CommandManager.Command.Abort.toString().equals(command)) {
			if (!activeJobs.isEmpty()) {
				Job[] jobs = activeJobs.toArray(new Job[activeJobs.size()]);
				for (Job job : jobs) {
					job.abortJob();
				}
			}
		}
	}

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

	public void refresh() {}

	protected static ImageIcon createImageIcon(String path) {
		java.net.URL imgURL = InteractiveView.class.getResource(path);
		if (imgURL != null) {
			return new ImageIcon(imgURL);
		} else {
			System.err.println("Couldn't find file: " + path);
			return null;
		}
	}

	private class MessagePanel extends JPanel {
		
		private JButton okButton;

		public MessagePanel(String message) {
			this(message, null, null);
		}

		public MessagePanel(String message, String details) {
			this(message, details, null);
		}

		public MessagePanel(String message, Icon icon) {
			this(message, null, icon);
		}

		public MessagePanel(String message, String details, Icon icon) {
			super(new BorderLayout());

			setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
			setBackground(UIManager.getColor("textHighlight"));

			JPanel topLine = new JPanel(new BorderLayout());
			topLine.setBackground(UIManager.getColor("textHighlight"));
			
			if (icon != null) {
				topLine.add(new JLabel(icon), BorderLayout.LINE_START);
			}
			JLabel messageLabel = new JLabel(message);
			topLine.add(messageLabel, BorderLayout.CENTER);

			okButton = new JButton("OK");
			okButton.setMargin(new Insets(0, 0, 0, 0));
			topLine.add(okButton, BorderLayout.LINE_END);
			
			if (message != null) {
				add(topLine, BorderLayout.PAGE_START);
				JLabel detailsLabel = new JLabel(details);
				add(detailsLabel, BorderLayout.CENTER);
			} else {
				add(topLine, BorderLayout.CENTER);
			}
		}
		
		public void addActionListener(ActionListener l) {
			okButton.addActionListener(l);
		}
		
		public void removeActionListener(ActionListener l) {
			okButton.removeActionListener(l);
		}
	}

	private class ErrorPanel extends MessagePanel {
		public ErrorPanel(String message) {
			super(message,UIManager.getIcon("OptionPane.errorIcon"));
		}

		public ErrorPanel(String message, String details) {
			super(message,details,UIManager.getIcon("OptionPane.errorIcon"));
			setBackground(UIManager.getColor("textHighlight"));
		}
	}

	private class WarningPanel extends MessagePanel {
		public WarningPanel(String message) {
			super(message, UIManager.getIcon("OptionPane.warningIcon"));
		}

		public WarningPanel(String message, String details) {
			super(message,details,UIManager.getIcon("OptionPane.warningIcon"));
			setBackground(UIManager.getColor("textHighlight"));
		}
	}

	// FIXME: timeout?
	private class InfoPanel extends MessagePanel {
		public InfoPanel(String message) {
			super(message, UIManager.getIcon("OptionPane.informationIcon"));
		}

		public InfoPanel(String message, String details) {
			super(message,details,UIManager.getIcon("OptionPane.informationIcon"));
			setBackground(UIManager.getColor("textHighlight"));
		}
	}
	
	private void pushMessagePanel(final MessagePanel p) {
		panelContainer.add(p);
		p.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				panelContainer.remove(p);
				InteractiveView.this.validate();
			}
		});
		this.validate();
	}

	private static class JobIndicatorPanel extends JPanel {

		private JLabel textLabel;
		private JButton cancelButton = null;

		public JobIndicatorPanel(String description, final Job job) {
			super(new BorderLayout());

			setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
			setBackground(UIManager.getColor("textHighlight"));

			textLabel = new JLabel(description);
			add(textLabel, BorderLayout.CENTER);

			cancelButton = new JButton(createImageIcon("/toolbarButtonGraphics/general/Stop16.gif"));
			cancelButton.setToolTipText("Abort this operation");
			cancelButton.setMargin(new Insets(0, 0, 0, 0));
			cancelButton.addActionListener(new ActionListener() {

				public void actionPerformed(ActionEvent e) {
					job.abortJob();
				}
			});
			add(cancelButton, BorderLayout.LINE_END);
		}
	}

	/**
	 * Registers a job, allowing it to be aborted by the "Abort all"
	 * action.
	 *
	 * Should not be called for a job if showJobIndicator() is called
	 * for that job.
	 * @param job  The job to register
	 */
	protected void registerJob(final Job job) {
		activeJobs.add(job);
		if (getViewPort() != null) {
			getViewPort().setCommandEnabled(CommandManager.Command.Abort, true);
		}
		job.addJobListener(new JobListener() {

			public void jobEnded(JobEndEvent event) {
				activeJobs.remove(job);
				if (activeJobs.isEmpty() && getViewPort() != null) {
					getViewPort().setCommandEnabled(CommandManager.Command.Abort, false);
				}
			}
		});
	}

	/**
	 * Shows an indicator at the bottom of the view with
	 * a button to cancel the job.
	 *
	 * @param jobDescription  The text on the indicator
	 * @param job             The job
	 */
	protected void showJobIndicator(String jobDescription, Job job) {
		registerJob(job);
		final JobIndicatorPanel indicator = new JobIndicatorPanel(jobDescription, job);
		panelContainer.add(indicator);
		job.addJobListener(new JobListener() {

			public void jobEnded(JobEndEvent event) {
				panelContainer.remove(indicator);
				InteractiveView.this.validate();
			}
		});
		this.validate();
	}

	/**
	 * Display an informational message without getting in the way.
	 *
	 * @param msg  the message (should be short)
	 */
	protected void infoMessage(String message) {
		final InfoPanel p = new InfoPanel(message);
		pushMessagePanel(p);
	}

	/**
	 * Display an informational message without getting in the way.
	 *
	 * @param msg  the message (should be short)
	 * @param details  a more detailed message
	 */
	protected void detailedInfoMessage(String message, String details) {
		final InfoPanel p = new InfoPanel(message, details);
		pushMessagePanel(p);
	}

	/**
	 * Display an warning message without getting in the way.
	 *
	 * @param msg  the message (should be short)
	 */
	protected void warningMessage(String message) {
		final WarningPanel p = new WarningPanel(message);
		pushMessagePanel(p);
	}

	/**
	 * Display an warning message without getting in the way.
	 *
	 * @param msg  the message (should be short)
	 * @param details  a more detailed message
	 */
	protected void detailedWarningMessage(String message, String details) {
		final WarningPanel p = new WarningPanel(message, details);
		pushMessagePanel(p);
	}

	/**
	 * Display an error message without getting in the way.
	 * 
	 * This is intended for situations where the problem was not directly
	 * caused by the user clicking something.
	 *
	 * @param msg  the message (should be short)
	 */
	protected void errorMessage(String message) {
		final ErrorPanel errorPanel = new ErrorPanel(message);
		pushMessagePanel(errorPanel);
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
		detailedErrorMessage(msg, ex);
	}

	/**
	 * Display an error message, with extra detail, without getting in the way.
	 *
	 * This is intended for situations where the problem was not directly
	 * caused by the user clicking something.
	 *
	 * @param msg  a short message explaining what could not be done
	 * @param details  a more detailed message explaining why it could not be done
	 */
	protected void detailedErrorMessage(String msg, String details) {
		final ErrorPanel errorPanel = new ErrorPanel(msg, details);
		pushMessagePanel(errorPanel);
	}

	/**
	 * Display an error message, with extra detail, without getting in the way.
	 *
	 * This is intended for situations where the problem was not directly
	 * caused by the user clicking something.
	 *
	 * @param msg  a short message explaining what could not be done
	 * @param ex  an exception detailing the error
	 */
	protected void detailedErrorMessage(String msg, Throwable ex) {
		detailedErrorMessage(msg, ex.getLocalizedMessage());
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
