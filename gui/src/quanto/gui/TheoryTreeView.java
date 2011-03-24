/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui;

import quanto.core.Theory;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTree;
import javax.swing.border.EtchedBorder;
import javax.swing.filechooser.FileFilter;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreePath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import quanto.core.CoreTalker;
import quanto.core.CoreException;

/**
 *
 * @author alex
 */
public class TheoryTreeView extends JPanel {

	private final static Logger logger =
		LoggerFactory.getLogger(TheoryTreeView.class);

	TheoryManager manager;
	ViewPort viewPort;
	JFileChooser chooser;
	JPopupMenu rootMenu = new RootMenu();
	JTree tree;

	public TheoryTreeView(TheoryManager manager, ViewPort viewPort) {
		super(new BorderLayout());
		this.manager = manager;
		this.viewPort = viewPort;

		this.tree = new JTree(manager.getTreeModel());
		tree.setRootVisible(false);
		tree.setShowsRootHandles(true);
		tree.setCellRenderer(new TheoryCellRenderer());
		tree.addMouseListener(new MouseAdapter() {
			private boolean isRightClick(MouseEvent e) {
				return (e.getButton() == MouseEvent.BUTTON3) ||
					(QuantoApp.isMac &&
					 e.isControlDown() &&
					 e.getButton() == MouseEvent.BUTTON1);
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				final TheoryManager.TheoryTreeModel tmodel = TheoryTreeView.this.manager.getTreeModel();
				if (isRightClick(e)) {
					TreePath p = tree.getPathForLocation(e.getX(), e.getY());
					if (p != null) {
						Object node = p.getLastPathComponent();
						if (node == tmodel.getRoot()) {
							rootMenu.show(tree, e.getX(), e.getY());
						} else if (tmodel.isTheoryNode(node)) {
							Theory rset = tmodel.getTheory(node);
							new TheoryMenu(rset).show(tree, e.getX(), e.getY());
						} else if (tmodel.isRuleNode(node)) {
							Theory rset = tmodel.getTheory(node);
							String rule = tmodel.getRuleName(node);
							new RuleMenu(rset, rule).show(tree, e.getX(), e.getY());
						}
					}
				}
			}
		});
		add(tree, BorderLayout.CENTER);

		final JLabel titleBar = new JLabel("Theories");
		java.net.URL imgURL = getClass().getResource("/toolbarButtonGraphics/navigation/Down16.gif");
		if (imgURL != null)
			titleBar.setIcon(new ImageIcon(imgURL));
		titleBar.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		titleBar.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				rootMenu.show(titleBar, 0, titleBar.getHeight() - titleBar.getBorder().getBorderInsets(titleBar).bottom);
			}
		});
		add(titleBar, BorderLayout.PAGE_START);

		// don't want to steal keyboard focus from the active InteractiveView
		setFocusable(false);
	}

	private JFileChooser getFileChooser()
	{
		if ( chooser == null )
		{
			chooser = new JFileChooser();
			chooser.addChoosableFileFilter(new FileFilter() {
				private String getExtension(File f) {
					String ext = null;
					String s = f.getName();
					int i = s.lastIndexOf('.');

					if (i > 0 &&  i < s.length() - 1) {
						ext = s.substring(i+1).toLowerCase();
					}
					return ext;
				}

				@Override
				public boolean accept(File f) {
					if (f.isDirectory())
						return true;
					return getExtension(f).equals("theory");
				}

				@Override
				public String getDescription() {
					return "Theory files (*.theory)";
				}
			});
		}
		return chooser;
	}

	private File askForTheoryToOpen() {
		JFileChooser fileChooser = getFileChooser();
		File lastDir = manager.getLastTheoryDirectory();
		if (lastDir != null) {
			chooser.setCurrentDirectory(lastDir);
		}
		int retVal = fileChooser.showOpenDialog(this);
		if (retVal == JFileChooser.APPROVE_OPTION) {
			return fileChooser.getSelectedFile();
		}
		return null;
	}

	private File askForSaveLocation( Theory theory ) {
		JFileChooser fileChooser = getFileChooser();
		if (theory.getPath() != null && theory.getPath().length() != 0)
		{
			fileChooser.setSelectedFile(new File(theory.getPath()));
		}
		else if (manager.getLastTheoryDirectory() != null)
		{
			File file = new File(manager.getLastTheoryDirectory(),
				theory.getName() + ".theory");
			fileChooser.setSelectedFile(file);
		}
		else
		{
			File file = new File(theory.getName() + ".theory");
			fileChooser.setSelectedFile(file);
		}
		int retVal = fileChooser.showSaveDialog(this);
		if (retVal == JFileChooser.APPROVE_OPTION) {
			return fileChooser.getSelectedFile();
		}
		return null;
	}

	public void loadTheory() {
		File file = askForTheoryToOpen();
		if (file != null) {
			try {
				String thyname = file.getName().replaceAll("\\.theory|\\n|\\r", "");
				String filename = file.getCanonicalPath().replaceAll("\\n|\\r", "");
				manager.loadTheory(thyname, filename);
			}
			catch (CoreException e) {
				logger.error("Failed to load theory", e);
				errorDialog(e.getMessage());
			}
			catch (java.io.IOException ioe) {
				logger.error("Failed to load theory", ioe);
				errorDialog(ioe.getMessage());
			}
		}
	}

	@SuppressWarnings("serial")
	private class TheoryCellRenderer extends DefaultTreeCellRenderer {
		@Override
		public Component getTreeCellRendererComponent(JTree tree, Object value,
				boolean selected, boolean expanded, boolean leaf, int row,
				boolean hasFocus) {
			// let parent set the basic component properties
			super.getTreeCellRendererComponent(tree, value, selected, expanded,
					leaf, row, hasFocus);

			// ghost the theory if it isn't active
			Theory th = manager.getTreeModel().getTheory(value);
			if (th != null) {
				if (!th.isActive())
					setForeground(Color.gray);

				if (th.hasUnsavedChanges() &&
					manager.getTreeModel().isTheoryNode(value)) {
					setText(getText() + " [*]");
				}
			}

			return this;
		}
	}

	/**
	 * Generic action listener that reports core errors to a dialog box.
	 */
	private abstract class WrappedActionListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			try {
				wrappedAction(e);
			}
			catch (CoreException err) {
				JOptionPane.showMessageDialog(
					TheoryTreeView.this,
					err.getMessage(),
					"Console Error",
					JOptionPane.ERROR_MESSAGE);
			}
		}

		public abstract void wrappedAction(ActionEvent e) throws CoreException;
	}

	private class RootMenu extends JPopupMenu {
		public RootMenu() {
			JMenuItem item;

			item = new JMenuItem("New theory");
			item.addActionListener(new WrappedActionListener() {
				public void wrappedAction(ActionEvent e) throws CoreException {
					TheoryTreeView.this.manager.createNewTheory();
				}
			});
			add(item);

			item = new JMenuItem("Load theory...");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					loadTheory();
				}
			});
			add(item);

			addSeparator();

			item = new JMenuItem("Refresh");
			item.addActionListener(new WrappedActionListener() {
				@Override
				public void wrappedAction(ActionEvent e) throws CoreException {
					TheoryTreeView.this.manager.reloadTheoriesFromCore();
				}
			});
			add(item);
		}
	};

	public void errorDialog(Object message) {
		JOptionPane.showMessageDialog(this, message, "Error", JOptionPane.ERROR_MESSAGE);
	}

	public void errorDialog(String title, Object message) {
		JOptionPane.showMessageDialog(this, message, title, JOptionPane.ERROR_MESSAGE);
	}

	@SuppressWarnings("serial")
	private class TheoryMenu extends JPopupMenu {
		public TheoryMenu(final Theory rset) {
			JMenuItem item;

			item = new JMenuItem("New rule");
			item.addActionListener(new WrappedActionListener() {
				public void wrappedAction(ActionEvent e) throws CoreException {
					String rule = rset.addRule();
					InteractiveView view = new SplitGraphView(rset, rset.getRule(rule));

					viewPort.getViewManager().addView(view);
					try {
						viewPort.attachView(view);
					}
					catch (ViewUnavailableException ex) {
						throw new Error("Caught a ViewUnavailableException: this shouldn't happen");
					}

				}
			});
			add(item);

			item = new JMenuItem("Save to file...");
			item.addActionListener(new WrappedActionListener() {
				public void wrappedAction(ActionEvent e) throws CoreException {
					File file = askForSaveLocation(rset);
					if (file != null) {
						try {
							String filename = file.getCanonicalPath().replaceAll("\\n|\\r", "");
							rset.save(filename);
						}
						catch (java.io.IOException ioe) {
							logger.error("Failed to save theory " + rset.getName(), ioe);
							errorDialog(ioe.getMessage());
						}
					}
				}
			});
			add(item);

			item = new JMenuItem("Rename...");
			item.addActionListener(new WrappedActionListener() {
				public void wrappedAction(ActionEvent e) throws CoreException {
					String newName = JOptionPane.showInputDialog(
						TheoryTreeView.this,
						"Please enter a new name for the theory",
						rset.getName());
					if (newName != null && !newName.equals(rset.getName())) {
						rset.renameTheory(newName);
					}
				}
			});
			add(item);

			addSeparator();

			item = new JMenuItem("Refresh");
			item.addActionListener(new WrappedActionListener() {
				public void wrappedAction(ActionEvent e) throws CoreException {
					rset.refreshRules();
				}
			});
			add(item);

			addSeparator();

			final boolean active = rset.isActive();
			item = new JMenuItem(active ? "Deactivate" : "Activate");
			item.addActionListener(new WrappedActionListener() {
				public void wrappedAction(ActionEvent e) throws CoreException {
					manager.setTheoryActive(rset, !active);
				}
			});
			add(item);

			item = new JMenuItem("Unload");
			item.addActionListener(new WrappedActionListener() {
				public void wrappedAction(ActionEvent e) throws CoreException {
					if (rset.hasUnsavedChanges()) {
						int answer = JOptionPane.showConfirmDialog(TheoryTreeView.this,
							"There are unsaved changes to this theory.  Are you sure you want to discard them?",
							"Unsaved changes",
							JOptionPane.YES_NO_OPTION);
						if (answer != JOptionPane.YES_OPTION)
							return;
					}
					manager.unloadTheory(rset);
				}
			});
			add(item);
		}
	}



	private enum Side {
		Left,
		Right,
		Both
	};
	@SuppressWarnings("serial")
	private class RuleMenu extends JPopupMenu {
		public RuleMenu(final Theory rset, final String rule) {
			JMenuItem item;

			class RuleAL extends WrappedActionListener {
				private Side side; // BOTH = 0, LEFT = 1, RIGHT = 2
				public RuleAL(Side side) { super(); this.side = side; }

				public void wrappedAction(ActionEvent e) throws CoreException {
					InteractiveView view;
					if (side == Side.Left) {
						InteractiveGraphView igv = new InteractiveGraphView(
							rset.getCore(), rset.getRuleLhs(rule));
						igv.updateGraph();
						view = igv;
					} else if (side == Side.Right) {
						InteractiveGraphView igv = new InteractiveGraphView(
							rset.getCore(), rset.getRuleRhs(rule));
						igv.updateGraph();
						view = igv;
					} else {
						view = new SplitGraphView(rset, rule);
					}
					viewPort.getViewManager().addView(view);
					try {
						viewPort.attachView(view);
					}
					catch (ViewUnavailableException ex) {
						throw new Error("Caught a ViewUnavailableException: this shouldn't happen");
					}
				}
			}

			item = new JMenuItem("Open Rule");
			item.addActionListener(new RuleAL(Side.Both));
			add(item);
			item = new JMenuItem("Open LHS");
			item.addActionListener(new RuleAL(Side.Left));
			add(item);
			item = new JMenuItem("Open RHS");
			item.addActionListener(new RuleAL(Side.Right));
			add(item);

			addSeparator();

			item = new JMenuItem("Rename...");
			item.addActionListener(new WrappedActionListener() {
				@Override
				public void wrappedAction(ActionEvent e) throws CoreException {
					String newName = JOptionPane.showInputDialog(
						TheoryTreeView.this,
						"Please enter a new name for the rule",
						rule);
					if (newName != null && !newName.equals(rule)) {
						rset.renameRule(rule, newName);
					}
				}
			});
			add(item);

			addSeparator();

			item = new JMenuItem("Delete");
			item.addActionListener(new WrappedActionListener() {
				@Override
				public void wrappedAction(ActionEvent e) throws CoreException {
					int answer = JOptionPane.showConfirmDialog(
						TheoryTreeView.this,
						String.format("Are you sure you want to delete the rule '%1$s'?", rule),
						"Delete rule",
						JOptionPane.YES_NO_OPTION);
					if (answer == JOptionPane.YES_OPTION) {
						rset.deleteRule(rule);
					}
				}
			});
			add(item);

		}
	}
}
