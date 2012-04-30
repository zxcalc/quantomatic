package quanto.gui;

import java.awt.BorderLayout;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Map;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import quanto.core.Core;

public class ViewPort extends JPanel {

    private static final long serialVersionUID = -2789609872128334500L;
    private ViewPortHost host;
    private InteractiveView attachedView = null;
    private JLabel pickView = null;
    private final String arrowDown = "\u25be";
    private final InteractiveViewManager viewManager;
    private boolean showInternalNames = false;
    private ViewRenameListener viewRenameListener = new ViewRenameListener();
    private final ConsoleView console;

    private class ViewRenameListener implements PropertyChangeListener {

        public void propertyChange(PropertyChangeEvent evt) {
            if (evt.getSource() == attachedView) {
                refreshLabel();
            }
        }
    }

    /**
     * The result of closing the current view
     */
    public enum CloseResult {

        /** The view was closed, and another view has taken its place */
        Success,
        /** The closing of the view was cancelled by the user */
        Cancelled,
        /** The view was closed, but there are no more views to replace it with */
        NoMoreViews
    }

    public ViewPort(InteractiveViewManager viewManager,
            ViewPortHost host, Core core) {
        this.viewManager = viewManager;
        this.host = host;
        this.console = new ConsoleView(core.getTalker());
        setLayout(new BorderLayout());
        pickView = makeViewMenu();
        add(pickView, BorderLayout.NORTH);
    }

    public void openView(InteractiveView view) {
        host.openView(view);
    }

    public InteractiveViewManager getViewManager() {
        return viewManager;
    }

    public void setShowInternalNames(boolean showInternalNames) {
        this.showInternalNames = showInternalNames;
    }

    public boolean showInternalNames() {
        return showInternalNames;
    }

    /**
     * Attaches a view to this viewport, detaching any existing view.
     * @param view The view to attach
     * @throws ViewUnavailableException @p view is already attached to another viewport
     */
    public void attachView(InteractiveView view)
            throws ViewUnavailableException {
        if (view == attachedView) {
            return;
        }
        if (view.isAttached()) {
            throw new ViewUnavailableException();
        }

        clearPort();

        if (view.getParent() != null) {
            throw new IllegalStateException("View '" + view.getTitle() + "' is already being displayed");
        }

        add(view, BorderLayout.CENTER);
        attachedView = view;
        host.setViewAllowedToClose(view != console);
        view.setViewPort(this);
        refreshLabel();
        view.addPropertyChangeListener("title", viewRenameListener);
        view.addPropertyChangeListener("saved", viewRenameListener);
        host.attachedViewChanged(attachedView);
        validate();
        repaint();
    }
    
    private void switchToNextAvailableView() {
        InteractiveView newView = viewManager.getNextFreeView();
        if (newView == null) {
            newView = console;
        }
        attachView(newView);
    }

    public void switchToConsole() {
        attachView(console);
    }

    public void clearPort() {
        if (attachedView != null) {
            attachedView.removePropertyChangeListener("title", viewRenameListener);
            attachedView.removePropertyChangeListener("saved", viewRenameListener);
            attachedView.setViewPort(null);
            remove(attachedView);
            attachedView = null;
            host.setViewAllowedToClose(false);
            setLabel("");
        }
    }

    public CloseResult closeCurrentView() {
        if (attachedView == null) {
            throw new IllegalStateException("There is no currently attached view");
        } else {
            if (!attachedView.checkCanClose()) {
                return CloseResult.Cancelled;
            }
            InteractiveView oldView = attachedView;
            clearPort();
            viewManager.removeView(oldView);
            oldView.cleanUp();

            switchToNextAvailableView();

            return CloseResult.Success;
        }
    }

    public void setCommandEnabled(CommandManager.Command command, boolean enabled) {
        host.setCommandEnabled(command.toString(), enabled);
    }

    public void setCommandEnabled(String command, boolean enabled) {
        host.setCommandEnabled(command, enabled);
    }

    public boolean isCommandEnabled(CommandManager.Command command) {
        return host.isCommandEnabled(command.toString());
    }

    public boolean isCommandEnabled(String command) {
        return host.isCommandEnabled(command);
    }

    public void setCommandStateSelected(CommandManager.Command command, boolean selected) {
        host.setCommandStateSelected(command.toString(), selected);
    }

    public void setCommandStateSelected(String command, boolean selected) {
        host.setCommandStateSelected(command, selected);
    }

    public boolean isCommandStateSelected(CommandManager.Command command) {
        return host.isCommandStateSelected(command.toString());
    }

    public boolean isCommandStateSelected(String command) {
        return host.isCommandStateSelected(command);
    }

    public void executeCommand(String command) {
        if (attachedView != null) {
            attachedView.commandTriggered(command);
        }
    }

    private JLabel makeViewMenu() {
        final JLabel picker = new JLabel("  (no views)  " + arrowDown);
        picker.setOpaque(true);
        picker.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
        java.net.URL imgURL = getClass().getResource("/toolbarButtonGraphics/navigation/Down16.gif");
        if (imgURL != null) {
            picker.setIcon(new ImageIcon(imgURL));
        }
        final JPopupMenu viewMenu = new JPopupMenu();
        picker.addMouseListener(new MouseAdapter() {
            private JMenuItem createMenuItem(String name, final InteractiveView view) {
                JMenuItem item = new JMenuItem(name);
                item.setFont(item.getFont().deriveFont(12.0f));
                item.setEnabled(!view.isAttached());
                item.addActionListener(new ActionListener() {
                    public void actionPerformed(java.awt.event.ActionEvent e) {
                        attachView(view);
                    }
                });
                return item;
            }

            @Override
            public void mouseClicked(MouseEvent e) {
                viewMenu.removeAll();

                viewMenu.add(createMenuItem("console", console));

                Map<String, InteractiveView> views = viewManager.getViews();
                if (!views.isEmpty())
                    viewMenu.addSeparator();

                for (final Map.Entry<String, InteractiveView> ent : views.entrySet()) {
                    JMenuItem item = null;
                    String title = ent.getKey();
                    if (!ent.getValue().isSaved()) {
                        title += "*";
                    }
                    if (ent.getValue() instanceof InteractiveGraphView
                            && QuantoApp.getInstance().getPreference(
                            QuantoApp.SHOW_INTERNAL_NAMES)) {
                        title += " (" + ((InteractiveGraphView) ent.getValue()).getGraph().getCoreName() + ")";
                    }
                    viewMenu.add(createMenuItem(title, ent.getValue()));
                }
                int yoffset = picker.getHeight();
                if (picker.getBorder() != null) {
                    yoffset -= picker.getBorder().getBorderInsets(picker).top;
                }
                viewMenu.show(picker, 0, yoffset);
            }
        });
        return picker;
    }

    public InteractiveView getAttachedView() {
        return attachedView;
    }

    public void preventViewClosure() {
        host.setViewAllowedToClose(false);
    }

    private void refreshLabel() {
        if (attachedView == console) {
            setLabel("console");
        } else if (attachedView != null) {
            String name = viewManager.getViewName(attachedView);
            if (!attachedView.isSaved()) {
                name += "*";
            }
            // if the view names and graph names are out of sync, show it
            if (showInternalNames) {
                if (attachedView instanceof InteractiveGraphView) {
                    name += " (" + ((InteractiveGraphView) attachedView).getGraph().getCoreName() + ")";
                } else if (attachedView instanceof SplitGraphView) {
                    name += String.format(" (%s -> %s)",
                            ((SplitGraphView) attachedView).getLeftView().getGraph().getCoreName(),
                            ((SplitGraphView) attachedView).getRightView().getGraph().getCoreName());
                }
            }
            setLabel(name);
        }
    }

    private void setLabel(String text) {
        if (text == null) {
            text = "[null]";
        }
        pickView.setText("  " + text + " " + arrowDown);
    }
}
