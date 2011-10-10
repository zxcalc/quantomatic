package quanto.gui;

import com.sun.jaf.ui.ActionManager;
import com.sun.jaf.ui.UIFactory;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Image;
import java.awt.event.WindowEvent;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JSplitPane;
import org.xml.sax.SAXException;

import quanto.core.CoreException;
import quanto.core.xml.TheoryParser;
import quanto.gui.QuantoApp.BoolPref;

public class QuantoFrame extends JFrame implements ViewPortHost {

    private final static Logger logger = Logger.getLogger("quanto.gui");
    private static final long serialVersionUID = 3656684775223085393L;
    private final ViewPort viewPort;
    private LeftTabbedPane sidebar;
    private JSplitPane splitPane;
    private volatile static int frameCount = 0;
    private QuantoApp app;
    private ActionManager actionManager = new ActionManager();
    // these all taken from resources/actions.xml
    public final static String NEW_WINDOW_COMMAND = "new-win-command";
    public final static String NEW_GRAPH_COMMAND = "new-graph-command";
    public final static String OPEN_GRAPH_COMMAND = "open-command";
    public final static String LOAD_RULESET_COMMAND = "load-ruleset-command";
    public final static String SAVE_RULESET_COMMAND = "save-ruleset-command";
    public final static String CLOSE_COMMAND = "close-command";
    public final static String QUIT_COMMAND = "quit-command";
    public final static String REFRESH_ALL_COMMAND = "refresh-all-graphs-command";
    public final static String DRAW_ARROW_HEADS_COMMAND = "draw-arrow-heads-command";
    public final static String SHOW_INTERNAL_GRAPH_NAMES_COMMAND = "internal-graph-names-command";
    public final static String OPEN_IN_NEW_WINDOW_COMMAND = "open-in-new-window-command";
    public final static String LOAD_THEORY_COMMAND = "load-theory-command";
    // This type has to be public in order to be registered as a
    // handler with ActionManager.  The constructor is private, however,
    // to prevent abuse.

    public class Delegate {

        private Delegate() {
        }

        public void executeCommand(String command) {
            viewPort.executeCommand(command);
        }

        public void executeCommand(String command, boolean state) {
            if (state) {
                viewPort.executeCommand(command);
            }
        }
    }
    // This type has to be public in order to be registered as a
    // handler with ActionManager.  The constructor is private, however,
    // to prevent abuse.

    public class BoolPrefDelegate {

        private final QuantoApp.BoolPref pref;

        private BoolPrefDelegate(BoolPref pref) {
            this.pref = pref;
        }

        public void setState(boolean state) {
            app.setPreference(pref, state);
        }
    }

    private void addIconFromRes(List<Image> to, String resourceRef) {
        try {
            to.add(ImageIO.read(getClass().getResource(resourceRef)));
        } catch (IOException ex) {
            logger.log(Level.WARNING, "Cannot find " + resourceRef, ex);
        }
    }

    private void loadIcons() {
        List<Image> icons = new ArrayList<Image>(6);
        addIconFromRes(icons, "/icons/quanto_icon_16.png");
        addIconFromRes(icons, "/icons/quanto_icon_24.png");
        addIconFromRes(icons, "/icons/quanto_icon_32.png");
        addIconFromRes(icons, "/icons/quanto_icon_48.png");
        addIconFromRes(icons, "/icons/quanto_icon_64.png");
        addIconFromRes(icons, "/icons/quanto_icon_128.png");
        setIconImages(icons);
    }

    public QuantoFrame(QuantoApp app) {
        super("Quantomatic");

        loadIcons();

        frameCount++;
        this.app = app;
        setBackground(Color.white);
        getContentPane().setLayout(new BorderLayout());

        actionManager.setControlConvertedToMeta(QuantoApp.isMac);
        URL actionsXml = getClass().getResource("resources/actions.xml");
        if (actionsXml == null) {
            throw new Error("Could not find resource \"resources/actions.xml\"");
        }
        try {
            actionManager.loadActions(actionsXml);
        } catch (IOException ex) {
            throw new Error("Could not load resource \"resources/actions.xml\": " + ex.getMessage());
        }
        Set<String> menuIds = actionManager.getActionListIDs();
        for (String id : actionManager.getActionIDs()) {
            if (!menuIds.contains(id)) {
                actionManager.setEnabled(id, false);
            }
        }
        actionManager.registerCallback(NEW_WINDOW_COMMAND, app, "createNewFrame");
        actionManager.setEnabled(NEW_WINDOW_COMMAND, true);

        actionManager.registerCallback(NEW_GRAPH_COMMAND, this, "createNewGraph");
        actionManager.setEnabled(NEW_GRAPH_COMMAND, true);

        actionManager.registerCallback(OPEN_GRAPH_COMMAND, this, "openGraph");
        actionManager.setEnabled(OPEN_GRAPH_COMMAND, true);

        actionManager.registerCallback(CLOSE_COMMAND, this, "closeCurrentView");
        actionManager.setEnabled(CLOSE_COMMAND, true);

        actionManager.registerCallback(LOAD_RULESET_COMMAND, this, "importRuleset");
        actionManager.setEnabled(LOAD_RULESET_COMMAND, true);

        actionManager.registerCallback(SAVE_RULESET_COMMAND, this, "exportRuleset");
        actionManager.setEnabled(SAVE_RULESET_COMMAND, true);

        actionManager.registerCallback(QUIT_COMMAND, app, "shutdown");
        actionManager.setEnabled(QUIT_COMMAND, true);

        actionManager.registerCallback(REFRESH_ALL_COMMAND, app.getViewManager(), "refreshAll");
        actionManager.setEnabled(REFRESH_ALL_COMMAND, true);

        actionManager.registerCallback(LOAD_THEORY_COMMAND, this, "openTheory");
        actionManager.setEnabled(LOAD_THEORY_COMMAND, true);

        actionManager.registerCallback(DRAW_ARROW_HEADS_COMMAND,
                new BoolPrefDelegate(QuantoApp.DRAW_ARROW_HEADS),
                "setState");
        actionManager.setEnabled(DRAW_ARROW_HEADS_COMMAND, true);
        actionManager.setSelected(DRAW_ARROW_HEADS_COMMAND,
                app.getPreference(QuantoApp.DRAW_ARROW_HEADS));

        actionManager.registerCallback(SHOW_INTERNAL_GRAPH_NAMES_COMMAND,
                new BoolPrefDelegate(QuantoApp.SHOW_INTERNAL_NAMES),
                "setState");
        actionManager.setEnabled(SHOW_INTERNAL_GRAPH_NAMES_COMMAND, true);
        actionManager.setSelected(SHOW_INTERNAL_GRAPH_NAMES_COMMAND,
                app.getPreference(QuantoApp.SHOW_INTERNAL_NAMES));

        actionManager.registerCallback(OPEN_IN_NEW_WINDOW_COMMAND,
                new BoolPrefDelegate(QuantoApp.NEW_WINDOW_FOR_GRAPHS),
                "setState");
        actionManager.setEnabled(OPEN_IN_NEW_WINDOW_COMMAND, true);
        actionManager.setSelected(OPEN_IN_NEW_WINDOW_COMMAND,
                app.getPreference(QuantoApp.NEW_WINDOW_FOR_GRAPHS));

        UIFactory factory = new UIFactory(actionManager);
        setJMenuBar(factory.createMenuBar("main-menu"));
        if (QuantoApp.MAC_OS_X) {
            removeQuitFromFileMenu();
        }
        getContentPane().add(factory.createToolBar("main-toolbar"), BorderLayout.PAGE_START);

        viewPort = new ViewPort(app.getViewManager(), this);
        sidebar = new LeftTabbedPane(app.getCore(), this);

        Delegate delegate = new Delegate();
        actionManager.registerGenericCallback(
                ViewPort.getKnownCommands(),
                delegate, "executeCommand");

        //Add the scroll panes to a split pane.
        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitPane.setLeftComponent(sidebar);
        splitPane.setRightComponent(viewPort);
        splitPane.setDividerLocation(150);
        splitPane.setOneTouchExpandable(true);

        getContentPane().add(splitPane, BorderLayout.CENTER);
        this.pack();
    }

    private void removeQuitFromFileMenu() {
        JMenuBar menuBar = getJMenuBar();
        Action fileMenuAction = actionManager.getAction("file-menu");
        Action quitCommandAction = actionManager.getAction(QUIT_COMMAND);
        for (int i = 0; i < menuBar.getMenuCount(); ++i) {
            JMenu menu = menuBar.getMenu(i);
            if (menu != null && menu.getAction() == fileMenuAction) {
                for (int j = menu.getItemCount() - 1; j >= 0; --j) {
                    JMenuItem item = menu.getItem(j);
                    if (item != null && item.getAction() == quitCommandAction) {
                        menu.remove(j);
                        return;
                    }
                }
                return;
            }
        }
    }

    public void openView(InteractiveView view) {
        if (app.getPreference(QuantoApp.NEW_WINDOW_FOR_GRAPHS)) {
            app.openNewFrame(view);
        } else {
            viewPort.attachView(view);
        }
    }

    public void closeCurrentView() {
        ViewPort.CloseResult result = viewPort.closeCurrentView();
        if (result == ViewPort.CloseResult.NoMoreViews) {
            dispose();
        }
    }

    public void createNewGraph() {
        try {
            openView(app.createNewGraph());
        } catch (CoreException ex) {
            app.errorDialog("Could not create new graph: " + ex.getMessage());
        }
    }

    public void importRuleset() {
        File f = app.openFile(this, "Import ruleset", app.DIR_RULESET);
        try {
            if (f != null) {
                app.getCore().loadRuleset(f);
            }
        } catch (CoreException e) {
            app.errorDialog("Error in core when opening \"" + f.getName() + "\": " + e.getMessage());
        } catch (java.io.IOException e) {
            app.errorDialog("Could not read \"" + f.getName() + "\": " + e.getMessage());
        }
    }

    public void exportRuleset() {
        File f = app.saveFile(this, "Export ruleset", app.DIR_RULESET);
        try {
            if (f != null) {
                app.getCore().saveRuleset(f);
            }
        } catch (CoreException e) {
            app.errorDialog("Error in core when opening \"" + f.getName() + "\": " + e.getMessage());
        } catch (java.io.IOException e) {
            app.errorDialog("Could not read \"" + f.getName() + "\": " + e.getMessage());
        }
    }

    /**
     * Read a graph from a file and send it to a fresh InteractiveGraphView.
     */
    public void openGraph() {
        File f = app.openFile(this);
        try {
            if (f != null) {
                InteractiveView view = app.openGraph(f);
                openView(view);
            }
        } catch (CoreException e) {
            app.errorDialog("Error in core when opening \"" + f.getName() + "\": " + e.getMessage());
        } catch (java.io.IOException e) {
            app.errorDialog("Could not read \"" + f.getName() + "\": " + e.getMessage());
        }
    }

    public void openTheory() {
        File f = app.openFile(this, "Select theory file", QuantoApp.DIR_THEORY);
        if (f != null) {
            try {
                TheoryParser theoryParser = new TheoryParser(f.getAbsolutePath());
                app.updateCoreTheory(theoryParser.getImplementedTheoryName(), theoryParser.getTheoryVertices());
                app.setPreference(quanto.gui.QuantoApp.LAST_THEORY_OPEN_FILE, f.getAbsolutePath());
                // FIXME: this isn't right...
                //Open a new graph as well...
                app.createNewFrame();
            this.closeCurrentView();
            } catch (SAXException e) {
                app.errorDialog(e.toString());
            } catch (IOException e) {
                app.errorDialog(e.toString());
            } catch (CoreException e) {
                app.errorDialog(e.toString());
            }
        }
    }

    @Override
    protected void processWindowEvent(WindowEvent e) {
        if (e.getID() == WindowEvent.WINDOW_CLOSING) {
            if (frameCount == 1) {
                app.shutdown();
            } else {
                frameCount--;
                viewPort.detachView();
                dispose();
            }
        } else {
            super.processWindowEvent(e);
        }
    }

    public ViewPort getViewPort() {
        return viewPort;
    }

    public void setViewAllowedToClose(boolean allowed) {
        actionManager.setEnabled(CLOSE_COMMAND, allowed);
    }

    public boolean isViewAllowedToClose() {
        return actionManager.isEnabled(CLOSE_COMMAND);
    }

    public void setCommandEnabled(String command, boolean enabled) {
        actionManager.setEnabled(command, enabled);
    }

    public boolean isCommandEnabled(String command) {
        return actionManager.isEnabled(command);
    }

    public void setCommandStateSelected(String command, boolean selected) {
        actionManager.setSelected(command, selected);
    }

    public boolean isCommandStateSelected(String command) {
        return actionManager.isSelected(command);
    }

    public void attachedViewChanged(InteractiveView newView) {
        if (newView == null) {
            setTitle("Quantomatic");
        } else {
            setTitle("Quantomatic: " + newView.getTitle());
        }
    }
}
