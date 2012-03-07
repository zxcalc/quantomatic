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
    /**
     * Command actions that are dealt with directly by the frame
     * 
     * These are "global" command (as opposed to toggle, for example) actions,
     * such as Open or Quit.  Each should be dealt with by a separate
     * (non-static) method in this class.
     */
    public enum CommandAction
    {
        NewWindow("new-win-command", "createNewFrame"),
        NewGraph("new-graph-command", "createNewGraph"),
        OpenGraph("open-command", "openGraph"),
        LoadTheory("load-theory-command", "openTheory"),
        LoadRuleset("load-ruleset-command", "importRuleset"),
        SaveRuleset("save-ruleset-command", "exportRuleset"),
        Close("close-command", "closeCurrentView"),
        Quit("quit-command", "quit"),
        RefreshAll("refresh-all-command", "refreshall");

        /**
         * Create a new command action
         * @param actionName  The action name (as in resources/actions.xml)
         * @param methodName  The name of the method (in the QuantoFrame class)
         *                     to invoke when the action is triggered
         */
        private CommandAction(String actionName, String methodName) {
            this.actionName = actionName;
            this.methodName = methodName;
        }
        private final String actionName;
        private final String methodName;
        @Override
        public String toString() {
            return actionName;
        }
        public String actionName() {
            return actionName;
        }
        public String methodName() {
            return methodName;
        }
    }
    /**
     * An action that toggles a boolean preference
     */
    public enum BoolPrefAction
    {
        DrawArrowHeads("draw-arrow-heads-command", QuantoApp.DRAW_ARROW_HEADS),
        ShowInternalGraphNames("internal-graph-names-command", QuantoApp.SHOW_INTERNAL_NAMES),
        OpenInNewWindow("open-in-new-window-command", QuantoApp.NEW_WINDOW_FOR_GRAPHS);

        /**
         * Create a boolean preference action
         * 
         * @param commandValue  The action name (as in resources/actions.xml)
         * @param pref  The boolean preference object (as in QuantoApp)
         */
        private BoolPrefAction(String actionName, BoolPref pref) {
            this.actionName = actionName;
            this.pref = pref;
        }
        private final String actionName;
        private final BoolPref pref;
        @Override
        public String toString() {
            return actionName;
        }
        public String actionName() {
            return actionName;
        }
        public BoolPref getPref() {
            return pref;
        }
    }

    // This type has to be public in order to be registered as a
    // handler with ActionManager.  The constructor is private, however,
    // to prevent abuse.
	private Set<String> knownCommands;

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

    public void quit() {
        app.shutdown();
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

        for (CommandAction action: CommandAction.values()) {
            actionManager.registerCallback(action.actionName(), this, action.methodName());
            actionManager.setEnabled(action.actionName(), true);
        }
        for (BoolPrefAction action: BoolPrefAction.values()) {
            actionManager.registerCallback(action.actionName(),
                    new BoolPrefDelegate(action.getPref()),
                    "setState");
            actionManager.setEnabled(action.actionName(), true);
            actionManager.setSelected(action.actionName(),
                    app.getPreference(action.getPref()));
        }
        CommandManager commandManager = new CommandManager(actionManager);
        InteractiveGraphView.registerKnownCommands(app.getCore(), commandManager);

        UIFactory factory = new UIFactory(actionManager);
        setJMenuBar(factory.createMenuBar("main-menu"));
        if (QuantoApp.MAC_OS_X) {
            removeQuitFromFileMenu();
        }
        getContentPane().add(factory.createToolBar("main-toolbar"), BorderLayout.PAGE_START);

        viewPort = new ViewPort(app.getViewManager(), this);
        sidebar = new LeftTabbedPane(app.getCore(), this);
        commandManager.setViewPort(viewPort);

        //Add the scroll panes to a split pane.
        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitPane.setLeftComponent(sidebar);
        splitPane.setRightComponent(viewPort);
        splitPane.setDividerLocation(150);
        splitPane.setDividerSize(15);
        splitPane.setOneTouchExpandable(true);

        getContentPane().add(splitPane, BorderLayout.CENTER);
        this.pack();
    }

    private void removeQuitFromFileMenu() {
        JMenuBar menuBar = getJMenuBar();
        Action fileMenuAction = actionManager.getAction("file-menu");
        Action quitCommandAction = actionManager.getAction(CommandAction.Quit.actionName());
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

    public void refreshAll() {
        app.getViewManager().refreshAll();
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

    public void createNewFrame() {
        app.createNewFrame(false);
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
                app.getViewManager().closeAllViews();
                TheoryParser theoryParser = new TheoryParser(f.getAbsolutePath());
                app.updateCoreTheory(theoryParser.getImplementedTheoryName(), theoryParser.getTheoryVertices());
                app.setPreference(quanto.gui.QuantoApp.LAST_THEORY_OPEN_FILE, f.getAbsolutePath());
                app.createNewFrame(true);
                this.dispose();
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
        actionManager.setEnabled(CommandAction.Close.actionName(), allowed);
    }

    public boolean isViewAllowedToClose() {
        return actionManager.isEnabled(CommandAction.Close.actionName());
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
