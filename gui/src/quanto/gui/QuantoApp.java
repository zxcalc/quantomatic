// vim:sts=8:ts=8:noet:sw=8
package quanto.gui;

import apple.dts.samplecode.osxadapter.OSXAdapter;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.Preferences;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.UIManager;
import org.xml.sax.SAXException;
import quanto.core.*;
import quanto.core.data.CoreGraph;
import quanto.core.protocol.ProtocolManager;
import quanto.core.xml.TheoryHandler;
import quanto.util.FileUtils;

/**
 * Singleton class 
 * @author aleks
 *
 */
public class QuantoApp {

    private static final boolean LOG_PROTOCOL = false;
    private static final boolean LOG_JUNG = false;
    private static final boolean LOG_QUANTO = false;
    private final static Logger logger =
            Logger.getLogger("quanto.gui");
    // isMac is used for CTRL vs META shortcuts, etc
    public static final boolean isMac =
            (System.getProperty("os.name").toLowerCase().indexOf("mac") != -1);
    public static final boolean isWin =
            (System.getProperty("os.name").toLowerCase().indexOf("win") != -1);
    public static final boolean isUnix =
            (System.getProperty("os.name").toLowerCase().indexOf("nix") != -1
            || System.getProperty("os.name").toLowerCase().indexOf("nux") != -1);
    // MAC_OS_X is used to determine whether we use OSXAdapter to
    // hook into the application menu
    public static final boolean MAC_OS_X =
            (System.getProperty("os.name").toLowerCase().startsWith("mac os x"));
    public static final int COMMAND_MASK =
            MAC_OS_X ? java.awt.event.InputEvent.META_DOWN_MASK
            : java.awt.event.InputEvent.CTRL_DOWN_MASK;
    private static QuantoApp theApp = null;
    public static boolean useExperimentalLayout = false;
    private final Preferences globalPrefs;
    private final Core core;
    private JFileChooser[] fileChooser = {null, null, null};
    private InteractiveViewManager viewManager;
    private TheoryManager theoryManager;
    public static final String lastTheoryDir = "theory";
    public static final String lastTheoryFileName = lastTheoryDir + File.separatorChar + "stored.qth";

    public static File getAppSettingsDirectory(boolean create) throws IOException {
        File dir;
        String userHome = System.getProperty("user.home");
        if (isWin) {
            dir = new File(userHome + File.separatorChar + "Quantomatic");
        } else if (isUnix) {
            dir = new File(userHome
                    + File.separatorChar + ".config"
                    + File.separatorChar + "Quantomatic");
        } else {
            dir = new File(userHome
                    + File.separatorChar + ".quantomatic");
        }
        if (create && !dir.exists()) {
            if (!dir.mkdirs()) {
                throw new IOException("Failed to create preferences directory " + dir.getAbsolutePath());
            }
        }
        if (dir.exists() && !dir.isDirectory()) {
            throw new IOException(dir.getAbsolutePath() + " is not a directory!");
        }
        return dir;
    }

    public String getRootDirectory() {
        String applicationDir = getClass().getProtectionDomain().getCodeSource().getLocation().getPath();
        if (applicationDir.endsWith(".jar")) {
            applicationDir = new File(applicationDir).getParent();
        } else {
            applicationDir += getClass().getName().replace('.', File.separatorChar);
            applicationDir = new File(applicationDir).getParent();
        }

        if (applicationDir.endsWith("gui" + File.separator + "dist")) {
            applicationDir = applicationDir.replaceFirst(File.separator + "gui" + File.separator + "dist", "");
        } else {
            applicationDir = applicationDir.replaceFirst(File.separator + "gui" + File.separator + "bin"
                    + File.separator + "quanto" + File.separator + "gui", "");
        }

        /*
         * If the user relocates the .jar file and appends the path to the core to $PATH
         * we cannot really infer the location of the root dir (or can we?): 
         * No default files will be loaded
         */

        return applicationDir;
    }

    /**
     * Gets the local persistent copy of a file for a given name for reading
     * 
     * If this returns a non-null object, it will be the same as that returned
     * by getPersistentCopyForWriting, and the same as the file saved to by
     * savePersistentCopy.
     * 
     * @param name the name to store the file as
     * @return null if there was no persistent copy, else a File object pointing
     *         to an actual file
     */
    private File getPersistentCopyForReading(String name) {
        File f = null;
        try {
            f = new File(getAppSettingsDirectory(false), name);
            if (!f.isFile()) {
                f = null;
            }
        } catch (IOException ex) {
        }
        return f;
    }

    /**
     * Gets the local persistent copy of a file for a given name for writing
     * 
     * @param name the name to store the file as
     * @return a File object; the file may not exist, but the parent directory
     *         will
     * @throws IOException if there was a non-normal file with that name, or if
     *                     the parent directory of the requested file could not
     *                     be created
     */
    private File getPersistentCopyForWriting(String name)
            throws IOException {
        File f = new File(getAppSettingsDirectory(false), name);
        if (f.exists() && !f.isFile()) {
            throw new IOException(name + " is a not a normal file");
        }
        if (!f.getParentFile().exists()) {
            if (!f.getParentFile().mkdirs()) {
                throw new IOException("Could not create parent directory for " + name);
            }
        }
        return f;
    }

    /**
     * Save a local persistent copy of a file
     * 
     * The copy can later be retrieved with getPersistentCopyForReading.
     *
     * @param file  the file to save a copy of
     * @param name  the name to give the saved copy
     */
    private void savePersistentCopy(File file, String name) {
        try {
            FileUtils.copy(file, getPersistentCopyForWriting(name));
        } catch (IOException ex) {
            logger.log(Level.WARNING, "Failed to save local copy of " + file.getPath(), ex);
        }
    }

    private static class Pref<T> {

        final T def; // default value
        final String key;
        String friendlyName;

        protected Pref(String key, T def) {
            this.key = key;
            this.def = def;
        }

        protected Pref(String key, T def, String friendlyName) {
            this.key = key;
            this.def = def;
            this.friendlyName = friendlyName;
        }

        public String getFriendlyName() {
            return friendlyName;
        }
    }

    public static class StringPref extends Pref<String> {

        protected StringPref(String key, String def) {
            super(key, def);
        }
    }

    public static class BoolPref extends Pref<Boolean> implements ItemListener {

        protected BoolPref(String key, Boolean def) {
            super(key, def);
        }

        protected BoolPref(String key, Boolean def, String friendlyName) {
            super(key, def, friendlyName);
        }

        public void itemStateChanged(ItemEvent e) {
            QuantoApp.getInstance().setPreference(this, e.getStateChange() == ItemEvent.SELECTED);
        }
    }
    // Preferences
    public static final BoolPref DRAW_ARROW_HEADS =
            new BoolPref("draw_arrow_heads", false, "Draw arrow geads");
    public static final BoolPref NEW_WINDOW_FOR_GRAPHS =
            new BoolPref("new_window_for_graphs", false, "Open graphs in a new window");
    public static final BoolPref SHOW_INTERNAL_NAMES =
            new BoolPref("show_internal_names", false, "Show internal graph names");
    public static final StringPref[] LAST_OPEN_DIRS = {new StringPref("last_open_dir", null),
        new StringPref("last_open_ruleset_dir", null),
        new StringPref("last_open_theory_dir", null)};
    public static final int DIR_GRAPH = 0;
    public static final int DIR_RULESET = 1;
    public static final int DIR_THEORY = 2;

    public static QuantoApp getInstance() {
        if (theApp == null) {
            try {
                theApp = new QuantoApp();
            } catch (CoreException ex) {
                // FATAL!!!
                logger.log(Level.SEVERE, "Failed to start core: terminating", ex);
                JOptionPane.showMessageDialog(null,
                        ex.getMessage(),
                        "Could not start core",
                        JOptionPane.ERROR_MESSAGE);
                System.exit(1);
            }
        }
        return theApp;
    }

    public static boolean hasInstance() {
        return !(theApp == null);
    }

    /**
     * main entry point for the GUI application
     * @param args
     */
    public static void main(String[] args) {
        if (LOG_PROTOCOL) {
            // protocol stream
            Logger protocolLogger = Logger.getLogger("quanto.core.protocol.stream");
            protocolLogger.setUseParentHandlers(false);
            ConsoleHandler ch = new ConsoleHandler();
            ch.setLevel(Level.FINEST);
            protocolLogger.addHandler(ch);

            // choose real log level here
            protocolLogger.setLevel(Level.ALL);
        } else {
            // only log problems by default
            // this is required for when LOG_QUANTO is true but LOG_PROTOCOL is false
            Logger protocolLogger = Logger.getLogger("quanto.core.protocol.stream");
            protocolLogger.setLevel(Level.INFO);
        }
        if (LOG_QUANTO) {
            // log everything to the console
            Logger ql = Logger.getLogger("quanto");
            ql.setUseParentHandlers(false);
            ConsoleHandler ch = new ConsoleHandler();
            ch.setLevel(Level.FINEST);
            ql.addHandler(ch);

            // choose real log level here
            ql.setLevel(Level.ALL);
        }
        if (LOG_JUNG) {
            // log everything to the console
            Logger ql = Logger.getLogger("edu.uci.ics.jung");
            ql.setUseParentHandlers(false);
            ConsoleHandler ch = new ConsoleHandler();
            ch.setLevel(Level.FINEST);
            ql.addHandler(ch);

            // choose real log level here
            ql.setLevel(Level.ALL);
        }

        logger.log(Level.FINER, "Starting quantomatic");
        boolean mathematicaMode = false;
        for (String arg : args) {
            if (arg.equals("--app-mode")) {
                String appName = "Quantomatic.app";

                // determine the app name from the classpath if I can...
                String classpath = System.getProperty("java.class.path");
                logger.log(Level.FINEST,
                        "Trying to determine app name using class path ({0})",
                        classpath);
                for (String path : classpath.split(System.getProperty("path.separator"))) {
                    if (path.indexOf("QuantoGui.jar") != -1) {
                        String[] dirs = path.split(System.getProperty("file.separator"));
                        if (dirs.length >= 5) {
                            appName = dirs[dirs.length - 5];
                        }
                    }
                }

                logger.log(Level.FINER, "Invoked as OS X application ({0})", appName);
                edu.uci.ics.jung.contrib.algorithms.layout.AbstractDotLayout.dotProgram =
                        appName + "/Contents/MacOS/dot_static";
                ProtocolManager.quantoCoreExecutable =
                        appName + "/Contents/MacOS/quanto-core-app";
            } else if (arg.equals("--mathematica-mode")) {
                mathematicaMode = true;
                logger.log(Level.FINER, "Mathematica mode enabled");
            }
        }
        logger.log(Level.FINE, "Using dot executable: {0}",
                edu.uci.ics.jung.contrib.algorithms.layout.AbstractDotLayout.dotProgram);
        logger.log(Level.FINE, "Using core executable: {0}",
                ProtocolManager.quantoCoreExecutable);

        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) {
            logger.log(Level.WARNING, "Could not set look-and-feel", e);
        }

        if (QuantoApp.isMac && !mathematicaMode) {
            //System.setProperty("apple.laf.useScreenMenuBar", "true");
            System.setProperty(
                    "com.apple.mrj.application.apple.menu.about.name",
                    "Quanto");
        }

        QuantoApp app = getInstance();

        app.newGraph(true);

        logger.log(Level.FINER, "Finished initialisation");
    }

    public boolean shutdown() {
        theoryManager.saveState();
        logger.log(Level.FINER, "Shutting down");
        if (viewManager.closeAllViews()) {
            logger.log(Level.FINER, "Exiting now");
            System.exit(0);
        }
        return false;
    }

    public TheoryManager getTheoryManager() {
        return theoryManager;
    }

    private void demandTheoryOrQuit() {
        File f = openFile(null, "Select theory file", QuantoApp.DIR_THEORY);
        if (f == null) {
            errorDialog("Cannot proceed without a theory");
            System.exit(1);
        }
        try {
            Theory theory = theoryManager.loadTheory(f.toURI().toURL());
            core.updateCoreTheory(theory);
        } catch (IOException ex) {
            errorDialog(String.format(
                    "Could not open theory file (%1$s); cannot proceed",
                    ex.getMessage()));
            System.exit(1);
        } catch (SAXException ex) {
            errorDialog(String.format(
                    "Corrupted theory file (%1$s); cannot proceed",
                    ex.getMessage()));
            System.exit(1);
        } catch (CoreException ex) {
            errorDialog(String.format(
                    "Core refused to load theory (%1$s); cannot proceed",
                    ex.getMessage()));
            System.exit(1);
        } catch (DuplicateTheoryException ex) {
            logger.log(Level.SEVERE,
                    "Got a duplicate theory exception, but there were " +
                    "no existing theories",
                    ex);
            System.exit(1);
        }
    }

    private QuantoApp() throws CoreException {
        globalPrefs = Preferences.userNodeForPackage(this.getClass());

        core = new Core();
        viewManager = new InteractiveViewManager();

        File theoryDir = null;
        try {
            theoryDir = new File(getAppSettingsDirectory(true), "theories");
        } catch (IOException ex) {
            logger.log(Level.SEVERE, ex.getLocalizedMessage(), ex);
        }
        theoryManager = new TheoryManager(theoryDir, core);
        if (core.getActiveTheory() == null)
            demandTheoryOrQuit();
        
        core.addCoreChangeListener(new CoreChangeListener() {

            public void theoryAboutToChange(TheoryChangeEvent evt) {}

            public void theoryChanged(TheoryChangeEvent evt) {
                viewManager.closeAllViews();
            }
        });

        if (MAC_OS_X) {
            try {
                OSXAdapter.setQuitHandler(this, getClass().getDeclaredMethod("shutdown", (Class[]) null));
            } catch (Exception e) {
                logger.log(Level.SEVERE, "Could not set quit handler", e);
            }
        }
    }

    private void createFileChooser(int type) {
        if (fileChooser[type] == null) {
            fileChooser[type] = new JFileChooser();
            String lastDir = getPreference(QuantoApp.LAST_OPEN_DIRS[type]);
            if (lastDir != null) {
                fileChooser[type].setCurrentDirectory(new File(lastDir));
            }
        }
    }

    public File openFile(Component parent, String title, int type) {
        createFileChooser(type);
        int retVal = fileChooser[type].showDialog(parent, title);
        fileChooser[type].setDialogType(JFileChooser.OPEN_DIALOG);
        if (retVal == JFileChooser.APPROVE_OPTION) {
            File f = fileChooser[type].getSelectedFile();
            if (f.getParent() != null) {
                setPreference(QuantoApp.LAST_OPEN_DIRS[type], f.getParent());
            }
            return f;
        }
        return null;
    }

    public File openFile(Component parent) {
        return openFile(parent, "Open", DIR_GRAPH);
    }

    public File saveFile(Component parent, String title, int type) {
        createFileChooser(type);
        int retVal = fileChooser[type].showDialog(parent, title);
        fileChooser[type].setDialogType(JFileChooser.SAVE_DIALOG);
        if (retVal == JFileChooser.APPROVE_OPTION) {
            File f = fileChooser[type].getSelectedFile();
            if (f.exists()) {
                int overwriteAnswer = JOptionPane.showConfirmDialog(
                        parent,
                        "Are you sure you want to overwrite \"" + f.getName() + "\"?",
                        "Overwrite file?",
                        JOptionPane.YES_NO_OPTION);
                if (overwriteAnswer != JOptionPane.YES_OPTION) {
                    return null;
                }
            }
            if (f.getParent() != null) {
                setPreference(QuantoApp.LAST_OPEN_DIRS[type], f.getParent());
            }
            return f;
        }
        return null;
    }

    public File saveFile(Component parent) {
        return saveFile(parent, "Save", DIR_GRAPH);
    }

    public InteractiveViewManager getViewManager() {
        return viewManager;
    }

    public Core getCore() {
        return core;
    }

    public void errorDialog(String message) {
        JOptionPane.showMessageDialog(null, message, "Console Error", JOptionPane.ERROR_MESSAGE);
    }

    public void createNewFrame() {
        try {
            InteractiveView view = viewManager.getNextFreeView();
            if (view == null) {
                view = createNewGraph();
            }
            openNewFrame(view);
        } catch (CoreException ex) {
            logger.log(Level.SEVERE, "Could not create a new graph", ex);
            errorDialog("Could not create a new graph to display");
        }
    }

    public void openNewFrame(InteractiveView view)
            throws ViewUnavailableException {
        QuantoFrame fr = new QuantoFrame(this);
        try {
            fr.getViewPort().attachView(view);
            fr.pack();
            fr.setVisible(true);
        } catch (ViewUnavailableException ex) {
            logger.log(Level.WARNING,
                    "Tried to open an already-attached view in a new frame", ex);
            fr.dispose();
            throw ex;
        }
    }

    public InteractiveGraphView createNewGraph()
            throws CoreException {
        CoreGraph newGraph = core.createEmptyGraph();
        InteractiveGraphView vis =
                new InteractiveGraphView(core, newGraph, new Dimension(800, 600));
        viewManager.addView(vis);
        return vis;
    }

    public InteractiveGraphView openGraph(File file)
            throws CoreException,
            java.io.IOException {
        CoreGraph loadedGraph = core.loadGraph(file);
        InteractiveGraphView vis =
                new InteractiveGraphView(core, loadedGraph, new Dimension(800, 600));
        vis.setTitle(file.getName());

        viewManager.addView(vis);
        core.renameGraph(loadedGraph, viewManager.getViewName(vis));

        //vis.cleanUp();
        //vis.updateGraph(null);
        vis.getGraph().setSaved(true);
        return vis;
    }

    /**
     * Create a new graph, read the name, and send to a fresh
     * InteractiveQuantoVisualizer.
     * @param initial   a <code>boolean</code> that tells whether this is the
     *                  first call to newGraph().
     */
    public void newGraph(boolean initial) {
        try {
            CoreGraph newGraph = core.createEmptyGraph();
            InteractiveGraphView vis =
                    new InteractiveGraphView(core, newGraph, new Dimension(800, 600));
            viewManager.addView(vis);

            if (initial || getPreference(NEW_WINDOW_FOR_GRAPHS)) { // are we making a new window?
                openNewFrame(vis);
            }
        } catch (CoreException e) {
            logger.log(Level.SEVERE, "Failed to create a new graph", e);
            errorDialog(e.getMessage());
        }
    }

    public void newGraph() {
        newGraph(false);
    }

    /**
     * Get a global preference. This method is overloaded because the preference API
     * doesn't support generics.
     */
    public boolean getPreference(QuantoApp.BoolPref pref) {
        return globalPrefs.getBoolean(pref.key, pref.def);
    }

    public String getPreference(QuantoApp.StringPref pref) {
        return globalPrefs.get(pref.key, pref.def);
    }

    /**
     * Set a global preference.
     */
    public void setPreference(QuantoApp.BoolPref pref, boolean value) {
        globalPrefs.putBoolean(pref.key, value);
    }

    public void setPreference(QuantoApp.StringPref pref, String value) {
        globalPrefs.put(pref.key, value);
    }
}

//vi:et:sts=4:sw=4