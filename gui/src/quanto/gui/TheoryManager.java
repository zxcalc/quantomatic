/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.Preferences;
import javax.swing.event.EventListenerList;
import org.xml.sax.SAXException;
import quanto.core.*;
import quanto.core.xml.TheoryHandler;
import quanto.util.FileUtils;

/**
 *
 * @author alex
 */
public class TheoryManager {

    private final static Logger logger = Logger.getLogger("quanto.gui");
    private static final FileFilter theoryDirFilter = new FileFilter() {
        public boolean accept(File pathname) {
            return pathname.isDirectory() &&
                    new File(pathname, theoryFilename).isFile();
        }
    };
    // the first theory listed here is made active
    private static final String[] defaultTheories = {
        "/theories/red_green/red-green-theory.qth",
        "/theories/black_white/black-white-theory.qth"
    };
    private static final String theoryFilename = "theory.qth";
    private static final String rulesetFilename = "ruleset.qrs";
    private static final String lastTheoryKey = "last-active-theory";
    
    public interface ChangeListener extends EventListener {
        void theoryAdded(Theory theory);
        void theoryRemoved(Theory theory);
    }

    private final File store;
    private final Core core;
    private Map<String,Theory> theorys = new HashMap<String,Theory>();
    EventListenerList listenerList = new EventListenerList();

    private final CoreChangeListener coreListener = new CoreChangeListener() {
        public void theoryChanged(TheoryChangeEvent evt) {
            if (evt.getNewTheory() == null)
                return;

            try {
                File rsetFile = getRulesetFile(evt.getNewTheory());
                if (rsetFile.isFile()) {
                    core.replaceRuleset(rsetFile);
                }
            } catch (CoreException ex) {
                logger.log(Level.WARNING, "Could not load ruleset for " +
                        evt.getNewTheory().getFriendlyName() + " theory", ex);
            } catch (IOException ex) {
                logger.log(Level.WARNING, "Could not load ruleset for " +
                        evt.getNewTheory().getFriendlyName() + " theory", ex);
            }
        }

        public void theoryAboutToChange(TheoryChangeEvent evt) {
            if (evt.getOldTheory() == null)
                return;

            try {
                core.saveRuleset(getRulesetFile(evt.getOldTheory()));
            } catch (CoreException ex) {
                logger.log(Level.WARNING, "Could not save ruleset for " +
                        evt.getOldTheory().getFriendlyName() + " theory", ex);
            } catch (IOException ex) {
                logger.log(Level.WARNING, "Could not save ruleset for " +
                        evt.getOldTheory().getFriendlyName() + " theory", ex);
            }
        }
    };

    public TheoryManager(File localStore, Core core) {
        this.store = localStore;
        this.core = core;
        core.addCoreChangeListener(coreListener);
        loadFromStore();
    }

    public Core getCore() {
        return core;
    }

    public void addChangeListener(ChangeListener l) {
        listenerList.add(ChangeListener.class, l);
    }

    public void removeChangeListener(ChangeListener l) {
        listenerList.remove(ChangeListener.class, l);
    }

    protected void fireTheoryAdded(Theory theory) {
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==ChangeListener.class) {
                ((ChangeListener)listeners[i+1]).theoryAdded(theory);
            }
        }
    }

    protected void fireTheoryRemoved(Theory theory) {
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length-2; i>=0; i-=2) {
            if (listeners[i]==ChangeListener.class) {
                ((ChangeListener)listeners[i+1]).theoryRemoved(theory);
            }
        }
    }
    
    private File getTheoryDir(Theory theory) {
        return new File(store, getId(theory));
    }
    
    private File getRulesetFile(Theory theory) {
        return new File(getTheoryDir(theory), rulesetFilename);
    }
    
    private File getTheoryFile(Theory theory) {
        return new File(getTheoryDir(theory), theoryFilename);
    }

    private void saveTheoryCopy(TheoryHandler.Data data, final File theoryDir) {
        if (theoryDir.isDirectory()) {
            for (File file : theoryDir.listFiles()) {
                file.delete();
            }
        } else if (!theoryDir.mkdir()) {
            logger.log(Level.WARNING,
                    "Failed to create dir {0} to save theory",
                    theoryDir.getAbsolutePath());
            return;
        }

        File theoryFile = new File(theoryDir, theoryFilename);
        try {
            TheoryHandler.ResourceHandler resHandler = new TheoryHandler.ResourceHandler() {
                int i = 0;
                public URL getResource(URL original) throws IOException {
                    ++i;
                    File resFile = new File(theoryDir,
                            String.format("%d.svg", i));
                    FileUtils.copy(original, resFile);
                    return resFile.toURI().toURL();
                }
            };
            TheoryHandler.write(data, theoryFile, resHandler);
        } catch (IOException ex) {
            logger.log(Level.WARNING,
                    "Failed to save local copy of theory", ex);
        }
    }

    /**
     * Unload the theory
     * 
     * @param theory
     * @throws IllegalArgumentException theory is currently active
     */
    public void unloadTheory(Theory theory) throws IllegalArgumentException {
        if (theory == core.getActiveTheory()) {
            throw new IllegalArgumentException("The " +
                    theory.getFriendlyName() + " theory is currently active");
        }
        String id = getId(theory);
        File theoryDir = getTheoryDir(theory);
        if (theoryDir.isDirectory()) {
            for (File file : theoryDir.listFiles()) {
                file.delete();
            }
            theoryDir.delete();
        }
        theorys.remove(id);
        fireTheoryRemoved(theory);
    }

    public Collection<Theory> getTheories() {
        return Collections.unmodifiableCollection(theorys.values());
    }

    /**
     * 
     * @param url
     * @return the loaded theory, or null if there was another theory with
     *         the same name
     * @throws SAXException
     * @throws IOException 
     */
    public Theory loadTheory(URL url) throws SAXException, IOException, DuplicateTheoryException {
        TheoryHandler.Data theoryData = TheoryHandler.parse(url);
        Theory theory = new Theory(theoryData);
        addTheory(theory);
        saveTheoryCopy(theoryData, getTheoryDir(theory));
        return theory;
    }

    public void saveState() {
        Theory theory = core.getActiveTheory();
        if (theory == null)
            return;

        Preferences.userNodeForPackage(getClass())
                .put(lastTheoryKey, getId(theory));
        try {
            core.saveRuleset(getRulesetFile(theory));
        } catch (CoreException ex) {
            logger.log(Level.WARNING, "Could not save ruleset for " +
                    theory.getFriendlyName() + " theory", ex);
        } catch (IOException ex) {
            logger.log(Level.WARNING, "Could not save ruleset for " +
                    theory.getFriendlyName() + " theory", ex);
        }
    }
    
    private String getId(Theory theory) {
        try {
            return URLEncoder.encode(theory.getFriendlyName(), "US-ASCII");
        } catch (UnsupportedEncodingException ex) {
            throw new Error(ex);
        }
    }

    private void addTheory(Theory theory) throws DuplicateTheoryException {
        String id = getId(theory);
        if (theorys.containsKey(id)) {
            throw new DuplicateTheoryException(theory.getFriendlyName());
        }
        theorys.put(id, theory);
        fireTheoryAdded(theory);
    }

    private void loadDefaults() {
        for (String res : defaultTheories) {
            try {
                URL url = getClass().getResource(res);
                if (url != null) {
                    Theory theory = loadTheory(url);
                    if (core.getActiveTheory() == null) {
                        core.updateCoreTheory(theory);
                    }
                } else {
                    logger.log(Level.SEVERE,
                            "Could not find default theory {0}", res);
                }
            } catch (SAXException ex) {
                logger.log(Level.SEVERE,
                        "Default theory " + res + " was not valid",
                        ex);
            } catch (IOException ex) {
                logger.log(Level.SEVERE,
                        "Could not open default theory " + res,
                        ex);
            } catch (CoreException ex) {
                logger.log(Level.SEVERE,
                        "Core did not recognise default theory " + res,
                        ex);
            } catch (DuplicateTheoryException ex) {
                logger.log(Level.SEVERE,
                        "Unexpected duplicate default theory names",
                        ex);
            }
        }
    }

    private Theory loadSavedTheory(File theoryDir) {
        try {
            File theoryFile = new File(theoryDir, theoryFilename);
            Theory theory = new Theory(TheoryHandler.parse(theoryFile));
            if (!theoryDir.getName().equals(getId(theory))) {
                logger.log(Level.SEVERE,
                        "Theory encoding changed from under us: expected " +
                        "\"{0}\", got \"{1}\" for theory \"{2}\"",
                        new Object[] {
                            getId(theory),
                            theoryDir.getName(),
                            theory.getFriendlyName()
                        });
                // not much we can do if this fails
                boolean moved = theoryDir.renameTo(new File(store, getId(theory)));
                if (!moved) {
                    logger.log(Level.SEVERE, "Failed to rename theory directory");
                }
            }
            addTheory(theory);
            return theory;
        } catch (SAXException ex) {
            logger.log(Level.SEVERE,
                    "Saved theory (" + theoryDir.getAbsolutePath() +
                    ") was not valid",
                    ex);
        } catch (IOException ex) {
            logger.log(Level.SEVERE,
                    "Could not open saved theory (" +
                    theoryDir.getAbsolutePath() + ")",
                    ex);
        } catch (DuplicateTheoryException ex) {
            logger.log(Level.SEVERE,
                    "Unexpected duplicate saved theory names",
                    ex);
        }
        return null;
    }

    private void loadFromStore() {
        if (!store.exists()) {
            if (!store.mkdirs()) {
                logger.log(Level.WARNING, "Could not create local theory store");
            }
            loadDefaults();
        } else if (!store.isDirectory()) {
            logger.log(Level.WARNING,
                    "Local theory store ({0}) is not a directory",
                    store.getAbsolutePath());
        } else {
            String lastActiveId = Preferences.userNodeForPackage(getClass())
                    .get(lastTheoryKey, null);
            Theory activate = null;

            for (File dir : store.listFiles(theoryDirFilter)) {
                Theory theory = loadSavedTheory(dir);
                if (dir.getName().equals(lastActiveId)) {
                    activate = theory;
                }
            }

            if (activate == null && !theorys.isEmpty()) {
                if (lastActiveId == null) {
                    logger.log(Level.FINE,
                            "Theories found, but no active theory recorded");
                } else {
                    logger.log(Level.FINE,
                            "Last active theory ({0}) was not found",
                            lastActiveId);
                }
                Iterator<Theory> it = theorys.values().iterator();
                assert(it.hasNext());
                activate = it.next();
            }

            if (activate != null) {
                try {
                    core.updateCoreTheory(activate);
                } catch (CoreException ex) {
                    logger.log(Level.SEVERE,
                            "Saved theory (" + activate.getFriendlyName() +
                            ") was not recognised by the core", ex);
                }
            }
        }
    }
}
