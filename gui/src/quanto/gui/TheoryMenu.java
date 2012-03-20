/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.logging.Logger;
import javax.swing.ButtonGroup;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JRadioButtonMenuItem;
import org.xml.sax.SAXException;
import quanto.core.CoreChangeListener;
import quanto.core.CoreException;
import quanto.core.Theory;
import quanto.core.TheoryChangeEvent;

/**
 *
 * @author alex
 */
public class TheoryMenu extends JMenu {

    private final static Logger logger = Logger.getLogger("quanto.gui.TheoryMenu");

    private final TheoryManager manager;
    private Frame parent;
    private QuantoApp app;
    private JMenu removeMenu;
    private List<Theory> shadowList;
    private int activeIndex = -1;
    private ButtonGroup activeTheoryGroup = new ButtonGroup();

    private final Comparator<Theory> theoryAlphaComparator =
            new Comparator<Theory>() {

        public int compare(Theory o1, Theory o2) {
            if (o1 == o2)
                return 0;
            if (o1 == null)
                return -1;
            if (o2 == null)
                return 1;
            int result = o1.getFriendlyName().compareToIgnoreCase(
                    o2.getFriendlyName());
            if (result == 0)
                result = o1.getFriendlyName().compareTo(o2.getFriendlyName());
            return result;
        }
    };
    private final TheoryManager.ChangeListener theoryManagerListener =
            new TheoryManager.ChangeListener() {

        public void theoryAdded(Theory theory) {
            int pos = Collections.binarySearch(shadowList, theory, theoryAlphaComparator);
            assert (pos < 0);
            pos = -(pos+1);

            TheoryRadioMenuItem setItem = new TheoryRadioMenuItem(theory);
            setItem.addActionListener(setActiveTheoryListener);
            activeTheoryGroup.add(setItem);
            TheoryMenu.this.add(setItem, pos);

            TheoryMenuItem remItem = new TheoryMenuItem(theory);
            remItem.addActionListener(unloadTheoryListener);
            removeMenu.add(remItem, (activeIndex < pos) ? pos-1 : pos);

            if (activeIndex >= pos)
                ++activeIndex;
            shadowList.add(pos, theory);
        }

        public void theoryRemoved(Theory theory) {
            int pos = Collections.binarySearch(shadowList, theory, theoryAlphaComparator);
            assert (pos >= 0);
            assert (pos != activeIndex);

            activeTheoryGroup.remove((TheoryRadioMenuItem)TheoryMenu.this.getMenuComponent(pos));
            TheoryMenu.this.remove(pos);
            removeMenu.remove((activeIndex < pos) ? pos-1 : pos);

            shadowList.remove(pos);
            if (activeIndex > pos)
                --activeIndex;
        }
    };
    private final CoreChangeListener coreListener =
            new CoreChangeListener() {

        public void theoryAboutToChange(TheoryChangeEvent evt) {}

        public void theoryChanged(TheoryChangeEvent evt) {
            if (activeIndex >= 0) {
                assert (shadowList.get(activeIndex) == evt.getOldTheory());
                TheoryMenuItem item = new TheoryMenuItem(shadowList.get(activeIndex));
                item.addActionListener(unloadTheoryListener);
                removeMenu.add(item, activeIndex);
                activeIndex = -1;
            }
            assert(activeIndex < 0);
            if (evt.getNewTheory() != null) {
                int pos = Collections.binarySearch(shadowList, evt.getNewTheory(), theoryAlphaComparator);
                assert (pos >= 0);
                activeIndex = pos;
                removeMenu.remove(activeIndex);
            }
        }
    };
    private final ActionListener setActiveTheoryListener =
            new ActionListener() {

        public void actionPerformed(ActionEvent e) {
            try {
                TheoryRadioMenuItem tmi = (TheoryRadioMenuItem)e.getSource();
                manager.getCore().updateCoreTheory(tmi.theory);
            } catch (CoreException ex) {
                app.errorDialog(ex.getLocalizedMessage());
            }
        }
    };
    private final ActionListener unloadTheoryListener =
            new ActionListener() {

        public void actionPerformed(ActionEvent e) {
            TheoryMenuItem tmi = (TheoryMenuItem)e.getSource();
            manager.unloadTheory(tmi.theory);
        }
    };

    private static class TheoryMenuItem extends JMenuItem {
        public Theory theory;

        public TheoryMenuItem(Theory theory) {
            super(theory.getFriendlyName());
            this.theory = theory;
        }
    }
    private static class TheoryRadioMenuItem extends JRadioButtonMenuItem {
        public Theory theory;

        public TheoryRadioMenuItem(Theory theory) {
            super(theory.getFriendlyName());
            this.theory = theory;
        }
    }

    public TheoryMenu(QuantoApp app, Frame parent) {
        super("Theories");
        this.manager = app.getTheoryManager();
        this.app = app;
        this.parent = parent;

        manager.addChangeListener(theoryManagerListener);
        manager.getCore().addCoreChangeListener(coreListener);
        removeMenu = new JMenu("Unload");

        shadowList = new ArrayList<Theory>(manager.getTheories());
        Collections.sort(shadowList, theoryAlphaComparator);
        int i = 0;
        for (Theory theory : shadowList) {
            TheoryRadioMenuItem setItem = new TheoryRadioMenuItem(theory);
            setItem.addActionListener(setActiveTheoryListener);
            activeTheoryGroup.add(setItem);
            this.add(setItem);

            if (theory == manager.getCore().getActiveTheory()) {
                activeIndex = i;
                setItem.setSelected(true);
            } else {
                TheoryMenuItem removeItem = new TheoryMenuItem(theory);
                removeItem.addActionListener(unloadTheoryListener);
                removeMenu.add(removeItem);
            }
            ++i;
        }

        this.addSeparator();
        addLoadMenuItem();
        this.add(removeMenu);
    }
    
    private void loadTheory() {
        File f = app.openFile(parent, "Select theory file", QuantoApp.DIR_THEORY);
        if (f != null) {
            try {
                Theory theory = manager.loadTheory(f.toURI().toURL());
                manager.getCore().updateCoreTheory(theory);
            } catch (CoreException ex) {
                app.errorDialog(ex.getLocalizedMessage());
            } catch (SAXException ex) {
                app.errorDialog(ex.getLocalizedMessage());
            } catch (IOException ex) {
                app.errorDialog(ex.getLocalizedMessage());
            } catch (DuplicateTheoryException ex) {
                // FIXME: maybe offer to replace it directly?
                app.errorDialog("There is already a theory named \"" +
                        ex.getTheoryName() +
                        "\"; please remove it first");
            }
        }
    }
    
    private void addLoadMenuItem() {
        JMenuItem item = new JMenuItem("Import new...");
        item.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                loadTheory();
            }
        });
        this.add(item);
    }
}
