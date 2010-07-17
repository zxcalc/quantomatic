/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui;

import edu.uci.ics.jung.contrib.HasName;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Map;
import org.apache.commons.collections15.BidiMap;
import org.apache.commons.collections15.bidimap.DualTreeBidiMap;
import org.apache.commons.collections15.comparators.ComparableComparator;
import org.apache.commons.collections15.contrib.HashCodeComparator;

/**
 *
 * @author alex
 */
public class InteractiveViewManager {
	// bidirectional map implemented as dual trees. note that get(null) or
	//  getKey(null) will raise exceptions in the Comparators.
	private final BidiMap<String, InteractiveView> views =
		new DualTreeBidiMap<String, InteractiveView>(
		ComparableComparator.<String>getInstance(),
		new HashCodeComparator<InteractiveView>());
	private final ConsoleView console;
	private ViewRenameListener viewRenameListener = new ViewRenameListener();

	private class ViewRenameListener implements PropertyChangeListener {
		public void propertyChange(PropertyChangeEvent evt) {
			if (!"title".equals(evt.getPropertyName()))
				return;
			InteractiveView view = (InteractiveView)evt.getSource();
			synchronized (views) {
				String oldName = views.getKey(view);
				views.remove(oldName);
				String newName = HasName.StringNamer.getFreshName(views.keySet(), evt.getNewValue().toString());
				views.put(newName, view);
			}
		}
	}

	public InteractiveViewManager(QuantoApp app, QuantoCore core) {
		console = new ConsoleView(app, core);
		addView(console);
	}

	public ConsoleView getConsole() {
		return console;
	}

	public InteractiveView getNextFreeView() {
		return getNextFreeView(false);
	}

	public InteractiveView getNextFreeView(boolean favourNonConsole) {
		InteractiveView foundView = null;
		for (InteractiveView view : views.values()) {
			if (!view.isAttached()) {
				foundView = view;
				if (!favourNonConsole || view != console)
					break;
			}
		}
		return foundView;
	}

	public void addView(InteractiveView view) {
		String name = HasName.StringNamer.getFreshName(views.keySet(), view.getTitle());
		synchronized (views) {
			views.put(name, view);
			view.setViewManager(this);
			view.addPropertyChangeListener("title", viewRenameListener);
		}
	}

	public void removeView(InteractiveView view) {
		synchronized (views) {
			view.removePropertyChangeListener("title", viewRenameListener);
			view.setViewManager(null);
			views.removeValue(view);
		}
	}

	public String getViewName(InteractiveView v) {
		return views.getKey(v);
	}

	public InteractiveView getView(String name) {
		return views.get(name);
	}

	public Map<String, InteractiveView> getViews() {
		return views;
	}

	public void removeView(String name) {
		InteractiveView view = views.get(name);
		if (view == null)
			throw new IllegalArgumentException("No such view");
		removeView(view);
	}

	public boolean closeAllViews() {
		for (InteractiveView view : views.values()) {
			if (!view.checkCanClose())
				return false;
		}
		for (InteractiveView view : views.values()) {
			if (view.isAttached())
				view.getViewPort().detachView();
			view.cleanUp();

		}
		return true;
	}

	/**
	 * Call "repaint" on all views that might be visible
	 */
	public void repaintViews() {
		synchronized (views) {
			for (InteractiveView v : views.values()) {
				v.repaint();
			}
		}
	}

	public void refreshAll() {
		synchronized (views) {
			for (InteractiveView v : views.values()) {
				v.refresh();
			}
		}
	}
}
