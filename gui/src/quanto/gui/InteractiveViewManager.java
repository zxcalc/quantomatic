/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui;

import quanto.util.StringNamer;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Map;
import org.apache.commons.collections15.BidiMap;
import org.apache.commons.collections15.MapIterator;
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

	private PropertyChangeListener viewRenameListener = new PropertyChangeListener() {
		public void propertyChange(PropertyChangeEvent evt) {
			if (!"title".equals(evt.getPropertyName()))
				return;
			InteractiveView view = (InteractiveView)evt.getSource();
			synchronized (views) {
				String oldName = views.getKey(view);
				views.remove(oldName);
				String newName = StringNamer.getFreshName(views.keySet(), evt.getNewValue().toString());
				views.put(newName, view);
			}
		}
	};

	public InteractiveView getNextFreeView() {
		InteractiveView foundView = null;
		for (InteractiveView view : views.values()) {
			if (!view.isAttached()) {
				foundView = view;
                break;
			}
		}
		return foundView;
	}

	public void addView(InteractiveView view) {
		String name = StringNamer.getFreshName(views.keySet(), view.getTitle());
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
        MapIterator<String, InteractiveView> it = views.mapIterator();
        while (it.hasNext()) {
            it.next();
            InteractiveView view = it.getValue();
            it.remove();
            if (view.isAttached())
                view.getViewPort().switchToConsole();
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
