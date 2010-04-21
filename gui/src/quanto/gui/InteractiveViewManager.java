/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui;

import edu.uci.ics.jung.contrib.HasName;
import java.awt.Component;
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
	private volatile ViewPort focusedViewPort = null;

	public InteractiveViewManager() {
		console = new ConsoleView();
		addView("console", console);
	}

	public ConsoleView getConsole() {
		return console;
	}

	public String addView(String name, InteractiveView v) {
		String realName = HasName.StringNamer.getFreshName(views.keySet(), name);
		synchronized (views) {
			views.put(realName, v);
		}
		return realName;
	}

	public String getViewName(InteractiveView v) {
		return views.getKey(v);
	}

	public String renameView(String oldName, String newName) {
		String realNewName;
		synchronized (views) {
			InteractiveView v = views.get(oldName);
			if (v == null) {
				throw new QuantoCore.FatalError("Attempting to rename null view.");
			}
			views.remove(oldName);
			realNewName = addView(newName, v);
			if (focusedViewPort != null) {
				if (focusedViewPort.getFocusedView().equals(oldName)) {
					focusedViewPort.setFocusedView(realNewName);
				}
			}
		}
		return realNewName;
	}

	public String renameView(InteractiveView v, String newName) {
		return renameView(getViewName(v), newName);
	}

	public Map<String, InteractiveView> getViews() {
		return views;
	}

	public void removeView(String name) {
		synchronized (views) {
			views.remove(name);
		}
	}

	public boolean killAllViews() {
		if (focusedViewPort == null) {
			System.err.println("focusedViewPort shouldn't be null here! (QuantoApp.shutdown())");
		}

		while (focusedViewPort.focusNonConsole()) {
			String foc = focusedViewPort.getFocusedView();
			InteractiveView iv = views.get(foc);

			// if any of the viewKill() operations return false, abort
			if (iv != null && !iv.viewKill(focusedViewPort)) {
				return false;
			}
			focusedViewPort.focusConsole(); // weird things happen if we kill views while they are focused
			views.remove(foc);
		}
		return true;
	}

	public void forceFocus(String view) {
		if (focusedViewPort != null) {
			focusedViewPort.setFocusedView(view);
			focusedViewPort.gainFocus();
		}
	}

	/**
	 * Get the currently focused viewport.
	 * @return
	 */
	public ViewPort getFocusedViewPort() {
		return focusedViewPort;
	}

	/**
	 * Set the focused view port and call the relevant focus handlers.
	 * @param vp
	 */
	public void setFocusedViewPort(ViewPort vp) {
		if (vp != focusedViewPort) {
			if (focusedViewPort != null) {
				focusedViewPort.loseFocus();
			}
			focusedViewPort = vp;
			if (focusedViewPort != null) {
				focusedViewPort.gainFocus();
			}
		}
	}

	/**
	 * return the first InteractiveGraphView available, or null.
	 * @return
	 */
	public String getFirstFreeView() {
		synchronized (views) {
			for (Map.Entry<String, InteractiveView> ent : views.entrySet()) {
				if (!ent.getValue().viewHasParent()) {
					return ent.getKey();
				}
			}
		}
		return null;
	}

	/**
	 * Call "repaint" on all views that might be visible
	 */
	public void repaintViews() {
		synchronized (views) {
			for (InteractiveView v : views.values()) {
				if (v instanceof Component) {
					((Component) v).repaint();
				}
			}
		}
	}

	public void closeFocusedView() {
		if (focusedViewPort != null) {
			String foc = focusedViewPort.getFocusedView();
			InteractiveView iv = views.get(foc);

			// If the view allows itself to be killed, close the window.
			if (iv != null && iv.viewKill(focusedViewPort)) {
				focusedViewPort.focusConsole();
				removeView(foc);
				focusedViewPort.focusNonConsole();
			}
		}
	}
}
