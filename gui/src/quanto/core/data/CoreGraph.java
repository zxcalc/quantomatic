package quanto.core.data;


import edu.uci.ics.jung.contrib.graph.DirectedSparseBangBoxMultigraph;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import edu.uci.ics.jung.visualization.util.ChangeEventSupport;

public class CoreGraph extends DirectedSparseBangBoxMultigraph<Vertex, Edge, BangBox>
implements CoreObject, ChangeEventSupport {

	private static final long serialVersionUID = -1519901566511300787L;
	protected String name;
	protected final Set<ChangeListener> changeListeners;
	
	private String fileName = null; // defined if this graph is backed by a file
	private boolean saved = true; // true if this graph has been modified since last saved

	public CoreGraph(String name) {
		this.name = name;
		this.changeListeners = Collections.synchronizedSet(
				new HashSet<ChangeListener>());
	}

	/**
	 * Use this constructor for unnamed graphs. The idea is you
	 * should do null checks before sending the name to the core.
	 */
	public CoreGraph() {
		this(null);
	}

	public Map<String,Vertex> getVertexMap() {
		Map<String, Vertex> verts =
			new HashMap<String, Vertex>();
		for (Vertex v : getVertices()) {
			verts.put(v.getCoreName(), v);
		}
		return verts;
	}
	
	public List<Vertex> getSubgraphVertices(CoreGraph graph) {
		List<Vertex> verts = new ArrayList<Vertex>();
		synchronized (this) {
			Map<String,Vertex> vmap = getVertexMap();
			for (Vertex v : graph.getVertices()) {
				if (v.isBoundaryVertex())
					continue; // don't highlight boundaries
				// find the vertex corresponding to the selected
				//  subgraph, by name
				Vertex real_v = vmap.get(v.getCoreName());
				if (real_v != null) verts.add(real_v);
			}
		}
		return verts;
	}

	public String getCoreName() {
		return name;
	}

	public void updateCoreName(String name) {
		this.name = name;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public boolean isSaved() {
		return saved;
	}

	public void setSaved(boolean saved) {
		this.saved = saved;
	}

	public void addChangeListener(ChangeListener l) {
		changeListeners.add(l);
	}

	public void fireStateChanged() {
		this.saved = false; // we have changed the graph so it needs to be saved
							// note that if this needs to be TRUE it will be set elsewhere
		synchronized (changeListeners) {
			ChangeEvent evt = new ChangeEvent(this);
			for (ChangeListener l : changeListeners) {
				l.stateChanged(evt);
			}
		}
	}

	public ChangeListener[] getChangeListeners() {
		return changeListeners.toArray(new ChangeListener[changeListeners.size()]);
	}

	public void removeChangeListener(ChangeListener l) {
		changeListeners.remove(l);
	}
}
