package quanto.core.data;


import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.MissingNode;
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
import java.util.Iterator;
import quanto.core.ParseException;
import quanto.core.Theory;

public class CoreGraph extends DirectedSparseBangBoxMultigraph<Vertex, Edge, BangBox>
implements CoreObject, ChangeEventSupport {

	private static final long serialVersionUID = -1519901566511300787L;
	private Theory theory;
	private String name;
	private final Set<ChangeListener> changeListeners;
	private Map<String, String> userData = new HashMap<String, String>();
	
	private String fileName = null; // defined if this graph is backed by a file
	private boolean saved = true; // true if this graph has been modified since last saved

	public CoreGraph(Theory theory, String name) {
		this.theory = theory;
		this.name = name;
		this.changeListeners = Collections.synchronizedSet(
				new HashSet<ChangeListener>());
	}

	/**
	 * Use this constructor for unnamed graphs. The idea is you
	 * should do null checks before sending the name to the core.
	 */
	public CoreGraph(Theory theory) {
		this(theory, null);
	}

	public Theory getTheory() {
		return theory;
	}

	public Map<String,Vertex> getVertexMap() {
		Map<String, Vertex> verts =
			new HashMap<String, Vertex>();
		for (Vertex v : getVertices()) {
			verts.put(v.getCoreName(), v);
		}
		return verts;
	}

	public Map<String,Edge> getEdgeMap() {
		Map<String, Edge> edges =
			new HashMap<String, Edge>();
		for (Edge e : getEdges()) {
			edges.put(e.getCoreName(), e);
		}
		return edges;
	}

	public Map<String,BangBox> getBangBoxMap() {
		Map<String, BangBox> bbs =
			new HashMap<String, BangBox>();
		for (BangBox bb : getBangBoxes()) {
			bbs.put(bb.getCoreName(), bb);
		}
		return bbs;
	}
	
	public List<Vertex> getSubgraphVertices(CoreGraph graph) {
		List<Vertex> verts = new ArrayList<Vertex>();
		synchronized (this) {
			Map<String,Vertex> vmap = getVertexMap();
			//Vertices which are not in the new graph
			//Will be highlighted
			for (Vertex v : getVertices()) {
				if (graph.getVertexMap().get(v.getCoreName()) == null){
					verts.add(v);
				}
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

	public Map<String, String> getUserData() {
		return Collections.unmodifiableMap(userData);
	}
	
	public void setUserData(Map<String, String> map) {
		userData = new HashMap<String, String>(map);
	}
	
	public String getUserDataEntry(String k) {
		return userData.get(k);
	}
	
	public void setUserDataEntry(String k, String v) {
		userData.put(k, v);
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
	
	public void updateGraph(CoreGraph new_graph) {
		synchronized (this) {
			for (Vertex v: new_graph.getVertices()) {
				addVertex(v);
			}
		}
	}
	
	private void verticesFromJson(JsonNode node, boolean isWv, Map<String,Vertex> oldVMap, Map<String,Vertex> newVMap) throws ParseException {
		if (node == null || node.isNull())
			return;
		if (node.isArray()) {
			for (JsonNode entry : node) {
				if (!entry.isTextual())
					throw new ParseException("vertex list contained something that was not a string");
				String vname = entry.asText();
				Vertex v = oldVMap.get(vname);
				if (v != null) {
					v.updateFromJson(theory, isWv, MissingNode.getInstance());
					oldVMap.remove(vname);
				} else {
					v = Vertex.fromJson(theory, vname, isWv, MissingNode.getInstance());
					addVertex(v);
				}
				newVMap.put(vname, v);
			}
		} else if (node.isObject()) {
			Iterator<Map.Entry<String,JsonNode>> it = node.fields();
			while (it.hasNext()) {
				Map.Entry<String,JsonNode> entry = it.next();
				String vname = entry.getKey();
				Vertex v = oldVMap.get(vname);
				if (v != null) {
					v.updateFromJson(theory, isWv, entry.getValue());
					oldVMap.remove(vname);
				} else {
					v = Vertex.fromJson(theory, vname, isWv, entry.getValue());
					addVertex(v);
				}
				newVMap.put(vname, v);
			}
		} else {
			throw new ParseException("Vertex list was neither an object nor an array");
		}
	}
	
	private void edgesFromJson(JsonNode node, boolean isDirected, Map<String,Edge> oldEMap, Map<String,Vertex> newVMap) throws ParseException {
		if (node == null || node.isNull())
			return;
		if (!node.isObject())
			throw new ParseException("Edge list was neither an object nor an array");
		Iterator<Map.Entry<String,JsonNode>> it = node.fields();
		while (it.hasNext()) {
			Map.Entry<String,JsonNode> entry = it.next();
			Edge e = oldEMap.get(entry.getKey());
			Edge.EdgeData ed;
			if (e != null) {
				ed = e.updateFromJson(theory, isDirected, entry.getValue());
				if (getSource(e) != newVMap.get(ed.source) || getDest(e) != newVMap.get(ed.target)) {
					removeEdge(e);
					addEdge(ed.edge, newVMap.get(ed.source), newVMap.get(ed.target));
				}
				oldEMap.remove(entry.getKey());
			} else {
				ed = Edge.fromJson(theory, entry.getKey(), isDirected, entry.getValue());
				if (!newVMap.containsKey(ed.source))
					throw new ParseException("Source of edge " + entry.getKey() + " does not exist");
				if (!newVMap.containsKey(ed.target))
					throw new ParseException("Target of edge " + entry.getKey() + " does not exist");
				addEdge(ed.edge, newVMap.get(ed.source), newVMap.get(ed.target));
			}
		}
	}
	
	private void bangBoxesFromJson(JsonNode node, Map<String,BangBox> oldBBMap, Map<String,Vertex> newVMap) throws ParseException {
		if (node == null || node.isNull())
			return;
		if (!node.isObject())
			throw new ParseException("Bang box list was neither an object nor an array");
		Iterator<Map.Entry<String,JsonNode>> it = node.fields();
		while (it.hasNext()) {
			Map.Entry<String,JsonNode> entry = it.next();
			BangBox bb = oldBBMap.get(entry.getKey());
			BangBox.BangBoxData bbd;
			if (bb != null) {
				bbd = bb.updateFromJson(theory, entry.getValue());
				oldBBMap.remove(entry.getKey());
			} else {
				bbd = BangBox.fromJson(theory, entry.getKey(), entry.getValue());
				addBangBox(bbd.bangBox, Collections.EMPTY_SET);
			}
			ArrayList<Vertex> contents = new ArrayList<Vertex>(bbd.contents.size());
			for (String item : bbd.contents) {
				contents.add(newVMap.get(item));
			}
			setBoxedVertices(bbd.bangBox, contents);
			// FIXME: parents
		}
	}
	
	public void updateFromJson(JsonNode node) throws ParseException {
		if (!node.isObject())
			throw new ParseException("Expected object");

		Map<String,Vertex> oldVMap = getVertexMap();
		Map<String,Vertex> newVMap = getVertexMap();
		Map<String,Edge> oldEMap = getEdgeMap();
		Map<String,BangBox> oldBBMap = getBangBoxMap();

		verticesFromJson(node.get("wire_vertices"), true, oldVMap, newVMap);
		verticesFromJson(node.get("node_vertices"), false, oldVMap, newVMap);
		edgesFromJson(node.get("dir_edges"), true, oldEMap, newVMap);
		edgesFromJson(node.get("undir_edges"), false, oldEMap, newVMap);
		bangBoxesFromJson(node.get("bang_boxes"), oldBBMap, newVMap);

		for (BangBox b : oldBBMap.values()) {
			removeBangBox(b);
		}
		for (Edge e : oldEMap.values()) {
			removeEdge(e);
		}
		for (Vertex v : oldVMap.values()) {
			removeVertex(v);
		}

		JsonNode dataNode = node.get("data");
		// FIXME: Graph data

		JsonNode annotationNode = node.get("annotation");
		if (annotationNode != null && annotationNode.isObject()) {
			ObjectMapper mapper = new ObjectMapper();
			setUserData(mapper.<HashMap<String,String>>convertValue(
					annotationNode,
					mapper.getTypeFactory().constructMapType(
					HashMap.class, String.class, String.class)));
		}
	}
	
	public static CoreGraph fromJson(Theory theory, String name, JsonNode node) throws ParseException {
		CoreGraph graph = new CoreGraph(theory, name);
		graph.updateFromJson(node);
		return graph;
	}
}
