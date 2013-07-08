package quanto.core;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;
import javax.swing.event.EventListenerList;
import org.apache.commons.collections15.CollectionUtils;
import org.apache.commons.collections15.Transformer;
import quanto.core.data.*;
import quanto.core.protocol.CoreTalker;

/**
 * Provides a nicer interface to the core
 *
 * @author alex
 */
public class Core {

	private final static Logger logger = Logger.getLogger("quanto.core");
	EventListenerList listenerList = new EventListenerList();
	private CoreTalker talker;
	private Theory activeTheory;
	private Ruleset ruleset;
	private ObjectMapper jsonMapper = new ObjectMapper();

	public Core(CoreTalker talker) throws CoreException {
		this.talker = talker;
		this.ruleset = new Ruleset(this);
	}

	public Core(CoreTalker talker, Theory theory) throws CoreException {
		this(talker);
		talker.changeTheory(theory.getCoreName());
		this.activeTheory = theory;
	}

	public void updateCoreTheory(Theory theory) throws CoreException {
		fireTheoryAboutToChange(theory);
		Theory oldTheory = activeTheory;
		talker.changeTheory(theory.getCoreName());
		this.activeTheory = theory;
		fireTheoryChanged(oldTheory);
	}

	public void addCoreChangeListener(CoreChangeListener l) {
		listenerList.add(CoreChangeListener.class, l);
	}

	public void removeCoreChangeListener(CoreChangeListener l) {
		listenerList.remove(CoreChangeListener.class, l);
	}

	protected void fireTheoryAboutToChange(Theory newTheory) {
		TheoryChangeEvent coreEvent = null;
		Object[] listeners = listenerList.getListenerList();
		for (int i = listeners.length - 2; i >= 0; i -= 2) {
			if (listeners[i] == CoreChangeListener.class) {
				// Lazily create the event:
				if (coreEvent == null) {
					coreEvent = new TheoryChangeEvent(this, activeTheory, newTheory);
				}
				((CoreChangeListener) listeners[i + 1]).theoryAboutToChange(coreEvent);
			}
		}
	}

	protected void fireTheoryChanged(Theory oldTheory) {
		TheoryChangeEvent coreEvent = null;
		Object[] listeners = listenerList.getListenerList();
		for (int i = listeners.length - 2; i >= 0; i -= 2) {
			if (listeners[i] == CoreChangeListener.class) {
				// Lazily create the event:
				if (coreEvent == null) {
					coreEvent = new TheoryChangeEvent(this, oldTheory, activeTheory);
				}
				((CoreChangeListener) listeners[i + 1]).theoryChanged(coreEvent);
			}
		}
	}

	public Theory getActiveTheory() {
		return activeTheory;
	}

	public Ruleset getRuleset() {
		return ruleset;
	}
	private Transformer<CoreObject, String> namer = new Transformer<CoreObject, String>() {
		public String transform(CoreObject i) {
			return i.getCoreName();
		}
	};

	private Collection<String> names(Collection<? extends CoreObject> c) {
		if (c == null) {
			return null;
		}
		return CollectionUtils.collect(c, namer);
	}

	public CoreTalker getTalker() {
		return talker;
	}

	private void assertCoreGraph(CoreGraph graph) {
		if (graph.getCoreName() == null) {
			throw new IllegalStateException("The graph does not have a name");
		}
	}

	public CoreGraph createEmptyGraph() throws CoreException {
		return new CoreGraph(activeTheory, talker.loadEmptyGraph());
	}

	public CoreGraph loadGraph(File location) throws CoreException, IOException {
		CoreGraph g = new CoreGraph(activeTheory, talker.loadGraphFromFile(location.getAbsolutePath()));
		updateGraph(g);
		g.setFileName(location.getAbsolutePath());
		return g;
	}

	public void saveGraph(CoreGraph graph, File location) throws CoreException,
			IOException {
		assertCoreGraph(graph);
		talker.saveGraphToFile(graph.getCoreName(), location.getAbsolutePath());
	}

	public void updateGraph(CoreGraph graph) throws CoreException {
		try {
			String json = talker.exportGraphAsJson(graph.getCoreName());
			JsonNode node = jsonMapper.readValue(json, JsonNode.class);
			graph.updateFromJson(node);
			graph.fireStateChanged();
		} catch (IOException ex) {
			throw new CoreCommunicationException("Failed to parse JSON from core", ex);
		} catch (ParseException ex) {
			throw new CoreCommunicationException("Failed to parse JSON from core", ex);
		}
	}

	public void renameGraph(CoreGraph graph, String suggestedNewName)
			throws CoreException {
		assertCoreGraph(graph);
		graph.updateCoreName(talker.renameGraph(graph.getCoreName(),
				suggestedNewName));
	}

	public void undo(CoreGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.undo(graph.getCoreName());
		updateGraph(graph);
	}

	public void redo(CoreGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.redo(graph.getCoreName());
		updateGraph(graph);
	}

	public void undoRewrite(CoreGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.undoRewrite(graph.getCoreName());
		updateGraph(graph);
	}

	public void redoRewrite(CoreGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.redoRewrite(graph.getCoreName());
		updateGraph(graph);
	}

	public void startUndoGroup(CoreGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.startUndoGroup(graph.getCoreName());
	}

	public void endUndoGroup(CoreGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.endUndoGroup(graph.getCoreName());
	}

	public Vertex addVertex(CoreGraph graph, String vertexType)
			throws CoreException {
		try {
			assertCoreGraph(graph);
			String json = talker.addVertex(graph.getCoreName(), vertexType);
			JsonNode node = jsonMapper.readTree(json);
			Vertex v = Vertex.fromJson(activeTheory, node);
			graph.addVertex(v);
			graph.fireStateChanged();
			return v;
		} catch (IOException ex) {
			throw new CoreCommunicationException("Could not parse JSON from core", ex);
		} catch (ParseException ex) {
			throw new CoreCommunicationException("Could not parse JSON from core", ex);
		}
	}

	public Vertex addBoundaryVertex(CoreGraph graph) throws CoreException {
		return addVertex(graph, "edge-point");
	}

	public void setVertexAngle(CoreGraph graph, Vertex v, String angle)
			throws CoreException {
		assertCoreGraph(graph);
		talker.setVertexData(graph.getCoreName(), v.getCoreName(), angle);
		v.getData().setString(angle);
		graph.fireStateChanged();
	}

	public void deleteVertices(CoreGraph graph, Collection<Vertex> vertices)
			throws CoreException {
		assertCoreGraph(graph);
		talker.deleteVertices(graph.getCoreName(), names(vertices));
		for (Vertex v : vertices) {
			graph.removeVertex(v);
		}
		graph.fireStateChanged();
	}

	public Edge addEdge(CoreGraph graph, boolean directed, Vertex source, Vertex target)
			throws CoreException {
		try {
			assertCoreGraph(graph);
			String json = talker.addEdge(graph.getCoreName(),
					"unit",
					directed,
					source.getCoreName(),
					target.getCoreName());
			JsonNode node = jsonMapper.readTree(json);
			Edge.EdgeData ed = Edge.fromJson(activeTheory, node);

			if (!source.getCoreName().equals(ed.source)) {
				throw new CoreException("Source name from core did not match what we sent");
			}
			if (!target.getCoreName().equals(ed.target)) {
				throw new CoreException("Target name from core did not match what we sent");
			}
			graph.addEdge(ed.edge, source, target);
			graph.fireStateChanged();
			return ed.edge;
		} catch (IOException ex) {
			throw new CoreCommunicationException("Could not parse JSON from core", ex);
		} catch (ParseException ex) {
			throw new CoreCommunicationException("Could not parse JSON from core", ex);
		}
	}

	public void deleteEdges(CoreGraph graph, Collection<Edge> edges)
			throws CoreException {
		assertCoreGraph(graph);
		talker.deleteEdges(graph.getCoreName(), names(edges));
		for (Edge e : edges) {
			graph.removeEdge(e);
		}
		graph.fireStateChanged();
	}
	
	private Collection<Vertex> lookupVertices(CoreGraph graph, Collection<String> vnames) {
		List<Vertex> verts = new ArrayList<Vertex>(vnames.size());
		for (Vertex v : graph.getVertices()) {
			if (vnames.contains(v.getCoreName()))
				verts.add(v);
		}
		return verts;
	}

	public BangBox addBangBox(CoreGraph graph, Collection<Vertex> vertices)
			throws CoreException {
		try {
			assertCoreGraph(graph);
			String bbdesc = talker.addBangBox(graph.getCoreName(), names(vertices));
			JsonNode bbjson = jsonMapper.readTree(bbdesc);
			BangBox.BangBoxData bbdata = BangBox.fromJson(graph.getTheory(), bbjson);
			graph.addBangBox(bbdata.bangBox, lookupVertices(graph, bbdata.contents));
			graph.fireStateChanged();
			return bbdata.bangBox;
		} catch (IOException ex) {
			throw new CoreCommunicationException("Could not parse JSON from core", ex);
		} catch (ParseException ex) {
			throw new CoreCommunicationException("Could not parse JSON from core", ex);
		}
	}

	public Collection<Vertex> bangVertices(CoreGraph graph, BangBox bangBox, Collection<Vertex> vertices)
			throws CoreException {
		assertCoreGraph(graph);
		if (!graph.containsBangBox(bangBox)) {
			throw new IllegalStateException("The graph does not contain that !-box");
		}
		String[] vnames = talker.bangVertices(graph.getCoreName(), bangBox.getCoreName(), names(vertices));
		Collection<Vertex> vs = lookupVertices(graph, Arrays.asList(vnames));
		graph.addVerticesToBangBox(bangBox, vs);
		graph.fireStateChanged();
		return vs;
	}

	public void removeVerticesFromBangBoxes(CoreGraph graph,
			Collection<Vertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		talker.unbangVertices(graph.getCoreName(), names(vertices));
		updateGraph(graph);
	}

	public void dropBangBoxes(CoreGraph graph, Collection<BangBox> bboxen)
			throws CoreException {
		assertCoreGraph(graph);
		talker.dropBangBoxes(graph.getCoreName(), names(bboxen));
		for (BangBox bb : bboxen) {
			graph.removeBangBox(bb);
		}
		graph.fireStateChanged();
	}

	public void killBangBoxes(CoreGraph graph, Collection<BangBox> bboxen)
			throws CoreException {
		assertCoreGraph(graph);
		talker.killBangBoxes(graph.getCoreName(), names(bboxen));
		for (BangBox bb : bboxen) {
			List<Vertex> verts = new ArrayList<Vertex>(graph.getBoxedVertices(bb));
			for (Vertex v : verts) {
				graph.removeVertex(v);
			}
			graph.removeBangBox(bb);
		}
		graph.fireStateChanged();
	}

	public BangBox mergeBangBoxes(CoreGraph graph, Collection<BangBox> bboxen)
			throws CoreException {
		assertCoreGraph(graph);
		BangBox newbb = new BangBox(talker.mergeBangBoxes(graph.getCoreName(),
				names(bboxen)));
		List<Vertex> contents = new LinkedList<Vertex>();
		for (BangBox bb : bboxen) {
			for (Vertex v : graph.getBoxedVertices(bb)) {
				contents.add(v);
			}
			graph.removeBangBox(bb);
		}
		graph.addBangBox(newbb, contents);
		graph.fireStateChanged();
		return newbb;
	}

	public BangBox duplicateBangBox(CoreGraph graph, BangBox bbox)
			throws CoreException {
		assertCoreGraph(graph);
		String name = talker.duplicateBangBox(graph.getCoreName(),
				bbox.getCoreName());
		updateGraph(graph);
		for (BangBox bb : graph.getBangBoxes()) {
			if (bb.getCoreName().equals(name)) {
				return bb;
			}
		}
		return null;
	}

	public void replaceRuleset(File location) throws CoreException, IOException {
		talker.replaceRulesetFromFile(location.getAbsolutePath());
		this.ruleset.reload();
	}

	public void renameBangBox(CoreGraph graph, String oldName, String newName)
			throws CoreException {
		assertCoreGraph(graph);
		talker.renameBangBox(graph.getCoreName(), oldName, newName);
	}

	public void loadRuleset(File location) throws CoreException, IOException {
		talker.importRulesetFromFile(location.getAbsolutePath());
		this.ruleset.reload();
	}

	public void saveRuleset(File location) throws CoreException, IOException {
		talker.exportRulesetToFile(location.getAbsolutePath());
	}

	/**
	 * Creates a rule from two graphs.
	 *
	 * Any existing rule with the same name will be replaced.
	 *
	 * @param ruleName
	 * @param lhs
	 * @param rhs
	 * @return
	 * @throws CoreException
	 */
	public Rule createRule(String ruleName, CoreGraph lhs,
			CoreGraph rhs) throws CoreException {
		assertCoreGraph(lhs);
		assertCoreGraph(rhs);
		talker.setRule(ruleName, lhs.getCoreName(), rhs.getCoreName());
		// FIXME: get actual rule active state from core
		if (!this.ruleset.getRules().contains(ruleName)) {
			this.ruleset.ruleAdded(ruleName, false);
		}
		return new Rule(ruleName, lhs, rhs);
	}

	public Rule openRule(String ruleName) throws CoreException {
		CoreGraph lhs = new CoreGraph(activeTheory, talker.openRuleLhs(ruleName));
		updateGraph(lhs);
		CoreGraph rhs = new CoreGraph(activeTheory, talker.openRuleRhs(ruleName));
		updateGraph(rhs);
		return new Rule(ruleName, lhs, rhs);
	}

	public void saveRule(Rule rule) throws CoreException {
		if (rule.getCoreName() == null) {
			throw new IllegalArgumentException("Rule has no name");
		}
		talker.setRule(rule.getCoreName(), rule.getLhs().getCoreName(),
				rule.getRhs().getCoreName());
	}

	public void fastNormalise(CoreGraph graph) throws CoreException {
		boolean didRewrites = false;
		try {
			int rwCount = talker.attachOneRewrite(graph.getCoreName());
			while (rwCount > 0) {
				talker.applyAttachedRewrite(graph.getCoreName(), 0);
				didRewrites = true;
				rwCount = talker.attachOneRewrite(graph.getCoreName());
			}
		} catch (CoreException e) {
			if (!e.getMessage().contains("No more rewrites.")) {
				throw e;
			}
		}
		if (didRewrites) {
			updateGraph(graph);
		}
	}

	public void cutSubgraph(CoreGraph graph, Collection<Vertex> vertices)
			throws CoreException {
		assertCoreGraph(graph);
		Collection<String> vnames = names(vertices);
		talker.copySubgraphAndOverwrite(graph.getCoreName(), "__clip__", vnames);
		talker.deleteVertices(graph.getCoreName(), vnames);
		for (Vertex v : vertices) {
			graph.removeVertex(v);
		}
		graph.fireStateChanged();
	}

	public void copySubgraph(CoreGraph graph, Collection<Vertex> vertices)
			throws CoreException {
		assertCoreGraph(graph);
		talker.copySubgraphAndOverwrite(graph.getCoreName(), "__clip__", names(vertices));
	}

	public void paste(CoreGraph target) throws CoreException {
		assertCoreGraph(target);
		talker.insertGraph("__clip__", target.getCoreName());
		updateGraph(target);
	}

	public int attachRewrites(CoreGraph graph, Collection<Vertex> vertices)
			throws CoreException {
		return talker.attachRewrites(graph.getCoreName(), names(vertices));
	}

	public boolean attachOneRewrite(CoreGraph graph, Collection<Vertex> vertices)
			throws CoreException {
		return talker.attachOneRewrite(graph.getCoreName(), names(vertices)) > 0;
	}

	public List<AttachedRewrite> getAttachedRewrites(CoreGraph graph)
			throws CoreException {
		try {
			String json = talker.listAttachedRewrites(graph.getCoreName());
			JsonNode rewritesNode = jsonMapper.readTree(json);
			if (!rewritesNode.isArray()) {
				throw new ParseException("Expected array");
			}
			List<AttachedRewrite> rws = new ArrayList<AttachedRewrite>(rewritesNode.size());
			int i = 0;
			for (JsonNode node : rewritesNode) {
				rws.add(AttachedRewrite.fromJson(graph, i, node));
				++i;
			}
			return rws;
		} catch (IOException ex) {
			throw new CoreCommunicationException("Could not parse JSON from core", ex);
		} catch (ParseException ex) {
			throw new CoreCommunicationException("Could not parse JSON from core", ex);
		}
	}

	public void applyAttachedRewrite(CoreGraph graph, int i)
			throws CoreException {
		try {
			String json = talker.applyAttachedRewrite(graph.getCoreName(), i);
			JsonNode node = jsonMapper.readValue(json, JsonNode.class);
			graph.updateFromJson(node);
			graph.fireStateChanged();
		} catch (IOException ex) {
			throw new CoreCommunicationException("Failed to parse JSON from core", ex);
		} catch (ParseException ex) {
			throw new CoreCommunicationException("Failed to parse JSON from core", ex);
		}
	}

	/**
	 * Rename a vertex.
	 *
	 * Note that if a vertex with the new name already exists, that vertex (and
	 * not v) will be given a new name.
	 *
	 * @param graph The graph the vertex is in
	 * @param v The vertex to rename
	 * @param newName The new name for the vertex
	 * @return if a vertex called newName already existed, the new name for that
	 * vertex, otherwise null
	 * @throws CoreException
	 */
	public String renameVertex(CoreGraph graph, Vertex v, String newName)
			throws CoreException {
		String[] names = talker.renameVertex(graph.getCoreName(), v.getCoreName(), newName);
		if (names.length > 1) {
			for (Vertex vv : graph.getVertices()) {
				if (vv.getCoreName().equals(names[0])) {
					vv.updateCoreName(names[1]);
				}
			}
		}
		v.updateCoreName(names[0]);
		graph.fireStateChanged();
		return (names.length > 1) ? names[1] : null;
	}
}
