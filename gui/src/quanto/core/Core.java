package quanto.core;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import net.n3.nanoxml.IXMLElement;
import net.n3.nanoxml.IXMLParser;
import net.n3.nanoxml.StdXMLBuilder;
import net.n3.nanoxml.StdXMLReader;
import net.n3.nanoxml.XMLException;
import net.n3.nanoxml.XMLParserFactory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import quanto.core.data.AttachedRewrite;
import quanto.core.data.BangBox;
import quanto.core.data.CoreGraph;
import quanto.core.data.CoreObject;
import quanto.core.data.Edge;
import quanto.core.data.Rule;
import quanto.core.data.Vertex;
import quanto.core.data.VertexType;
import quanto.util.FileUtils;

/**
 * Provides a nicer interface to the core
 * 
 * @author alex
 */
public class Core {

	private final static Logger logger = LoggerFactory.getLogger(Core.class);

	private class CoreTheory implements Theory {
		Map<String, VertexType> types = new HashMap<String, VertexType>();

		public VertexType getVertexType(String typeName) {
			return types.get(typeName);
		}

		public Collection<VertexType> getVertexTypes() {
			return types.values();
		}

		void addVertexType(VertexType type) {
			types.put(type.getTypeName(), type);
		}
	}

	private CoreTalker talker;
	private GraphBuilder graphBuilder;
	private CoreTheory activeTheory;
    private Ruleset ruleset;

	public Core(CoreTalker talker) {
		this.talker = talker;
		this.activeTheory = new CoreTheory();
		activeTheory.addVertexType(new VertexType.Red());
		activeTheory.addVertexType(new VertexType.Green());
		activeTheory.addVertexType(new VertexType.Hadamard());
		this.graphBuilder = new GraphBuilder(activeTheory);
        this.ruleset = new Ruleset(this);
	}

	public Core() throws CoreException {
		this(new CoreConsoleTalker());
	}

	public Theory getActiveTheory() {
		return activeTheory;
	}

	public Ruleset getRuleset() {
		return ruleset;
	}

	private String[] names(Collection<? extends CoreObject> c) {
		String[] ns = new String[c.size()];
		int i = 0;
		for (CoreObject n : c) {
			ns[i] = n.getCoreName();
			++i;
		}
		return ns;
	}
	
	protected List<AttachedRewrite<CoreGraph>> parseRewrites(CoreGraph graph,
			String xml) throws ParseException {
		List<AttachedRewrite<CoreGraph>> rewrites = new ArrayList<AttachedRewrite<CoreGraph>>();
		try {
			IXMLParser parser = XMLParserFactory
					.createDefaultXMLParser(new StdXMLBuilder());
			parser.setReader(StdXMLReader.stringReader(xml));
			IXMLElement root = (IXMLElement) parser.parse();
			int i = 0;
			for (Object obj : root.getChildrenNamed("rewrite")) {
				IXMLElement rw = (IXMLElement) obj;
				IXMLElement ruleName = rw.getFirstChildNamed("rulename");
				if (ruleName == null)
					throw new XMLException(
							"<rewrite> must have a <rulename> element");
				IXMLElement lhs = rw.getFirstChildNamed("lhs")
						.getFirstChildNamed("graph");
				IXMLElement rhs = rw.getFirstChildNamed("rhs")
						.getFirstChildNamed("graph");
				rewrites.add(new AttachedRewrite<CoreGraph>(graph, i, ruleName
						.getContent(), graphBuilder.createGraphFromXml(null,
						lhs), graphBuilder.createGraphFromXml(null, rhs)));
				++i;

			}
		} catch (XMLException e) {
			throw new ParseException(e.getMessage(), e);
		} catch (ClassNotFoundException e) {
			throw new RuntimeException(e);
		} catch (InstantiationException e) {
			throw new RuntimeException(e);
		} catch (IllegalAccessException e) {
			throw new RuntimeException(e);
		}
		return rewrites;
	}

	public CoreTalker getTalker() {
		return talker;
	}

	private void assertCoreGraph(CoreGraph graph) {
		if (graph.getCoreName() == null)
			throw new IllegalStateException("The graph does not have a name");
	}

	public CoreGraph createEmptyGraph() throws CoreException {
		return new CoreGraph(talker.new_graph());
	}

	public CoreGraph loadGraph(File location) throws CoreException, IOException {
		CoreGraph g = new CoreGraph(talker.load_graph(location));
		updateGraph(g);
		g.setFileName(location.getAbsolutePath());
		return g;
	}

	public void saveGraph(CoreGraph graph, File location) throws CoreException,
			IOException {
		assertCoreGraph(graph);
		talker.save_graph(graph.getCoreName(), location);
	}

	public void updateGraph(CoreGraph graph) throws CoreException {
		String xml = talker.graph_xml(graph.getCoreName());
		try {
			graphBuilder.updateGraphFromXml(graph, xml);
		} catch (ParseException ex) {
			logger.error("Failed to parse from core", ex);
			throw new BadResponseException(
					"Could not parse the graph XML from the core", xml);
		}
	}

	public enum RepresentationType {
		Plain, Latex, Mathematica, Matlab
	}

	public String hilbertSpaceRepresentation(CoreGraph graph,
			RepresentationType format) throws CoreException {
		return talker
				.hilb(graph.getCoreName(), format.toString().toLowerCase());
	}

	public void renameGraph(CoreGraph graph, String suggestedNewName)
			throws CoreException {
		assertCoreGraph(graph);
		graph.updateCoreName(talker.rename_graph(graph.getCoreName(),
				suggestedNewName));
	}

	public void forgetGraph(CoreGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.kill_graph(graph.getCoreName());
		graph.updateCoreName(null);
	}

	public void undo(CoreGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.undo(graph.getCoreName());
	}

	public void redo(CoreGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.redo(graph.getCoreName());
	}

	public Vertex addVertex(CoreGraph graph, VertexType vertexType)
			throws CoreException {
		assertCoreGraph(graph);
		Vertex v = Vertex.createVertex(talker.add_vertex(graph.getCoreName(), vertexType.getTypeName()), vertexType);
		graph.addVertex(v);
		graph.fireStateChanged();
		return v;
	}

	public Vertex addVertex(CoreGraph graph, String vertexType)
			throws CoreException {
		return addVertex(graph, activeTheory.getVertexType(vertexType));
	}

	public Vertex addBoundaryVertex(CoreGraph graph) throws CoreException {
		assertCoreGraph(graph);
		Vertex v = Vertex.createBoundaryVertex(talker.add_vertex(graph.getCoreName(),
				"boundary"));
		graph.addVertex(v);
		graph.fireStateChanged();
		return v;
	}

	public void renameVertex(CoreGraph graph, Vertex vertex,
			String suggestedNewName) throws CoreException {
		assertCoreGraph(graph);
		vertex.updateCoreName(talker.rename_vertex(graph.getCoreName(),
				vertex.getCoreName(), suggestedNewName));
	}

	public void setVertexAngle(CoreGraph graph, Vertex v, String angle)
			throws CoreException {
		assertCoreGraph(graph);
		talker.set_angle(graph.getCoreName(), v.getCoreName(), angle);
		v.getData().setValue(angle);
		graph.fireStateChanged();
	}

	public void deleteVertices(CoreGraph graph, Collection<Vertex> vertices)
			throws CoreException {
		assertCoreGraph(graph);
		talker.delete_vertices(graph.getCoreName(), names(vertices));
		for (Vertex v : vertices) {
			graph.removeVertex(v);
		}
		graph.fireStateChanged();
	}

	public Edge addEdge(CoreGraph graph, Vertex source, Vertex target)
			throws CoreException {
		assertCoreGraph(graph);
		String eName = talker.add_edge(graph.getCoreName(),
				source.getCoreName(), target.getCoreName());
		Edge e = new Edge(eName);
		graph.addEdge(e, source, target);
		graph.fireStateChanged();
		return e;
	}

	public void deleteEdges(CoreGraph graph, Collection<Edge> edges)
			throws CoreException {
		assertCoreGraph(graph);
		talker.delete_edges(graph.getCoreName(), names(edges));
		for (Edge e : edges) {
			graph.removeEdge(e);
		}
		graph.fireStateChanged();
	}

	public BangBox addBangBox(CoreGraph graph, Collection<Vertex> vertices)
			throws CoreException {
		assertCoreGraph(graph);
		BangBox bb = new BangBox(talker.add_bang(graph.getCoreName()));
		if (vertices.size() > 0) {
			talker.bang_vertices(graph.getCoreName(), bb.getCoreName(),
					names(vertices));
		}
		graph.addBangBox(bb, vertices);
		graph.fireStateChanged();
		return bb;
	}

	public void removeVerticesFromBangBoxes(CoreGraph graph,
			Collection<Vertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		talker.unbang_vertices(graph.getCoreName(), names(vertices));
		updateGraph(graph);
	}

	public void dropBangBoxes(CoreGraph graph, Collection<BangBox> bboxen)
			throws CoreException {
		assertCoreGraph(graph);
		talker.bbox_drop(graph.getCoreName(), names(bboxen));
		for (BangBox bb : bboxen) {
			graph.removeBangBox(bb);
		}
		graph.fireStateChanged();
	}

	public void killBangBoxes(CoreGraph graph, Collection<BangBox> bboxen)
			throws CoreException {
		assertCoreGraph(graph);
		talker.bbox_kill(graph.getCoreName(), names(bboxen));
		for (BangBox bb : bboxen) {
			for (Vertex v : graph.getBoxedVertices(bb)) {
				graph.removeVertex(v);
			}
			graph.removeBangBox(bb);
		}
		graph.fireStateChanged();
	}

	public BangBox mergeBangBoxes(CoreGraph graph, Collection<BangBox> bboxen)
			throws CoreException {
		assertCoreGraph(graph);
		BangBox newbb = new BangBox(talker.bbox_merge(graph.getCoreName(),
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
		String name = talker.bbox_duplicate(graph.getCoreName(),
				bbox.getCoreName());
		updateGraph(graph);
		graph.fireStateChanged();
		for (BangBox bb : graph.getBangBoxes()) {
			if (bb.getCoreName().equals(name))
				return bb;
		}
		return null;
	}

	public void loadRuleset(File location) throws CoreException, IOException {
		talker.load_ruleset(location);
		this.ruleset.fireStateChanged();
	}

	public void loadRuleset(String ruleset) throws CoreException, IOException {
		File file = File.createTempFile("quanto", "rules");
		try {
			BufferedWriter w = new BufferedWriter(new FileWriter(file));
			w.write(ruleset);
			w.close();
			talker.load_ruleset(file);
			this.ruleset.fireStateChanged();
		} finally {
			file.delete();
		}
	}

	public void saveRuleset(File location) throws CoreException, IOException {
		talker.save_ruleset(location);
	}

	public String getRulesetEncoded() throws CoreException, IOException {
		File file = File.createTempFile("quanto", "rules");
		try {
			talker.save_ruleset(file);
			return FileUtils.slurp(file);
		} finally {
			file.delete();
		}
	}

	public Rule<CoreGraph> createRule(String ruleName, CoreGraph lhs,
			CoreGraph rhs) throws CoreException {
		assertCoreGraph(lhs);
		assertCoreGraph(rhs);
		talker.new_rule(ruleName, lhs.getCoreName(), rhs.getCoreName());
		this.ruleset.fireStateChanged();
		return new Rule<CoreGraph>(ruleName, lhs, rhs);
	}

	public Rule<CoreGraph> openRule(String ruleName) throws CoreException {
		CoreGraph lhs = new CoreGraph(talker.open_rule_lhs(ruleName));
		updateGraph(lhs);
		CoreGraph rhs = new CoreGraph(talker.open_rule_rhs(ruleName));
		updateGraph(rhs);
		return new Rule<CoreGraph>(ruleName, lhs, rhs);
	}

	public void saveRule(Rule<CoreGraph> rule) throws CoreException {
		if (rule.getCoreName() == null)
			throw new IllegalArgumentException("Rule has no name");
		talker.update_rule(rule.getCoreName(), rule.getLhs().getCoreName(),
				rule.getRhs().getCoreName());
	}

	/*
	 * Derived methods, note these are in CamelCase to emphasise that they are
	 * not actual core commands.
	 */
	public void fastNormalise(CoreGraph graph) throws CoreException {
		boolean didRewrites = false;
		try {
			while (true) {
				talker.apply_first_rewrite(graph.getCoreName());
				didRewrites = true;
			}
		} catch (CoreException e) {
			if (!e.getMessage().contains("No more rewrites."))
				throw e;
		}
		if (didRewrites)
			updateGraph(graph);
	}

	public void cutSubgraph(CoreGraph graph, Collection<Vertex> vertices)
			throws CoreException {
		assertCoreGraph(graph);
		String[] vnames = names(vertices);
		talker.copy_subgraph(graph.getCoreName(), "__clip__", vnames);
		talker.delete_vertices(graph.getCoreName(), vnames);
		for (Vertex v : vertices) {
			graph.removeVertex(v);
		}
		graph.fireStateChanged();
	}

	public void copySubgraph(CoreGraph graph, Collection<Vertex> vertices)
			throws CoreException {
		assertCoreGraph(graph);
		String[] vnames = names(vertices);
		talker.copy_subgraph(graph.getCoreName(), "__clip__", vnames);
	}

	public void paste(CoreGraph target) throws CoreException {
		assertCoreGraph(target);
		talker.insert_graph("__clip__", target.getCoreName());
		updateGraph(target);
	}

	public void attachRewrites(CoreGraph graph, Collection<Vertex> vertices)
			throws CoreException {
		talker.attach_rewrites(graph.getCoreName(), names(vertices));
	}

	public void attachOneRewrite(CoreGraph graph, Collection<Vertex> vertices)
			throws CoreException {
		talker.attach_one_rewrite(graph.getCoreName(), names(vertices));
	}

	public List<AttachedRewrite<CoreGraph>> getAttachedRewrites(CoreGraph graph)
			throws CoreException {
		try {
			String xml = talker.show_rewrites(graph.getCoreName());
			return parseRewrites(graph, xml);
		} catch (ParseException ex) {
			throw new BadResponseException("Core gave bad rewrite XML");
		}
	}

	public void applyAttachedRewrite(CoreGraph graph, int i)
			throws CoreException {
		talker.apply_rewrite(graph.getCoreName(), i);
	}
}
