/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import net.n3.nanoxml.IXMLElement;
import net.n3.nanoxml.IXMLParser;
import net.n3.nanoxml.StdXMLBuilder;
import net.n3.nanoxml.StdXMLReader;
import net.n3.nanoxml.XMLException;
import net.n3.nanoxml.XMLParserFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import quanto.util.FileUtils;

/**
 * Provides a nicer interface to the core
 *
 * @author alex
 */
public class Core<G extends CoreGraph<V,E,B>,
	          V extends CoreVertex,
	          E extends CoreObject,
	          B extends CoreObject> {

	private final static Logger logger =
		LoggerFactory.getLogger(Core.class);

	private CoreTalker talker;
	private GraphFactory<G,V,E,B> graphFactory;

	public Core(CoreTalker talker, GraphFactory<G,V,E,B> graphFactory) {
		this.talker = talker;
		this.graphFactory = graphFactory;
	}

	public Core(GraphFactory<G,V,E,B> graphFactory) throws CoreException {
		this(new CoreConsoleTalker(), graphFactory);
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

	protected List<AttachedRewrite<G>> parseRewrites(G graph, String xml) throws ParseException {
		List<AttachedRewrite<G>> rewrites = new ArrayList<AttachedRewrite<G>>();
		try {
			IXMLParser parser = XMLParserFactory.createDefaultXMLParser(new StdXMLBuilder());
			parser.setReader(StdXMLReader.stringReader(xml));
			IXMLElement root = (IXMLElement)parser.parse();
			int i = 0;
			for (Object obj : root.getChildrenNamed("rewrite")) {
				IXMLElement rw = (IXMLElement)obj;
				IXMLElement ruleName = rw.getFirstChildNamed("rulename");
				if (ruleName == null)
					throw new XMLException("<rewrite> must have a <rulename> element");
				IXMLElement lhs = rw.getFirstChildNamed("lhs")
					.getFirstChildNamed("graph");
				IXMLElement rhs = rw.getFirstChildNamed("rhs")
					.getFirstChildNamed("graph");
				rewrites.add(new AttachedRewrite<G>(
						graph,
						i,
						ruleName.getContent(),
						graphFactory.createGraphFromXml(null, lhs),
						graphFactory.createGraphFromXml(null, rhs)
					));
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

	public G createEmptyGraph() throws CoreException {
		return graphFactory.createGraph(talker.new_graph());
	}

	public G loadGraph(File location) throws CoreException, IOException {
		G g = graphFactory.createGraph(talker.load_graph(location));
		updateGraph(g);
		g.setFileName(location.getAbsolutePath());
		return g;
	}

	public void saveGraph(G graph, File location) throws CoreException, IOException {
		assertCoreGraph(graph);
		talker.save_graph(graph.getCoreName(), location);
	}

	public void updateGraph(G graph) throws CoreException {
		String xml = talker.graph_xml(graph.getCoreName());
		try {
			graphFactory.updateGraphFromXml(graph, xml);
		}
		catch (ParseException ex) {
			throw new BadResponseException("Could not parse the graph XML from the core", xml);
		}
	}

	public enum RepresentationType {
		Plain,
		Latex,
		Mathematica,
		Matlab
	}

	public String hilbertSpaceRepresentation(G graph, RepresentationType format) throws CoreException {
		return talker.hilb(graph.getCoreName(), format.toString().toLowerCase());
	}

	public void renameGraph(G graph, String suggestedNewName) throws CoreException {
		assertCoreGraph(graph);
		graph.updateCoreName(talker.rename_graph(graph.getCoreName(), suggestedNewName));
	}

	public void forgetGraph(G graph) throws CoreException {
		assertCoreGraph(graph);
		talker.kill_graph(graph.getCoreName());
		graph.updateCoreName(null);
	}

	public void undo(G graph) throws CoreException {
		assertCoreGraph(graph);
		talker.undo(graph.getCoreName());
	}

	public void redo(G graph) throws CoreException {
		assertCoreGraph(graph);
		talker.redo(graph.getCoreName());
	}

	public V addVertex(G graph, String vertexType) throws CoreException {
		assertCoreGraph(graph);
		V v = graphFactory.createVertex(talker.add_vertex(graph.getCoreName(), vertexType), vertexType);
		graph.addVertex(v);
		graph.fireStateChanged();
		return v;
	}

	public void renameVertex(G graph, V vertex, String suggestedNewName) throws CoreException {
		assertCoreGraph(graph);
		vertex.updateCoreName(talker.rename_vertex(graph.getCoreName(), vertex.getCoreName(), suggestedNewName));
	}

	public void setVertexAngle(RGGraph graph, RGVertex v, String angle) throws CoreException {
		assertCoreGraph(graph);
		talker.set_angle(graph.getCoreName(), v.getCoreName(), angle);
		v.setLabel(angle);
		graph.fireStateChanged();
	}

	public void deleteVertices(G graph, Collection<V> vertices) throws CoreException {
		assertCoreGraph(graph);
		talker.delete_vertices(graph.getCoreName(), names(vertices));
		for (V v : vertices) {
			graph.removeVertex(v);
		}
		graph.fireStateChanged();
	}

	public E addEdge(G graph, V source, V target) throws CoreException {
		assertCoreGraph(graph);
		String eName = talker.add_edge(graph.getCoreName(), source.getCoreName(), target.getCoreName());
		E e = graphFactory.createEdge(eName);
		graph.addEdge(e, source, target);
		graph.fireStateChanged();
		return e;
	}

	public void deleteEdges(G graph, Collection<E> edges) throws CoreException {
		assertCoreGraph(graph);
		talker.delete_edges(graph.getCoreName(), names(edges));
		for (E e : edges) {
			graph.removeEdge(e);
		}
		graph.fireStateChanged();
	}

	public B addBangBox(G graph, Collection<V> vertices) throws CoreException {
		assertCoreGraph(graph);
		B bb = graphFactory.createBangBox(talker.add_bang(graph.getCoreName()));
		if (vertices.size() > 0) {
			talker.bang_vertices(graph.getCoreName(), bb.getCoreName(), names(vertices));
		}
		graph.addBangBox(bb, vertices);
		graph.fireStateChanged();
		return bb;
	}

	public void removeVerticesFromBangBoxes(G graph, Collection<V> vertices) throws CoreException {
		assertCoreGraph(graph);
		talker.unbang_vertices(graph.getCoreName(), names(vertices));
		updateGraph(graph);
	}

	public void dropBangBoxes(G graph, Collection<B> bboxen) throws CoreException {
		assertCoreGraph(graph);
		talker.bbox_drop(graph.getCoreName(), names(bboxen));
		for (B bb : bboxen) {
			graph.removeBangBox(bb);
		}
		graph.fireStateChanged();
	}

	public void killBangBoxes(G graph, Collection<B> bboxen) throws CoreException {
		assertCoreGraph(graph);
		talker.bbox_kill(graph.getCoreName(), names(bboxen));
		// FIXME: this is inefficient for multiple overlapping !-boxes
		for (B bb : bboxen) {
			for (V v : graph.getBoxedVertices(bb)) {
				graph.removeVertex(v);
			}
			graph.removeBangBox(bb);
		}
		graph.fireStateChanged();
	}

	public B mergeBangBoxes(G graph, Collection<B> bboxen) throws CoreException {
		assertCoreGraph(graph);
		B newbb = graphFactory.createBangBox(talker.bbox_merge(graph.getCoreName(), names(bboxen)));
		List<V> contents = new LinkedList<V>();
		for (B bb : bboxen) {
			for (V v : graph.getBoxedVertices(bb)) {
				contents.add(v);
			}
			graph.removeBangBox(bb);
		}
		graph.addBangBox(newbb, contents);
		graph.fireStateChanged();
		return newbb;
	}

	public B duplicateBangBox(G graph, B bbox) throws CoreException {
		assertCoreGraph(graph);
		String name = talker.bbox_duplicate(graph.getCoreName(), bbox.getCoreName());
		updateGraph(graph);
		graph.fireStateChanged();
		for (B bb : graph.getBangBoxes()) {
			if (bb.getCoreName().equals(name))
				return bb;
		}
		return null;
	}

	public void loadRuleset(File location) throws CoreException, IOException {
		talker.load_ruleset(location);
	}

	public void loadRuleset(String ruleset) throws CoreException, IOException {
		File file = File.createTempFile("quanto", "rules");
		try {
			BufferedWriter w = new BufferedWriter(new FileWriter(file));
			w.write(ruleset);
			w.close();
			talker.load_ruleset(file);
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

	public Rule<G> createRule(String ruleName, G lhs, G rhs) throws CoreException {
		assertCoreGraph(lhs);
		assertCoreGraph(rhs);
		talker.new_rule(ruleName, lhs.getCoreName(), rhs.getCoreName());
		return new Rule<G>(ruleName, lhs, rhs);
	}

	public Rule<G> openRule(String ruleName) throws CoreException {
		G lhs = graphFactory.createGraph(talker.open_rule_lhs(ruleName));
		updateGraph(lhs);
		G rhs = graphFactory.createGraph(talker.open_rule_rhs(ruleName));
		updateGraph(rhs);
		return new Rule<G>(ruleName, lhs, rhs);
	}

	public void saveRule(Rule<G> rule) throws CoreException {
		if (rule.getCoreName() == null)
			throw new IllegalArgumentException("Rule has no name");
		talker.update_rule(rule.getCoreName(), rule.getLhs().getCoreName(), rule.getRhs().getCoreName());
	}

	/*
	 * Derived methods, note these are in CamelCase to emphasise that they
	 * are not actual core commands.
	 */
	public void fastNormalise(G graph) throws CoreException {
		boolean didRewrites = false;
		try {
			while (true) {
				talker.apply_first_rewrite(graph.getCoreName());
				didRewrites = true;
			}
		} catch (CoreException e) {
			if (! e.getMessage().contains("No more rewrites.")) throw e;
		}
		if (didRewrites)
			updateGraph(graph);
	}

	public void cutSubgraph(G graph, Collection<V> vertices) throws CoreException {
		assertCoreGraph(graph);
		String[] vnames = names(vertices);
		talker.copy_subgraph(graph.getCoreName(), "__clip__", vnames);
		talker.delete_vertices(graph.getCoreName(), vnames);
		for (V v : vertices) {
			graph.removeVertex(v);
		}
		graph.fireStateChanged();
	}

	public void copySubgraph(G graph, Collection<V> vertices) throws CoreException {
		assertCoreGraph(graph);
		String[] vnames = names(vertices);
		talker.copy_subgraph(graph.getCoreName(), "__clip__", vnames);
	}

	public void paste(G target) throws CoreException {
		assertCoreGraph(target);
		talker.insert_graph("__clip__", target.getCoreName());
		updateGraph(target);
	}

	public void attachRewrites(G graph, Collection<V> vertices) throws CoreException {
		talker.attach_rewrites(graph.getCoreName(), names(vertices));
	}

	public void attachOneRewrite(G graph, Collection<V> vertices) throws CoreException {
		talker.attach_one_rewrite(graph.getCoreName(), names(vertices));
	}

	public List<AttachedRewrite<G>> getAttachedRewrites(G graph) throws CoreException {
		try {
			String xml = talker.show_rewrites(graph.getCoreName());
			return parseRewrites(graph, xml);
		} catch (ParseException ex) {
			throw new BadResponseException("Core gave bad rewrite XML");
		}
	}

	public void applyAttachedRewrite(G graph, int i) throws CoreException {
		talker.apply_rewrite(graph.getCoreName(), i);
	}
}
