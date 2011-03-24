/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import edu.uci.ics.jung.contrib.HasName;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Provides a nicer interface to the core
 *
 * @author alex
 */
public class Core {
	private final static Logger logger =
		LoggerFactory.getLogger(Core.class);

	private CoreTalker talker;

	public Core(CoreTalker talker) {
		this.talker = talker;
	}

	public Core() throws CoreException {
		talker = new CoreConsoleTalker();
	}

	private String[] names(Collection<? extends HasName> c) {
		String[] ns = new String[c.size()];
		int i = 0;
		for (HasName n : c) {
			ns[i] = n.getName();
			++i;
		}
		return ns;
	}

	public CoreTalker getTalker() {
		return talker;
	}

	private void assertCoreGraph(QuantoGraph graph) {
		if (graph.getName() == null)
			throw new IllegalStateException("The graph does not have a name");
	}

	public QuantoGraph createEmptyGraph() throws CoreException {
		return new QuantoGraph(talker.new_graph());
	}

	public QuantoGraph loadGraph(File location) throws CoreException, IOException {
		QuantoGraph g = new QuantoGraph(talker.load_graph(location));
		updateGraph(g);
		g.setFileName(location.getAbsolutePath());
		return g;
	}

	public void saveGraph(QuantoGraph graph, File location) throws CoreException, IOException {
		assertCoreGraph(graph);
		talker.save_graph(graph.getName(), location);
	}

	public void updateGraph(QuantoGraph graph) throws CoreException {
		String xml = talker.graph_xml(graph.getName());
		try {
			graph.fromXml(xml);
		}
		catch (QuantoGraph.ParseException ex) {
			throw new BadResponseException("Could not parse the graph XML from the core", xml);
		}
	}

	public enum RepresentationType {
		Plain,
		Latex,
		Mathematica,
		Matlab
	}

	public String hilbertSpaceRepresentation(QuantoGraph graph, RepresentationType format) throws CoreException {
		return talker.hilb(graph.getName(), format.toString().toLowerCase());
	}

	public void renameGraph(QuantoGraph graph, String suggestedNewName) throws CoreException {
		assertCoreGraph(graph);
		graph.setName(talker.rename_graph(graph.getName(), suggestedNewName));
	}

	public void forgetGraph(QuantoGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.kill_graph(graph.getName());
		graph.setName(null);
	}

	public void undo(QuantoGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.undo(graph.getName());
	}

	public void redo(QuantoGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.redo(graph.getName());
	}

	public QVertex addVertex(QuantoGraph graph, QVertex.Type type) throws CoreException {
		assertCoreGraph(graph);
		QVertex v = new QVertex(talker.add_vertex(graph.getName(), type), type);
		graph.addVertex(v);
		graph.fireStateChanged();
		return v;
	}

	public void flipVertices(QuantoGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		talker.flip_vertices(graph.getName(), names(vertices));
		for (QVertex v : vertices) {
			if (v.getVertexType() == QVertex.Type.RED)
				v.setVertexType(QVertex.Type.GREEN);
			else if (v.getVertexType() == QVertex.Type.GREEN)
				v.setVertexType(QVertex.Type.RED);
		}
		graph.fireStateChanged();
	}

	public void renameVertex(QuantoGraph graph, QVertex vertex, String suggestedNewName) throws CoreException {
		assertCoreGraph(graph);
		vertex.setName(talker.rename_vertex(graph.getName(), vertex.getName(), suggestedNewName));
	}

	public void setVertexAngle(QuantoGraph graph, QVertex v, String angle) throws CoreException {
		assertCoreGraph(graph);
		talker.set_angle(graph.getName(), v.getName(), angle);
		v.setLabel(angle);
		graph.fireStateChanged();
	}

	public void deleteVertices(QuantoGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		talker.delete_vertices(graph.getName(), names(vertices));
		for (QVertex v : vertices) {
			graph.removeVertex(v);
		}
		graph.fireStateChanged();
	}

	public QEdge addEdge(QuantoGraph graph, QVertex source, QVertex target) throws CoreException {
		assertCoreGraph(graph);
		String eName = talker.add_edge(graph.getName(), source.getName(), target.getName());
		QEdge e = new QEdge(eName);
		graph.addEdge(e, source, target);
		graph.fireStateChanged();
		return e;
	}

	public void deleteEdges(QuantoGraph graph, Collection<QEdge> edges) throws CoreException {
		assertCoreGraph(graph);
		talker.delete_edges(graph.getName(), names(edges));
		for (QEdge e : edges) {
			graph.removeEdge(e);
		}
		graph.fireStateChanged();
	}

	public BangBox addBangBox(QuantoGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		BangBox bb = new BangBox(talker.add_bang(graph.getName()));
		if (vertices != null && vertices.size() > 0) {
			talker.bang_vertices(graph.getName(), bb.getName(), names(vertices));
			for (QVertex v : vertices) {
				bb.add(v);
			}
		}
		graph.addBangBox(bb);
		graph.fireStateChanged();
		return bb;
	}

	public void removeVerticesFromBangBoxes(QuantoGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		talker.unbang_vertices(graph.getName(), names(vertices));
		updateGraph(graph);
	}

	public void dropBangBoxes(QuantoGraph graph, Collection<BangBox> bboxen) throws CoreException {
		assertCoreGraph(graph);
		talker.bbox_drop(graph.getName(), names(bboxen));
		for (BangBox bb : bboxen) {
			graph.removeBangBox(bb);
		}
		graph.fireStateChanged();
	}

	public void killBangBoxes(QuantoGraph graph, Collection<BangBox> bboxen) throws CoreException {
		assertCoreGraph(graph);
		talker.bbox_kill(graph.getName(), names(bboxen));
		for (BangBox bb : bboxen) {
			for (QVertex v : bb) {
				graph.removeVertex(v);
			}
			graph.removeBangBox(bb);
		}
		graph.fireStateChanged();
	}

	public BangBox mergeBangBoxes(QuantoGraph graph, Collection<BangBox> bboxen) throws CoreException {
		assertCoreGraph(graph);
		BangBox newbb = new BangBox(talker.bbox_merge(graph.getName(), names(bboxen)));
		for (BangBox bb : bboxen) {
			for (QVertex v : bb) {
				newbb.add(v);
			}
			graph.removeBangBox(bb);
		}
		graph.addBangBox(newbb);
		graph.fireStateChanged();
		return newbb;
	}

	public BangBox duplicateBangBox(QuantoGraph graph, BangBox bbox) throws CoreException {
		assertCoreGraph(graph);
		String name = talker.bbox_duplicate(graph.getName(), bbox.getName());
		updateGraph(graph);
		graph.fireStateChanged();
		for (BangBox bb : graph.getBangBoxes()) {
			if (bb.getName().equals(name))
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
			BufferedReader r = new BufferedReader(new FileReader(file));
			try
			{
				StringBuilder result = new StringBuilder();
				int c = 0;
				while (c != -1) {
					c = r.read();
					result.append((char)c);
				}
				return result.toString();
			} finally {
				r.close();
			}
		} finally {
			file.delete();
		}
	}

	public Rewrite createRule(String ruleName, QuantoGraph lhs, QuantoGraph rhs) throws CoreException {
		assertCoreGraph(lhs);
		assertCoreGraph(rhs);
		talker.new_rule(ruleName, lhs.getName(), rhs.getName());
		return new Rewrite(ruleName, lhs, rhs);
	}

	public Rewrite openRule(String ruleName) throws CoreException {
		QuantoGraph lhs = new QuantoGraph(talker.open_rule_lhs(ruleName));
		updateGraph(lhs);
		QuantoGraph rhs = new QuantoGraph(talker.open_rule_rhs(ruleName));
		updateGraph(rhs);
		return new Rewrite(ruleName, lhs, rhs);
	}

	public void saveRule(Rewrite rule) throws CoreException {
		if (rule.getName() == null)
			throw new IllegalArgumentException("Rewrite has no name");
		talker.update_rule(rule.getName(), rule.getLhs().getName(), rule.getRhs().getName());
	}

	/*
	 * Derived methods, note these are in CamelCase to emphasise that they
	 * are not actual core commands.
	 */
	public void fastNormalise(QuantoGraph graph) throws CoreException {
		boolean didRewrites = false;
		try {
			while (true) {
				talker.apply_first_rewrite(graph.getName());
				didRewrites = true;
			}
		} catch (CoreException e) {
			if (! e.getMessage().contains("No more rewrites.")) throw e;
		}
		if (didRewrites)
			updateGraph(graph);
	}

	public void cutSubgraph(QuantoGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		String[] vnames = names(vertices);
		talker.copy_subgraph(graph.getName(), "__clip__", vnames);
		talker.delete_vertices(graph.getName(), vnames);
		for (QVertex v : vertices) {
			graph.removeVertex(v);
		}
		graph.fireStateChanged();
	}

	public void copySubgraph(QuantoGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		String[] vnames = names(vertices);
		talker.copy_subgraph(graph.getName(), "__clip__", vnames);
	}

	public void paste(QuantoGraph target) throws CoreException {
		assertCoreGraph(target);
		talker.insert_graph("__clip__", target.getName());
		updateGraph(target);
	}

	public void attachRewrites(QuantoGraph graph, Collection<QVertex> vertices) throws CoreException {
		talker.attach_rewrites(graph.getName(), names(vertices));
	}

	public void attachOneRewrite(QuantoGraph graph, Collection<QVertex> vertices) throws CoreException {
		talker.attach_one_rewrite(graph.getName(), names(vertices));
	}

	public List<Rewrite> getAttachedRewrites(QuantoGraph graph) throws CoreException {
		try {
			String xml = talker.show_rewrites(graph.getName());
			return Rewrite.parseRewrites(xml);
		} catch (QuantoGraph.ParseException ex) {
			throw new BadResponseException("Core gave bad rewrite XML");
		}
	}

	public void applyAttachedRewrite(QuantoGraph graph, int i) throws CoreException {
		talker.apply_rewrite(graph.getName(), i);
	}
}
