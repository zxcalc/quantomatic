/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import edu.uci.ics.jung.contrib.HasName;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import quanto.Util;

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

	private void assertCoreGraph(QGraph graph) {
		if (graph.getName() == null)
			throw new IllegalStateException("The graph does not have a name");
	}

	public QGraph createEmptyGraph() throws CoreException {
		return new QGraph(talker.new_graph());
	}

	public QGraph loadGraph(File location) throws CoreException, IOException {
		QGraph g = new QGraph(talker.load_graph(location));
		updateGraph(g);
		g.setFileName(location.getAbsolutePath());
		return g;
	}

	public void saveGraph(QGraph graph, File location) throws CoreException, IOException {
		assertCoreGraph(graph);
		talker.save_graph(graph.getName(), location);
	}

	public void updateGraph(QGraph graph) throws CoreException {
		String xml = talker.graph_xml(graph.getName());
		try {
			graph.fromXml(xml);
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

	public String hilbertSpaceRepresentation(QGraph graph, RepresentationType format) throws CoreException {
		return talker.hilb(graph.getName(), format.toString().toLowerCase());
	}

	public void renameGraph(QGraph graph, String suggestedNewName) throws CoreException {
		assertCoreGraph(graph);
		graph.setName(talker.rename_graph(graph.getName(), suggestedNewName));
	}

	public void forgetGraph(QGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.kill_graph(graph.getName());
		graph.setName(null);
	}

	public void undo(QGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.undo(graph.getName());
	}

	public void redo(QGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.redo(graph.getName());
	}

	public QVertex addVertex(QGraph graph, QVertex.Type type) throws CoreException {
		assertCoreGraph(graph);
		QVertex v = new QVertex(talker.add_vertex(graph.getName(), type), type);
		graph.addVertex(v);
		graph.fireStateChanged();
		return v;
	}

	public void flipVertices(QGraph graph, Collection<QVertex> vertices) throws CoreException {
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

	public void renameVertex(QGraph graph, QVertex vertex, String suggestedNewName) throws CoreException {
		assertCoreGraph(graph);
		vertex.setName(talker.rename_vertex(graph.getName(), vertex.getName(), suggestedNewName));
	}

	public void setVertexAngle(QGraph graph, QVertex v, String angle) throws CoreException {
		assertCoreGraph(graph);
		talker.set_angle(graph.getName(), v.getName(), angle);
		v.setLabel(angle);
		graph.fireStateChanged();
	}

	public void deleteVertices(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		talker.delete_vertices(graph.getName(), names(vertices));
		for (QVertex v : vertices) {
			graph.removeVertex(v);
		}
		graph.fireStateChanged();
	}

	public QEdge addEdge(QGraph graph, QVertex source, QVertex target) throws CoreException {
		assertCoreGraph(graph);
		String eName = talker.add_edge(graph.getName(), source.getName(), target.getName());
		QEdge e = new QEdge(eName);
		graph.addEdge(e, source, target);
		graph.fireStateChanged();
		return e;
	}

	public void deleteEdges(QGraph graph, Collection<QEdge> edges) throws CoreException {
		assertCoreGraph(graph);
		talker.delete_edges(graph.getName(), names(edges));
		for (QEdge e : edges) {
			graph.removeEdge(e);
		}
		graph.fireStateChanged();
	}

	public BangBox addBangBox(QGraph graph, Collection<QVertex> vertices) throws CoreException {
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

	public void removeVerticesFromBangBoxes(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		talker.unbang_vertices(graph.getName(), names(vertices));
		updateGraph(graph);
	}

	public void dropBangBoxes(QGraph graph, Collection<BangBox> bboxen) throws CoreException {
		assertCoreGraph(graph);
		talker.bbox_drop(graph.getName(), names(bboxen));
		for (BangBox bb : bboxen) {
			graph.removeBangBox(bb);
		}
		graph.fireStateChanged();
	}

	public void killBangBoxes(QGraph graph, Collection<BangBox> bboxen) throws CoreException {
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

	public BangBox mergeBangBoxes(QGraph graph, Collection<BangBox> bboxen) throws CoreException {
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

	public BangBox duplicateBangBox(QGraph graph, BangBox bbox) throws CoreException {
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
			return Util.slurp(file);
		} finally {
			file.delete();
		}
	}

	public Rewrite createRule(String ruleName, QGraph lhs, QGraph rhs) throws CoreException {
		assertCoreGraph(lhs);
		assertCoreGraph(rhs);
		talker.new_rule(ruleName, lhs.getName(), rhs.getName());
		return new Rewrite(ruleName, lhs, rhs);
	}

	public Rewrite openRule(String ruleName) throws CoreException {
		QGraph lhs = new QGraph(talker.open_rule_lhs(ruleName));
		updateGraph(lhs);
		QGraph rhs = new QGraph(talker.open_rule_rhs(ruleName));
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
	public void fastNormalise(QGraph graph) throws CoreException {
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

	public void cutSubgraph(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		String[] vnames = names(vertices);
		talker.copy_subgraph(graph.getName(), "__clip__", vnames);
		talker.delete_vertices(graph.getName(), vnames);
		for (QVertex v : vertices) {
			graph.removeVertex(v);
		}
		graph.fireStateChanged();
	}

	public void copySubgraph(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		String[] vnames = names(vertices);
		talker.copy_subgraph(graph.getName(), "__clip__", vnames);
	}

	public void paste(QGraph target) throws CoreException {
		assertCoreGraph(target);
		talker.insert_graph("__clip__", target.getName());
		updateGraph(target);
	}

	public void attachRewrites(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		talker.attach_rewrites(graph.getName(), names(vertices));
	}

	public void attachOneRewrite(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		talker.attach_one_rewrite(graph.getName(), names(vertices));
	}

	public List<Rewrite> getAttachedRewrites(QGraph graph) throws CoreException {
		try {
			String xml = talker.show_rewrites(graph.getName());
			return Rewrite.parseRewrites(xml);
		} catch (ParseException ex) {
			throw new BadResponseException("Core gave bad rewrite XML");
		}
	}

	public void applyAttachedRewrite(QGraph graph, int i) throws CoreException {
		talker.apply_rewrite(graph.getName(), i);
	}
}
