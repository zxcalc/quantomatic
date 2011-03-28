/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;
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

	private String[] names(Collection<? extends CoreObject> c) {
		String[] ns = new String[c.size()];
		int i = 0;
		for (CoreObject n : c) {
			ns[i] = n.getCoreName();
			++i;
		}
		return ns;
	}

	public CoreTalker getTalker() {
		return talker;
	}

	private void assertCoreGraph(QGraph graph) {
		if (graph.getCoreName() == null)
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
		talker.save_graph(graph.getCoreName(), location);
	}

	public void updateGraph(QGraph graph) throws CoreException {
		String xml = talker.graph_xml(graph.getCoreName());
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
		return talker.hilb(graph.getCoreName(), format.toString().toLowerCase());
	}

	public void renameGraph(QGraph graph, String suggestedNewName) throws CoreException {
		assertCoreGraph(graph);
		graph.updateCoreName(talker.rename_graph(graph.getCoreName(), suggestedNewName));
	}

	public void forgetGraph(QGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.kill_graph(graph.getCoreName());
		graph.updateCoreName(null);
	}

	public void undo(QGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.undo(graph.getCoreName());
	}

	public void redo(QGraph graph) throws CoreException {
		assertCoreGraph(graph);
		talker.redo(graph.getCoreName());
	}

	public QVertex addVertex(QGraph graph, QVertex.Type type) throws CoreException {
		assertCoreGraph(graph);
		QVertex v = new QVertex(talker.add_vertex(graph.getCoreName(), type), type);
		graph.addVertex(v);
		graph.fireStateChanged();
		return v;
	}

	public void flipVertices(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		talker.flip_vertices(graph.getCoreName(), names(vertices));
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
		vertex.updateCoreName(talker.rename_vertex(graph.getCoreName(), vertex.getCoreName(), suggestedNewName));
	}

	public void setVertexAngle(QGraph graph, QVertex v, String angle) throws CoreException {
		assertCoreGraph(graph);
		talker.set_angle(graph.getCoreName(), v.getCoreName(), angle);
		v.setLabel(angle);
		graph.fireStateChanged();
	}

	public void deleteVertices(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		talker.delete_vertices(graph.getCoreName(), names(vertices));
		for (QVertex v : vertices) {
			graph.removeVertex(v);
		}
		graph.fireStateChanged();
	}

	public QEdge addEdge(QGraph graph, QVertex source, QVertex target) throws CoreException {
		assertCoreGraph(graph);
		String eName = talker.add_edge(graph.getCoreName(), source.getCoreName(), target.getCoreName());
		QEdge e = new QEdge(eName);
		graph.addEdge(e, source, target);
		graph.fireStateChanged();
		return e;
	}

	public void deleteEdges(QGraph graph, Collection<QEdge> edges) throws CoreException {
		assertCoreGraph(graph);
		talker.delete_edges(graph.getCoreName(), names(edges));
		for (QEdge e : edges) {
			graph.removeEdge(e);
		}
		graph.fireStateChanged();
	}

	public QBangBox addBangBox(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		QBangBox bb = new QBangBox(talker.add_bang(graph.getCoreName()));
		if (vertices.size() > 0) {
			talker.bang_vertices(graph.getCoreName(), bb.getCoreName(), names(vertices));
		}
		graph.addBangBox(bb, vertices);
		graph.fireStateChanged();
		return bb;
	}

	public void removeVerticesFromBangBoxes(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		talker.unbang_vertices(graph.getCoreName(), names(vertices));
		updateGraph(graph);
	}

	public void dropBangBoxes(QGraph graph, Collection<QBangBox> bboxen) throws CoreException {
		assertCoreGraph(graph);
		talker.bbox_drop(graph.getCoreName(), names(bboxen));
		for (QBangBox bb : bboxen) {
			graph.removeBangBox(bb);
		}
		graph.fireStateChanged();
	}

	public void killBangBoxes(QGraph graph, Collection<QBangBox> bboxen) throws CoreException {
		assertCoreGraph(graph);
		talker.bbox_kill(graph.getCoreName(), names(bboxen));
		// FIXME: this is inefficient for multiple overlapping !-boxes
		for (QBangBox bb : bboxen) {
			for (QVertex v : graph.getBoxedVertices(bb)) {
				graph.removeVertex(v);
			}
			graph.removeBangBox(bb);
		}
		graph.fireStateChanged();
	}

	public QBangBox mergeBangBoxes(QGraph graph, Collection<QBangBox> bboxen) throws CoreException {
		assertCoreGraph(graph);
		QBangBox newbb = new QBangBox(talker.bbox_merge(graph.getCoreName(), names(bboxen)));
		List<QVertex> contents = new LinkedList<QVertex>();
		for (QBangBox bb : bboxen) {
			for (QVertex v : graph.getBoxedVertices(bb)) {
				contents.add(v);
			}
			graph.removeBangBox(bb);
		}
		graph.addBangBox(newbb, contents);
		graph.fireStateChanged();
		return newbb;
	}

	public QBangBox duplicateBangBox(QGraph graph, QBangBox bbox) throws CoreException {
		assertCoreGraph(graph);
		String name = talker.bbox_duplicate(graph.getCoreName(), bbox.getCoreName());
		updateGraph(graph);
		graph.fireStateChanged();
		for (QBangBox bb : graph.getBangBoxes()) {
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
			return Util.slurp(file);
		} finally {
			file.delete();
		}
	}

	public Rewrite createRule(String ruleName, QGraph lhs, QGraph rhs) throws CoreException {
		assertCoreGraph(lhs);
		assertCoreGraph(rhs);
		talker.new_rule(ruleName, lhs.getCoreName(), rhs.getCoreName());
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
		if (rule.getCoreName() == null)
			throw new IllegalArgumentException("Rewrite has no name");
		talker.update_rule(rule.getCoreName(), rule.getLhs().getCoreName(), rule.getRhs().getCoreName());
	}

	/*
	 * Derived methods, note these are in CamelCase to emphasise that they
	 * are not actual core commands.
	 */
	public void fastNormalise(QGraph graph) throws CoreException {
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

	public void cutSubgraph(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		String[] vnames = names(vertices);
		talker.copy_subgraph(graph.getCoreName(), "__clip__", vnames);
		talker.delete_vertices(graph.getCoreName(), vnames);
		for (QVertex v : vertices) {
			graph.removeVertex(v);
		}
		graph.fireStateChanged();
	}

	public void copySubgraph(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		assertCoreGraph(graph);
		String[] vnames = names(vertices);
		talker.copy_subgraph(graph.getCoreName(), "__clip__", vnames);
	}

	public void paste(QGraph target) throws CoreException {
		assertCoreGraph(target);
		talker.insert_graph("__clip__", target.getCoreName());
		updateGraph(target);
	}

	public void attachRewrites(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		talker.attach_rewrites(graph.getCoreName(), names(vertices));
	}

	public void attachOneRewrite(QGraph graph, Collection<QVertex> vertices) throws CoreException {
		talker.attach_one_rewrite(graph.getCoreName(), names(vertices));
	}

	public List<Rewrite> getAttachedRewrites(QGraph graph) throws CoreException {
		try {
			String xml = talker.show_rewrites(graph.getCoreName());
			return Rewrite.parseRewrites(xml);
		} catch (ParseException ex) {
			throw new BadResponseException("Core gave bad rewrite XML");
		}
	}

	public void applyAttachedRewrite(QGraph graph, int i) throws CoreException {
		talker.apply_rewrite(graph.getCoreName(), i);
	}
}
