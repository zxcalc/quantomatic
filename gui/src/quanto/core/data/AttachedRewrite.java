/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core.data;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import quanto.core.ParseException;

/**
 *
 * @author alemer
 */
public class AttachedRewrite {
	private CoreGraph graph;
	private int index;
    private Rule rule;
	private CoreGraph newGraph;

	public AttachedRewrite(CoreGraph graph, int index, Rule rule, CoreGraph newGraph) {
		this.graph = graph;
		this.index = index;
		this.rule = rule;
        this.newGraph = newGraph;
	}

	public CoreGraph getGraph() {
		return graph;
	}

	public int getIndex() {
		return index;
	}
	
	public String getRuleName() {
		return rule.getCoreName();
	}

	public CoreGraph getLhs() {
		return rule.getLhs();
	}

	public CoreGraph getRhs() {
		return rule.getRhs();
	}

	public CoreGraph getNewGraph() {
		return newGraph;
	}
	
	public Collection<Vertex> getRemovedVertices() {
		Set<String> newVNames = getNewGraph().getVertexMap().keySet();
		Map<String,Vertex> vmap = getGraph().getVertexMap();
		Set<String> hlVNames = new HashSet<String>(vmap.keySet());
		hlVNames.removeAll(newVNames);
		List<Vertex> newVerts = new ArrayList<Vertex>(hlVNames.size());
		for (String vname : hlVNames) {
			Vertex v = vmap.get(vname);
			if (v != null)
				newVerts.add(v);
		}
		return newVerts;
	}
	
	public static AttachedRewrite fromJson(CoreGraph graph, int index, JsonNode node) throws ParseException {
		if (!node.isObject())
			throw new ParseException("Expected object");
		
		JsonNode ruleNode = node.get("rule");
		if (ruleNode == null || ruleNode.isNull())
			throw new ParseException("No rhs given for rule");
		Rule rule = Rule.fromJson(graph.getTheory(), ruleNode);
		
		JsonNode newGraphNode = node.get("rewritten_graph");
		if (newGraphNode == null || newGraphNode.isNull())
			throw new ParseException("No rhs given for rule");
		CoreGraph newGraph = CoreGraph.fromJson(graph.getTheory(), null, newGraphNode);
		
		return new AttachedRewrite(graph, index, rule, newGraph);
	}
}
