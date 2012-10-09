package quanto.core.data;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.HashMap;
import quanto.core.ParseException;
import quanto.core.Theory;

/**
 * An edge
 *
 * @author alemer
 */
public class Edge extends GraphElement {
    private boolean directed;
	
	public Edge(String name, boolean directed) {
		super(name);
        this.directed = directed;
	}

    public void setDirected(boolean directed) {
        this.directed = directed;
    }

    public boolean isDirected() {
        return directed;
    }
	
	public static class EdgeData
	{
		public Edge edge;
		public String source;
		public String target;
	}
	
	public EdgeData updateFromJson(Theory theory, JsonNode node) throws ParseException {
		if (!node.isObject())
			throw new ParseException("Expected object");

		JsonNode nameNode = node.get("name");
		if (nameNode != null && nameNode.isTextual())
			updateCoreName(nameNode.asText());

		JsonNode dirNode = node.get("is_directed");
		if (dirNode == null || !dirNode.isBoolean())
			throw new ParseException("Standalone edge had no 'is_directed' property");
		
		return updateFromJson(theory, dirNode.asBoolean(), node);
	}
	
	public static EdgeData fromJson(Theory theory, JsonNode node) throws ParseException {
		if (!node.isObject())
			throw new ParseException("Expected object");

		JsonNode nameNode = node.get("name");
		if (nameNode == null || !nameNode.isTextual())
			throw new ParseException("Standalone edge had no name");

		JsonNode dirNode = node.get("is_directed");
		if (dirNode == null || !dirNode.isBoolean())
			throw new ParseException("Standalone edge had no 'is_directed' property");

		Edge edge = new Edge(nameNode.textValue(), dirNode.asBoolean());
		return edge.updateFromJson(theory, dirNode.asBoolean(), node);
	}
	
	EdgeData updateFromJson(Theory theory, boolean isDirected, JsonNode node) throws ParseException {
		if (!node.isObject())
			throw new ParseException("Expected object");

		directed = isDirected;

		JsonNode dataNode = node.get("data");
		// FIXME: edge data

		JsonNode annotationNode = node.get("annotation");
		if (annotationNode != null && annotationNode.isObject()) {
			ObjectMapper mapper = new ObjectMapper();
			setUserData(mapper.<HashMap<String,String>>convertValue(
					annotationNode,
					mapper.getTypeFactory().constructMapType(
					HashMap.class, String.class, String.class)));
		}
		
		EdgeData ed = new EdgeData();
		ed.edge = this;

		JsonNode srcNode = node.get("src");
		if (srcNode == null || !srcNode.isTextual())
			throw new ParseException("Edge had no 'src' property");
		ed.source = srcNode.asText();

		JsonNode tgtNode = node.get("tgt");
		if (tgtNode == null || !tgtNode.isTextual())
			throw new ParseException("Edge had no 'tgt' property");
		ed.target = tgtNode.asText();
		
		return ed;
	}
	
	static EdgeData fromJson(Theory theory, String name, boolean isDirected, JsonNode desc) throws ParseException {
		Edge edge = new Edge(name, isDirected);
		return edge.updateFromJson(theory, isDirected, desc);
	}
}
