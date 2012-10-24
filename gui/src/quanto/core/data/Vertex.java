package quanto.core.data;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.awt.geom.Point2D;
import java.util.HashMap;
import quanto.core.ParseException;
import quanto.core.Theory;


public class Vertex extends GraphElement {

	// null == boundary
	protected VertexType vertexType;
	private Point2D pos;
	
	public static Vertex createVertex(String name, VertexType vertexType) {
		if (vertexType == null) {
			throw new IllegalArgumentException("vertexType cannot be null");
		}
		return new Vertex(name, vertexType);
	}

	public static Vertex createBoundaryVertex(String name) {
		return new Vertex(name);
	}

	protected Vertex(String name, VertexType vertexType) {
		super(name);
		this.vertexType = vertexType;
		pos=null;
	}

	protected Vertex(String name) {
		super(name);
		pos=null;
	}

	/**
	 * The vertex type name.
	 *
	 * @return the vertex type, as specified by the core,
	 *         or null if it is a boundary vertex
	 */
	public VertexType getVertexType() {
		return vertexType;
	}

	
	public void setPosition(Point2D pos) {
		this.pos=pos;
	}
	
	public Point2D getPosition(){
		return pos;
	}
	
	@Override
	public String toString() {
		return getLabel().replace('\\', 'B')+"    ";
	}

	public String getLabel() {
		if (data == null)
			return "";
		else
			return data.getDisplayString();
	}

	public boolean isBoundaryVertex() {
		return this.vertexType == null;
	}
	
	public void updateFromJson(Theory theory, JsonNode node) throws ParseException {
		if (!node.isObject())
			throw new ParseException("Expected object");

		JsonNode nameNode = node.get("name");
		if (nameNode != null && nameNode.isTextual())
			updateCoreName(nameNode.asText());

		JsonNode isWvNode = node.get("is_wire_vertex");
		if (isWvNode == null || !isWvNode.isBoolean())
			throw new ParseException("Standalone vertex did not have is_wire_vertex");
		
		updateFromJson(theory, isWvNode.asBoolean(), node);
	}
	
	public static Vertex fromJson(Theory theory, JsonNode node) throws ParseException {
		if (!node.isObject())
			throw new ParseException("Expected object");

		JsonNode nameNode = node.get("name");
		if (nameNode == null || !nameNode.isTextual())
			throw new ParseException("Standalone vertex had no name");

		Vertex vertex = new Vertex(nameNode.textValue());
		vertex.updateFromJson(theory, node);
		
		return vertex;
	}
	
	void updateFromJson(Theory theory, boolean isWireVertex, JsonNode node) throws ParseException {
		if (isWireVertex) {
			vertexType = null;
			data = null;

			if (!node.isObject())
				return;
		} else {
			if (!node.isObject())
				throw new ParseException("Expected object");

			JsonNode dataNode = node.get("data");
			vertexType = theory.getVertexType(dataNode);
			GraphElementDataType dataType = vertexType.getDataType();
			if (dataType == null)
				data = null;
			else
				data = dataType.parseData(dataNode);
		}

		JsonNode annotationNode = node.get("annotation");
		if (annotationNode != null && annotationNode.isObject()) {
			ObjectMapper mapper = new ObjectMapper();
			setUserData(mapper.<HashMap<String,String>>convertValue(
					annotationNode,
					mapper.getTypeFactory().constructMapType(
					HashMap.class, String.class, String.class)));
		}
	}

	static Vertex fromJson(Theory theory, String name, boolean isWireVertex, JsonNode desc) throws ParseException {
		Vertex vertex = new Vertex(name);
		vertex.updateFromJson(theory, isWireVertex, desc);
		return vertex;
	}
}