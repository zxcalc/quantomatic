package quanto.core.data;

import java.awt.geom.Point2D;
import java.util.Map;


public class Vertex extends GraphElement {

	// null == boundary
	protected VertexType vertexType;
	private Point2D pos;
	private Map<String, String> user_data;
	
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
	
	public void setVertexUserData(Map<String, String> map) {
		user_data = map;
	}
	
	public void setVertexUserData(String k, String v) {
		user_data.put(k, v);
	}
	
	public Map<String, String> getVertexUserData() {
		return user_data;
	}
	
	public String getVertexUserData(String k) {
		return user_data.get(k);
	}
	@Override
	public String toString() {
		return getLabel().replace('\\', 'B')+"    ";
	}

	public String getLabel() {
		if (data == null)
			return "";
		else
			return data.getStringValue();
	}

	public boolean isBoundaryVertex() {
		return this.vertexType == null;
	}

}