package quanto.core.data;


public class Vertex extends GraphElement {

	// null == boundary
	protected String vertexType;

	public Vertex(String name, String vertexType) {
		super(name);
		if (vertexType == null) {
			throw new IllegalArgumentException("vertexType cannot be null");
		}
		this.vertexType = vertexType;
	}

	protected Vertex(String name) {
		super(name);
	}

	/**
	 * The vertex type name.
	 *
	 * @return the vertex type, as specified by the core,
	 *         or null if it is a boundary vertex
	 */
	public String getVertexType() {
		return vertexType;
	}

	public void setVertexType(String vertexType) {
		if (vertexType == null)
			throw new IllegalArgumentException("vertexType cannot be null");
		this.vertexType = vertexType;
	}
	
	@Override
	public String toString() {
		return getLabel().replace('\\', 'B')+"    ";
	}

	public void updateTo(Vertex v) {
		if (v.isBoundaryVertex()) {
			throw new IllegalArgumentException("Cannot update a non-boundary vertex to a boundary vertex");
		}
		setVertexType(v.getVertexType());
		setData(v.getData());
	}

	public String getLabel() {
		if (data == null)
			return "";
		else
			return data.getStringValue();
	}

	public boolean isBoundaryVertex() {
		return false;
	}
}
