package quanto.core.data;


public class RGVertex implements CoreVertex, Comparable<RGVertex> {

	public enum Type { RED, GREEN, BOUNDARY, HADAMARD };
	private Type vertexType;
	private String name;
	private String label = "0";

	public RGVertex() {
		this(null);
	}
	
	public RGVertex(Type vertexType) {
		this.vertexType = vertexType;
	}

	public RGVertex(String name, Type vertexType) {
		this.vertexType = vertexType;
		this.name = name;
	}

	public Type getVertexType() {
		return vertexType;
	}

	public void setVertexType(Type vertexType) {
		this.vertexType = vertexType;
	}
	
	public void setVertexType(String vertexType) {
		vertexType = vertexType.toLowerCase();
		if (vertexType.equals("red"))
			setVertexType(RGVertex.Type.RED);
		else if (vertexType.equals("green"))
			setVertexType(RGVertex.Type.GREEN);
		else if (vertexType.equals("h"))
			setVertexType(RGVertex.Type.HADAMARD);
		else throw new IllegalArgumentException("vertexType");
	}
	
	public String getCoreName() {
		return name;
	}

	public void updateCoreName(String name) {
		this.name = name;
	}

	@Override
	public String toString() {
		return getLabel().replace('\\', 'B')+"    ";
	}

	public void updateTo(CoreVertex v) {
		if (!(v instanceof RGVertex))
			throw new IllegalArgumentException("v must be an RGVertex");
		name = v.getCoreName();
		vertexType = ((RGVertex)v).getVertexType();
		label = v.getDataAsString();
	}

	public String getLabel() {
		return label;
	}

	public String getDataAsString() {
		return label;
	}

	public void setData(String data) {
		this.label = data;
	}

	public void setLabel(String label) {
		this.label = label;
	}
	
	public boolean isAngleVertex() {
		return (vertexType == Type.RED || vertexType == Type.GREEN);
	}

	public int compareTo(RGVertex o) {
		return getCoreName().compareTo(o.getCoreName());
	}

	public boolean isBoundaryVertex() {
		return this.vertexType == Type.BOUNDARY;
	}

	public String getCoreVertexType() {
		return this.vertexType.name().toLowerCase();
	}
}
