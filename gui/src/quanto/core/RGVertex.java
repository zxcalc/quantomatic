package quanto.core;


public class RGVertex implements CoreVertex, Comparable<RGVertex> {

	public enum Type { RED, GREEN, BOUNDARY, HADAMARD };
	private Type vertexType;
	private String name;
	private String label = "0";
	public boolean old;

	public RGVertex() {
		this(null);
	}
	
	public RGVertex(Type vertexType) {
		this.vertexType = vertexType;
		this.old = false;
	}

	public RGVertex(String name, Type vertexType) {
		this.vertexType = vertexType;
		this.old = false;
		updateCoreName(name);
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
	
	public void updateTo(RGVertex v) {
		old = false;
		name = v.getCoreName();
		vertexType = v.getVertexType();
		label = v.getLabel();
	}

	public String getLabel() {
		return label;
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
