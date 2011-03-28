package quanto.core;


public class QVertex implements CoreObject, Comparable<QVertex> {
	public enum Type { RED, GREEN, BOUNDARY, HADAMARD };
	private Type vertexType;
	private String name;
	private String label = "0";
	public boolean old;

	public QVertex() {
		this(null);
	}
	
	public QVertex(Type vertexType) {
		this.vertexType = vertexType;
		this.old = false;
	}

	public QVertex(String name, Type vertexType) {
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
			setVertexType(QVertex.Type.RED);
		else if (vertexType.equals("green"))
			setVertexType(QVertex.Type.GREEN);
		else if (vertexType.equals("h"))
			setVertexType(QVertex.Type.HADAMARD);
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
	
	public void updateTo(QVertex v) {
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

	public int compareTo(QVertex o) {
		return getCoreName().compareTo(o.getCoreName());
	}
	
}
