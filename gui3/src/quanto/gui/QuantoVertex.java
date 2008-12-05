package quanto.gui;

import java.awt.Color;

public class QuantoVertex {
	public enum Type { RED, GREEN, BOUNDARY, HADAMARD };
	private Type vertexType;
	private String name;
	public boolean old;

	public QuantoVertex() {
		this.vertexType = null;
		this.old = false;
	}
	
	public QuantoVertex(Type vertexType) {
		super();
		this.vertexType = vertexType;
		this.old = false;
	}

	public Type getVertexType() {
		return vertexType;
	}

	public void setVertexType(Type vertexType) {
		this.vertexType = vertexType;
	}
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	public Color getColor() {
		if (vertexType==Type.RED) return Color.red;
		if (vertexType==Type.GREEN) return Color.green;
		if (vertexType==Type.HADAMARD) return Color.yellow;
		return Color.black;
	}
	
	public void updateTo(QuantoVertex v) {
		old = false;
		name = v.getName();
		vertexType = v.getVertexType();
	}
}