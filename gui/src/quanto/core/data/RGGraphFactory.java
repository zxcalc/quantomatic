/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core.data;

import quanto.core.GraphFactory;

/**
 * @author alemer
 */
public class RGGraphFactory
	implements GraphFactory<RGGraph, RGVertex, BasicEdge, BasicBangBox>
{
	public RGGraph createGraph(String name) {
		return new RGGraph(name);
	}

	public RGVertex createVertex(String name, String vertexType) {
		RGVertex.Type type;
		vertexType = vertexType.toLowerCase();
		if (vertexType.equals("red"))
			type = RGVertex.Type.RED;
		else if (vertexType.equals("green"))
			type = RGVertex.Type.GREEN;
		else if (vertexType.equals("h") || vertexType.equals("hadamard"))
			type = RGVertex.Type.HADAMARD;
		else
			throw new IllegalArgumentException(vertexType + " is not a valid RG vertex type");
		RGVertex v = new RGVertex(name, type);
		v.setLabel("0");
		return v;
	}

	public RGVertex createBoundaryVertex(String name) {
		return new RGVertex(name, RGVertex.Type.BOUNDARY);
	}

	public BasicEdge createEdge(String name) {
		return new BasicEdge(name);
	}

	public BasicBangBox createBangBox(String name) {
		return new BasicBangBox(name);
	}
}
