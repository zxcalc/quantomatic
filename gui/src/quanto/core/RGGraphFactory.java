/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import net.n3.nanoxml.IXMLElement;

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
		return new RGVertex(name, RGVertex.Type.valueOf(vertexType.toUpperCase()));
	}

	public BasicEdge createEdge(String name) {
		return new BasicEdge(name);
	}

	public BasicBangBox createBangBox(String name) {
		return new BasicBangBox(name);
	}

	public RGGraph createGraphFromXml(String name, String xml) throws ParseException {
		RGGraph g = new RGGraph(name);
		updateGraphFromXml(g, xml);
		return g;
	}

	public RGGraph createGraphFromXml(String name, IXMLElement xml) throws ParseException {
		RGGraph g = new RGGraph(name);
		updateGraphFromXml(g, xml);
		return g;
	}

	public void updateGraphFromXml(RGGraph graph, String xml) throws ParseException {
		graph.fromXml(xml);
	}

	public void updateGraphFromXml(RGGraph graph, IXMLElement xml) throws ParseException {
		graph.fromXml(xml);
	}
}
