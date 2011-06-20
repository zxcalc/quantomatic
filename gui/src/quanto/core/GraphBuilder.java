/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core;

import edu.uci.ics.jung.graph.util.EdgeType;
import edu.uci.ics.jung.visualization.util.ChangeEventSupport;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import net.n3.nanoxml.IXMLElement;
import net.n3.nanoxml.IXMLParser;
import net.n3.nanoxml.IXMLReader;
import net.n3.nanoxml.StdXMLBuilder;
import net.n3.nanoxml.StdXMLReader;
import net.n3.nanoxml.XMLException;
import net.n3.nanoxml.XMLParserFactory;

import quanto.core.data.BangBox;
import quanto.core.data.CoreGraph;
import quanto.core.data.Edge;
import quanto.core.data.GraphElementData;
import quanto.core.data.Vertex;
import quanto.core.data.VertexType;

/**
 *
 * @author alemer
 */
public class GraphBuilder {

	private final static Logger logger =
			Logger.getLogger("quanto.core.xml");
	private Theory theory;

	public GraphBuilder(Theory theory) {
		this.theory = theory;
	}

	private void throwParseException(IXMLElement element, String message) throws ParseException {
		String finalmsg = "Bad " + element.getName() + " definition";
		if (element.getLineNr() != IXMLElement.NO_LINE) {
			finalmsg += " at line " + element.getLineNr();
		}
		if (message != null) {
			finalmsg += ": " + message;
		}

		throw new ParseException(finalmsg);
	}

	public Map<String, Vertex> getVertexMap(CoreGraph graph) {
		Map<String, Vertex> verts =
				new HashMap<String, Vertex>();
		for (Vertex v : graph.getVertices()) {
			verts.put(v.getCoreName(), v);
		}
		return verts;
	}

	private IXMLElement rootElementFromXmlString(String xml) throws ParseException {
		return rootElementFromReader(StdXMLReader.stringReader(xml));
	}

	private IXMLElement rootElementFromXmlFile(String filename) throws IOException, ParseException {
		return rootElementFromReader(StdXMLReader.fileReader(filename));
	}

	private IXMLElement rootElementFromReader(IXMLReader reader) throws ParseException {
		IXMLElement root = null;
		try {
			long millis = System.currentTimeMillis();
			IXMLParser parser = XMLParserFactory.createDefaultXMLParser(new StdXMLBuilder());
			parser.setReader(reader);
			root = (IXMLElement) parser.parse();
			logger.log(Level.FINEST, "XML parse took {} milliseconds", System.currentTimeMillis() - millis);
			return root;
		}
		catch (XMLException e) {
			throw new ParseException("The file contains badly-formed XML: " + e.getMessage(), e);
		}
		catch (ClassNotFoundException e) {
			throw new Error(e);
		}
		catch (InstantiationException e) {
			throw new Error(e);
		}
		catch (IllegalAccessException e) {
			throw new Error(e);
		}
	}

	public CoreGraph createGraphFromXmlFile(String name, File file) throws IOException, ParseException {
		CoreGraph graph = new CoreGraph(name);
		updateGraphFromXmlFile(graph, file);
		return graph;
	}

	public CoreGraph createGraphFromXml(String name, String xml) throws ParseException {
		CoreGraph graph = new CoreGraph(name);
		updateGraphFromXml(graph, xml);
		return graph;
	}

	public CoreGraph createGraphFromXml(String name, IXMLElement graphNode) throws ParseException {
		CoreGraph graph = new CoreGraph(name);
		updateGraphFromXml(graph, graphNode);
		return graph;
	}

	public void updateGraphFromXmlFile(CoreGraph graph, File file) throws IOException, ParseException {
		updateGraphFromXml(graph, rootElementFromXmlFile(file.getAbsolutePath()));
	}

	public void updateGraphFromXml(CoreGraph graph, String xml) throws ParseException {
		updateGraphFromXml(graph, rootElementFromXmlString(xml));
	}

	public String vertexDataToString(VertexType.DataType type, IXMLElement dataNode) throws ParseException {
		if (type == VertexType.DataType.MathExpression) {
			IXMLElement expr = dataNode.getFirstChildNamed("angleexpr");
			if (expr == null) {
				throwParseException(dataNode, "No angle data given");
			}
			return expr.getFirstChildNamed("string_of").getContent();
		}
		else if (type == VertexType.DataType.String) {
			// FIXME: how is this stored?
		}
		return null;
	}

	public Vertex createVertexFromXml(CoreGraph graph, String vertexXml) throws ParseException {
		return createVertexFromXml(graph, rootElementFromXmlString(vertexXml));
	}

	public Vertex createVertexFromXml(CoreGraph graph, IXMLElement vertexNode) throws ParseException {
		Vertex v = null;

		String vname = vertexNode.getAttribute("name", "");
		if (vname.length() == 0) {
			throwParseException(vertexNode, "no name given");
		}

		IXMLElement typeNode = vertexNode.getFirstChildNamed("type");
		if (typeNode == null) {
			throwParseException(vertexNode, "No vertex type given");
		}
		String vTypeName = typeNode.getContent();
		if (vTypeName == null || vTypeName.length() == 0) {
			throwParseException(typeNode, "Invalid type description");
		}
		if (vTypeName.equals("edge-point")) {
			v = Vertex.createBoundaryVertex(vname);
		}
		else {
			VertexType vType = theory.getVertexType(vTypeName);
			if (vType == null) {
				throwParseException(typeNode, "Unknown vertex type \"" + vTypeName + "\"");
			}
			v = Vertex.createVertex(vname, vType);

			if (vType.getDataType() != VertexType.DataType.None) {
				IXMLElement dataNode = vertexNode.getFirstChildNamed("data");
				if (dataNode == null) {
					throwParseException(vertexNode, "No vertex data given");
				}
				v.setData(new GraphElementData(vertexDataToString(vType.getDataType(), dataNode)));
			}
		}

		return v;
	}

	public Vertex addVertexFromXml(CoreGraph graph, String vertexXml) throws ParseException {
		Vertex v = addVertexFromXmlQuiet(graph, vertexXml);
		graph.fireStateChanged();
		return v;
	}

	public Vertex addVertexFromXmlQuiet(CoreGraph graph, String vertexXml) throws ParseException {
		return addVertexFromXmlQuiet(graph, rootElementFromXmlString(vertexXml));
	}

	public Vertex addVertexFromXml(CoreGraph graph, IXMLElement vertexNode) throws ParseException {
		Vertex v = addVertexFromXmlQuiet(graph, vertexNode);
		graph.fireStateChanged();
		return v;
	}

	public Vertex addVertexFromXmlQuiet(CoreGraph graph, IXMLElement vertexNode) throws ParseException {
		Vertex v = createVertexFromXml(graph, vertexNode);
		graph.addVertex(v);
		return v;
	}

	public Edge addEdgeFromXml(CoreGraph graph, String edgeXml) throws ParseException {
		Edge e = addEdgeFromXmlQuiet(graph, edgeXml);
		graph.fireStateChanged();
		return e;
	}

	public Edge addEdgeFromXml(CoreGraph graph, IXMLElement edgeNode) throws ParseException {
		Edge e = addEdgeFromXmlQuiet(graph, edgeNode);
		graph.fireStateChanged();
		return e;
	}

	public Edge addEdgeFromXmlQuiet(CoreGraph graph, String edgeXml) throws ParseException {
		return addEdgeFromXmlQuiet(graph, rootElementFromXmlString(edgeXml));
	}

	public Edge addEdgeFromXmlQuiet(CoreGraph graph, IXMLElement edgeNode) throws ParseException {
		return addEdgeFromXmlQuiet(graph, graph.getVertexMap(), edgeNode);
	}

	private Edge addEdgeFromXmlQuiet(CoreGraph graph, Map<String, Vertex> vertices, IXMLElement edgeNode) throws ParseException {
		Vertex source = null, target = null;
		String ename = null;
		EdgeType etype = EdgeType.DIRECTED;
		String attrData = null;

		ename = edgeNode.getAttribute("name", "");
		if (ename.length() == 0) {
			throwParseException(edgeNode, "no name given");
		}

		attrData = edgeNode.getAttribute("source", "");
		if (attrData.length() == 0) {
			throwParseException(edgeNode, "no source given");
		}
		else {
			source = vertices.get(attrData);
		}
		if (source == null) {
			throwParseException(edgeNode, "unknown source");
		}

		attrData = edgeNode.getAttribute("target", "");
		if (attrData.length() == 0) {
			throwParseException(edgeNode, "no target given");
		}
		else {
			target = vertices.get(attrData);
		}
		if (target == null) {
			throwParseException(edgeNode, "unknown target");
		}

//		attrData = edgeNode.getAttribute("dir", null);
//		if (attrData == null)
//			throwParseException(edgeNode, "no directedness given");
//		if (attrData.equalsIgnoreCase("true"))
//			etype = EdgeType.DIRECTED;
//		else if (attrData.equalsIgnoreCase("false"))
//			etype = EdgeType.UNDIRECTED;
//		else
//			throwParseException(edgeNode, "invalid value for edge directedness (dir)");

		Edge e = new Edge(ename);
		graph.addEdge(e, source, target, etype);
		return e;
	}

	public BangBox addBangBoxFromXml(CoreGraph graph, String bangBoxXml) throws ParseException {
		BangBox bb = addBangBoxFromXmlQuiet(graph, bangBoxXml);
		graph.fireStateChanged();
		return bb;
	}

	public BangBox addBangBoxFromXml(CoreGraph graph, IXMLElement bangBoxNode) throws ParseException {
		BangBox bb = addBangBoxFromXmlQuiet(graph, bangBoxNode);
		graph.fireStateChanged();
		return bb;
	}

	public BangBox addBangBoxFromXmlQuiet(CoreGraph graph, String bangBoxXml) throws ParseException {
		return addBangBoxFromXmlQuiet(graph, rootElementFromXmlString(bangBoxXml));
	}

	public BangBox addBangBoxFromXmlQuiet(CoreGraph graph, IXMLElement bangBoxNode) throws ParseException {
		return addBangBoxFromXmlQuiet(graph, graph.getVertexMap(), bangBoxNode);
	}

	private BangBox addBangBoxFromXmlQuiet(CoreGraph graph, Map<String, Vertex> vertices, IXMLElement bangBoxNode) throws ParseException {
		String bbname = bangBoxNode.getAttribute("name", "");
		if (bbname.length() == 0) {
			throwParseException(bangBoxNode, "no name given");
		}

		BangBox bbox = new BangBox(bbname);
		List<Vertex> contents = new LinkedList<Vertex>();

		for (Object obj : bangBoxNode.getChildrenNamed("vertex")) {
			IXMLElement boxedVert = (IXMLElement) obj;
			Vertex v = vertices.get(boxedVert.getContent());
			if (v == null) {
				throwParseException(boxedVert, "unknown vertex");
			}
			contents.add(v);
		}
		graph.addBangBox(bbox, contents);
		return bbox;
	}

	@SuppressWarnings("unchecked")
	public void updateGraphFromXml(CoreGraph graph, IXMLElement graphNode) throws ParseException {
		if (graphNode == null) {
			throw new ParseException("Graph is null");
		}

		synchronized (graph) {
			for (Edge e : new ArrayList<Edge>(graph.getEdges())) {
				graph.removeEdge(e);
			}
			for (BangBox b : new ArrayList<BangBox>(graph.getBangBoxes())) {
				graph.removeBangBox(b);
			}
			for (Vertex v : new ArrayList<Vertex>(graph.getVertices())) {
				graph.removeVertex(v);
			}

			Map<String, Vertex> verts = getVertexMap(graph);

			for (Object obj : graphNode.getChildrenNamed("vertex")) {
				Vertex v = addVertexFromXmlQuiet(graph, (IXMLElement) obj);
				verts.put(v.getCoreName(), v);
			}

			for (Object obj : graphNode.getChildrenNamed("edge")) {
				addEdgeFromXmlQuiet(graph, (IXMLElement) obj);
			}

			for (Object obj : graphNode.getChildrenNamed("bangbox")) {
				addBangBoxFromXmlQuiet(graph, verts, (IXMLElement) obj);
			}
		} // synchronized(this)
		if (graph instanceof ChangeEventSupport) {
			((ChangeEventSupport) graph).fireStateChanged();
		}
	}
}
