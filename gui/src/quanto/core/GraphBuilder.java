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
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import net.n3.nanoxml.IXMLElement;
import net.n3.nanoxml.IXMLParser;
import net.n3.nanoxml.IXMLReader;
import net.n3.nanoxml.StdXMLBuilder;
import net.n3.nanoxml.StdXMLReader;
import net.n3.nanoxml.XMLException;
import net.n3.nanoxml.XMLParserFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import quanto.core.data.BangBox;
import quanto.core.data.CoreGraph;
import quanto.core.data.CoreObject;
import quanto.core.data.Edge;
import quanto.core.data.Vertex;
import quanto.core.data.VertexType;

/**
 *
 * @author alemer
 */
public class GraphBuilder {

	private final static Logger logger =
		LoggerFactory.getLogger(GraphBuilder.class);
	
	private Theory theory;

	public GraphBuilder(Theory theory) {
		this.theory = theory;
	}

	private void throwParseException(IXMLElement element, String message) throws ParseException
	{
		String finalmsg = "Bad " + element.getName() + " definition";
		if (element.getLineNr() != IXMLElement.NO_LINE)
			finalmsg += " at line " + element.getLineNr();
		if (message != null)
			finalmsg += ": " + message;

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

	private void updateGraphFromXmlReader(CoreGraph graph, IXMLReader reader) throws ParseException {
		IXMLElement root = null;
		try {
			long millis = System.currentTimeMillis();
			IXMLParser parser = XMLParserFactory.createDefaultXMLParser(new StdXMLBuilder());
			parser.setReader(reader);
			root = (IXMLElement)parser.parse();
			updateGraphFromXml(graph, root);
			logger.debug("XML parse took {} milliseconds", System.currentTimeMillis()-millis);
		} catch (XMLException e) {
			throw new ParseException("The file contains badly-formed XML: " + e.getMessage(), e);
		} catch (ClassNotFoundException e) {
			throw new Error(e);
		} catch (InstantiationException e) {
			throw new Error(e);
		} catch (IllegalAccessException e) {
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
		updateGraphFromXmlReader(graph, StdXMLReader.fileReader(file.getAbsolutePath()));
	}

	public void updateGraphFromXml(CoreGraph graph, String xml) throws ParseException {
		updateGraphFromXmlReader(graph, StdXMLReader.stringReader(xml));
	}

	@SuppressWarnings("unchecked")
	public void updateGraphFromXml(CoreGraph graph, IXMLElement graphNode) throws ParseException {
		if (graphNode == null)
			throw new ParseException("Graph is null");

		synchronized (graph) {
			List<Vertex> boundaryVertices = new ArrayList<Vertex>();
			for (Edge e : new ArrayList<Edge>(graph.getEdges()))
				graph.removeEdge(e);
			for (BangBox b : new ArrayList<BangBox>(graph.getBangBoxes()))
				graph.removeBangBox(b);
			for (Vertex v : new ArrayList<Vertex>(graph.getVertices()))
				graph.removeVertex(v);

			Map<String, Vertex> verts = getVertexMap(graph);

			for (Object obj : graphNode.getChildrenNamed("vertex")) {
				IXMLElement vertexNode = (IXMLElement)obj;
				Vertex v = null;

				try {
					String vname = vertexNode.getFirstChildNamed("name").getContent();
					if (vname == null || vname.length() == 0)
						throwParseException(vertexNode, "no name given");

					if (vertexNode.getFirstChildNamed("boundary")
							.getContent().equals("true"))
					{
						v = Vertex.createBoundaryVertex(vname);
					} else if (vertexNode.getFirstChildNamed("boundary")
							.getContent().equals("false")) {
						String vTypeName = vertexNode.getFirstChildNamed("colour").getContent().toLowerCase();
						if (vTypeName.equals("h"))
							vTypeName = "hadamard";
						VertexType vType = theory.getVertexType(vTypeName);
						v = Vertex.createVertex(vname, vType);

						if (vType.getDataType() == VertexType.DataType.MathExpression) {
							IXMLElement expr = vertexNode.getFirstChildNamed("angleexpr");
							if (expr != null) {
								v.getData().setValue(expr.getFirstChildNamed("as_string").getContent());
							}
						}
					} else {
						throwParseException(vertexNode, ": invalid value for \"boundary\"");
					}
				} catch (IllegalArgumentException e) {
					logger.info("Got illegal arg ex", e);
					throwParseException(vertexNode, null);
				} catch (NullPointerException e) {
					logger.info("Got nullptr ex", e);
					/* if NullPointerException is thrown, the
					 * core has most likely neglected to include
					 * a required field.
					 */
					throwParseException(vertexNode, null);
				}

				verts.put(v.getCoreName(), v);
				graph.addVertex(v);

				if (v.isBoundaryVertex()) {
					boundaryVertices.add(v);
				}
			} // foreach vertex

			Collections.sort(boundaryVertices, new CoreObject.NameComparator());

			for (Object obj : graphNode.getChildrenNamed("edge")) {
				IXMLElement edgeNode = (IXMLElement)obj;

				Vertex source = null, target = null;
				String ename = null;
				IXMLElement ch = null;

				ch = edgeNode.getFirstChildNamed("name");
				if (ch!=null)
					ename = ch.getContent();
				if (ename == null || ename.length() == 0)
					throwParseException(edgeNode, "no name given");

				ch = edgeNode.getFirstChildNamed("source");
				if (ch!=null)
					source = verts.get(ch.getContent());
				else
					throwParseException(edgeNode, "no source given");
				if (source == null)
					throwParseException(edgeNode, "unknown source");


				ch = edgeNode.getFirstChildNamed("target");
				if (ch!=null)
					target = verts.get(ch.getContent());
				else
					throwParseException(edgeNode, "no target given");
				if (target == null)
					throwParseException(edgeNode, "unknown target");

				graph.addEdge(new Edge(ename),
					source, target, EdgeType.DIRECTED);

			} // foreach edge

			for (IXMLElement bangBox :
				(Vector<IXMLElement>)graphNode.getChildrenNamed("bangbox"))
			{
				IXMLElement nm = bangBox.getFirstChildNamed("name");
				if (nm == null)
					throwParseException(bangBox, "no name given");

				String bbname = nm.getContent();
				if (bbname == null || bbname.length() == 0)
					throwParseException(bangBox, "no name given");

				BangBox bbox = new BangBox(bbname);
				List<Vertex> contents = new LinkedList<Vertex>();

				for (IXMLElement boxedVert :
					(Vector<IXMLElement>)bangBox.getChildrenNamed("boxedvertex"))
				{
					Vertex v = verts.get(boxedVert.getContent());
					if (v == null)
						throwParseException(boxedVert, "unknown vertex");
					contents.add(v);
				}
				graph.addBangBox(bbox, contents);
			}
		} // synchronized(this)
		if (graph instanceof ChangeEventSupport)
			((ChangeEventSupport)graph).fireStateChanged();
	}
}
