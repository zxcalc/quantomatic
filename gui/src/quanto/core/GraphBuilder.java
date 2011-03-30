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
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
import quanto.core.data.CoreGraph;
import quanto.core.data.CoreObject;
import quanto.core.data.CoreVertex;

/**
 *
 * @author alemer
 */
public class GraphBuilder<G extends CoreGraph<V,E,B>,
	                  V extends CoreVertex,
			  E extends CoreObject,
			  B extends CoreObject> {

	private final static Logger logger =
		LoggerFactory.getLogger(GraphBuilder.class);

	private GraphFactory<G, V, E, B> factory;

	public GraphBuilder(GraphFactory<G, V, E, B> factory) {
		this.factory = factory;
	}

	public GraphFactory<G, V, E, B> getFactory() {
		return factory;
	}

	public void setFactory(GraphFactory<G, V, E, B> factory) {
		this.factory = factory;
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

	public Map<String, V> getVertexMap(G graph) {
		Map<String, V> verts =
			new HashMap<String, V>();
		for (V v : graph.getVertices()) {
			verts.put(v.getCoreName(), v);
		}
		return verts;
	}

	private void updateGraphFromXmlReader(G graph, IXMLReader reader) throws ParseException {
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

	public G createGraphFromXmlFile(String name, File file) throws IOException, ParseException {
		G graph = factory.createGraph(name);
		updateGraphFromXmlFile(graph, file);
		return graph;
	}

	public G createGraphFromXml(String name, String xml) throws ParseException {
		G graph = factory.createGraph(name);
		updateGraphFromXml(graph, xml);
		return graph;
	}

	public G createGraphFromXml(String name, IXMLElement graphNode) throws ParseException {
		G graph = factory.createGraph(name);
		updateGraphFromXml(graph, graphNode);
		return graph;
	}

	public void updateGraphFromXmlFile(G graph, File file) throws IOException, ParseException {
		updateGraphFromXmlReader(graph, StdXMLReader.fileReader(file.getAbsolutePath()));
	}

	public void updateGraphFromXml(G graph, String xml) throws ParseException {
		updateGraphFromXmlReader(graph, StdXMLReader.stringReader(xml));
	}

	@SuppressWarnings("unchecked")
	public void updateGraphFromXml(G graph, IXMLElement graphNode) throws ParseException {
		if (graphNode == null)
			throw new ParseException("Graph is null");

		synchronized (graph) {
			List<V> boundaryVertices = new ArrayList<V>();
			for (E e : new ArrayList<E>(graph.getEdges()))
				graph.removeEdge(e);
			for (B b : new ArrayList<B>(graph.getBangBoxes()))
				graph.removeBangBox(b);

			Map<String, V> verts = getVertexMap(graph);
			Set<V> stale = new HashSet<V>(graph.getVertices());

			for (Object obj : graphNode.getChildrenNamed("vertex")) {
				IXMLElement vertexNode = (IXMLElement)obj;
				V v = null;

				try {
					String vname = vertexNode.getFirstChildNamed("name").getContent();
					if (vname == null || vname.length() == 0)
						throwParseException(vertexNode, "no name given");

					if (vertexNode.getFirstChildNamed("boundary")
							.getContent().equals("true"))
					{
						v = factory.createBoundaryVertex(vname);
					} else if (vertexNode.getFirstChildNamed("boundary")
							.getContent().equals("false")) {
						v = factory.createVertex(vname, vertexNode.getFirstChildNamed("colour").getContent());
					} else {
						throwParseException(vertexNode, ": invalid value for \"boundary\"");
					}

					IXMLElement expr = vertexNode
						.getFirstChildNamed("angleexpr");
					if (expr != null) {
						v.setData(expr.getFirstChildNamed("as_string").getContent());
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

				V old_v = verts.get(v.getCoreName());
				if (old_v == null) {
					verts.put(v.getCoreName(), v);
					graph.addVertex(v);
				} else {
					stale.remove(old_v);
					old_v.updateTo(v);
					v = old_v;
				}

				if (v.isBoundaryVertex()) {
					boundaryVertices.add(v);
				}
			} // foreach vertex

			Collections.sort(boundaryVertices, new CoreObject.NameComparator());

                        for (int i = 0; i < boundaryVertices.size(); ++i) {
                                boundaryVertices.get(i).setData(String.valueOf(i));
                        }

			// Prune removed vertices
			for (V v : stale) {
				verts.remove(v.getCoreName());
				graph.removeVertex(v);
			}


			for (Object obj : graphNode.getChildrenNamed("edge")) {
				IXMLElement edgeNode = (IXMLElement)obj;

				V source = null, target = null;
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

				graph.addEdge(factory.createEdge(ename),
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

				B bbox = factory.createBangBox(bbname);
				List contents = new LinkedList();

				for (IXMLElement boxedVert :
					(Vector<IXMLElement>)bangBox.getChildrenNamed("boxedvertex"))
				{
					V v = verts.get(boxedVert.getContent());
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
