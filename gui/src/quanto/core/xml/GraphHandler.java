/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.xml;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;
import quanto.core.Theory;
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
public class GraphHandler extends DefaultHandler {
	private Theory theory;
	private CoreGraph graph;
	private Locator documentLocator = null;
	private Map<String,VertexHandler> verts;
	private Set<EdgeHandler> edges;
	private Set<BangBoxHandler> bbs;
	private enum Mode {
		None,
		Graph,
		Component
	}
	private Mode mode = Mode.None;
	private ComponentData componentData = null;

	private static final String GRAPH_ELEM = "graph";
	private static final String VERTEX_ELEM = "vertex";
	private static final String EDGE_ELEM = "edge";
	private static final String BANGBOX_ELEM = "bangbox";

	public GraphHandler(Theory theory, CoreGraph graph) {
		this.theory = theory;
		this.graph = graph;
	}

	public GraphHandler(Theory theory, String name) {
		this.theory = theory;
		this.graph = new CoreGraph(name);
	}

	public CoreGraph getGraph() {
		return graph;
	}

	@Override
	public void setDocumentLocator(Locator locator) {
		this.documentLocator = locator;
	}

	@Override
	public void startDocument() throws SAXException {
		verts = new HashMap<String, VertexHandler>();
		edges = new HashSet<EdgeHandler>();
		bbs = new HashSet<BangBoxHandler>();
	}

	@Override
	public void endDocument() throws SAXException {
		// make copies of the collections to avoid concurrent modification
		// issues
		for (Edge e : new ArrayList<Edge>(graph.getEdges())) {
			graph.removeEdge(e);
		}
		for (BangBox b : new ArrayList<BangBox>(graph.getBangBoxes())) {
			graph.removeBangBox(b);
		}
		for (Vertex v : new ArrayList<Vertex>(graph.getVertices())) {
			graph.removeVertex(v);
		}
		for (VertexHandler h : verts.values()) {
			h.addToGraph();
		}
		for (BangBoxHandler h : bbs) {
			h.addToGraph();
		}
		for (EdgeHandler h : edges) {
			h.addToGraph();
		}
		graph.fireStateChanged();
		verts = null;
		edges = null;
		bbs = null;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		if (mode == Mode.None) {
			if (!GRAPH_ELEM.equals(localName)) {
				throw new SAXParseException(
						"Root element was not 'graph'", documentLocator);
			}
			mode = Mode.Graph;
		} else if (mode == Mode.Graph) {
			if (VERTEX_ELEM.equals(localName)) {
				componentData = new VertexHandler(attributes);
			} else if (EDGE_ELEM.equals(localName)) {
				componentData = new EdgeHandler(attributes);
			} else if (BANGBOX_ELEM.equals(localName)) {
				componentData = new BangBoxHandler(attributes);
			}
			mode = Mode.Component;
		} else if (mode == Mode.Component) {
			componentData.startElement(uri, localName, qName, attributes);
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		if (mode == Mode.Component) {
			componentData.endElement(uri, localName, qName);
		}
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (mode == Mode.Component) {
			componentData.characters(ch, start, length);
		}
	}

	private abstract class ComponentData extends DefaultHandler {
		protected String name;

		public ComponentData(Attributes attributes) throws SAXException {
			this.name = attributes.getValue("", "name");
			if (name == null || name.length() == 0)
				throw new SAXParseException("Required attribute 'name' missing", documentLocator);
		}
		public abstract void addToGraph() throws SAXException;
	}
	enum VMode {
		None, Type, Data
	}
	enum EMode {
		None, Data
	}
	enum BBMode {
		None, Content
	}
	private class VertexHandler extends ComponentData {
		StringBuilder typeName;
		VertexType type;
		VMode mode = VMode.None;
		Vertex vertex;
		DataHandler data;

		public VertexHandler(Attributes attributes) throws SAXException {
			super(attributes);
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			if (mode == VMode.Data) {
				data.startElement(uri, localName, qName, attributes);
			} else if (mode == VMode.None) {
				if ("type".equals(localName)) {
					mode = VMode.Type;
					typeName = new StringBuilder();
				}
				else if ("data".equals(localName)) {
					if (data != null) {
						throw new SAXParseException(
								"Multiple 'data' elements", documentLocator);
					}
					mode = VMode.Data;
					data = new DataHandler();
				}
			}
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {
			if (mode == VMode.Type && "type".equals(localName)) {
				mode = VMode.None;
				String sTypeName = this.typeName.toString();
				if (!sTypeName.equals("edge-point")) {
					type = theory.getVertexType(sTypeName);
					if (type == null) {
						throw new SAXParseException(
								"Unknown vertex type \"" + sTypeName + "\"",
								documentLocator);
					}
				}
			} else if (mode == VMode.Data) {
				if ("data".equals(localName)) {
					mode = VMode.None;
				} else {
					data.endElement(uri, localName, qName);
				}
			} else if (VERTEX_ELEM.equals(localName)) {
				GraphHandler.this.componentData = null;
				GraphHandler.this.mode = Mode.Graph;

				if (this.typeName == null) {
					throw new SAXParseException("Missing vertex type declaration",
							documentLocator);
				}

				if (type == null)
					vertex = Vertex.createBoundaryVertex(name);
				else {
					vertex = Vertex.createVertex(name, type);
					if (type.hasData()) {
						if (data == null || data.data == null) {
							throw new SAXParseException(
									"Missing vertex data", documentLocator);
						}
						vertex.setData(data.data);
					}
				}

				GraphHandler.this.verts.put(name, this);
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			if (mode == VMode.Type)
				typeName.append(ch, start, length);
			else if (mode == VMode.Data)
				data.characters(ch, start, length);
		}

		public void addToGraph() {
			graph.addVertex(vertex);
		}
	}
	private class EdgeHandler extends ComponentData {
		String source;
		String target;
		DataHandler data;
		EMode mode = EMode.None;
		public EdgeHandler(Attributes attributes) throws SAXException {
			super(attributes);

			source = attributes.getValue("", "source");
			if (source == null)
				throw new SAXParseException("Missing 'source' attribute",
						documentLocator);
			if (source.length() == 0)
				throw new SAXParseException("'source' attribute cannot be empty",
						documentLocator);

			target = attributes.getValue("", "target");
			if (target == null)
				throw new SAXParseException("Missing 'target' attribute",
						documentLocator);
			if (target.length() == 0)
				throw new SAXParseException("'target' attribute cannot be empty",
						documentLocator);
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			if (mode == EMode.None && "data".equals(localName)) {
				data = new DataHandler();
				mode = EMode.Data;
			} else if (mode == EMode.Data) {
				data.startElement(uri, localName, qName, attributes);
			}
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {
			if (mode == EMode.Data) {
				if ("data".equals(localName)) {
					mode = EMode.None;
				} else {
					data.endElement(uri, localName, qName);
				}
			} else if (mode == EMode.None && EDGE_ELEM.equals(localName)) {
				GraphHandler.this.componentData = null;
				GraphHandler.this.mode = Mode.Graph;
				// TODO: data
				GraphHandler.this.edges.add(this);
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			if (mode == EMode.Data)
				data.characters(ch, start, length);
		}

		public void addToGraph() throws SAXException {
			Edge e = new Edge(name);
			VertexHandler s = verts.get(source.trim());
			if (s == null) {
				throw new SAXParseException(
						"Source for edge \"" + name + "\" does not exist",
						documentLocator);
			}
			VertexHandler t = verts.get(target.trim());
			if (t == null) {
				throw new SAXParseException(
						"Target for edge \"" + name + "\" does not exist",
						documentLocator);
			}
			graph.addEdge(e, s.vertex, t.vertex);
		}
	}
	private class BangBoxHandler extends ComponentData {
		Set<String> vertices = new HashSet<String>();
		StringBuilder currentVertex = new StringBuilder();
		BBMode mode = BBMode.None;
		public BangBoxHandler(Attributes attributes) throws SAXException {
			super(attributes);
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			if ("vertex".equals(localName)) {
				mode = BBMode.Content;
				currentVertex.setLength(0);
			}
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {
			if (mode == BBMode.Content && "vertex".equals(localName)) {
				if (currentVertex.length() == 0) {
					throw new SAXParseException(
							"'vertex' element cannot be empty",
							documentLocator);
				}
				vertices.add(currentVertex.toString().trim());
			} else if (mode == BBMode.None && BANGBOX_ELEM.equals(localName)) {
				GraphHandler.this.componentData = null;
				GraphHandler.this.mode = Mode.Graph;
				GraphHandler.this.bbs.add(this);
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			if (mode == BBMode.Content)
				currentVertex.append(ch, start, length);
		}

		public void addToGraph() throws SAXException {
			BangBox bb = new BangBox(name);
			Set<Vertex> contents = new HashSet<Vertex>();
			for (String vName : vertices) {
				VertexHandler v = verts.get(vName);
				if (v == null) {
					throw new SAXParseException(
							"No such vertex \"" + vName + "\"",
							documentLocator);
				}
				contents.add(v.vertex);
			}
			graph.addBangBox(bb, contents);
		}
	}
	// quick hack - we only care about the contents of as_string
	private class DataHandler extends DefaultHandler {
		StringBuilder asString;
		GraphElementData data;

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			if ("string_of".equals(localName)) {
				asString = new StringBuilder();
			}
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {
			if (asString != null && "string_of".equals(localName)) {
				data = new GraphElementData(asString.toString());
				asString = null;
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			if (asString != null)
				asString.append(ch, start, length);
		}
	}
}
