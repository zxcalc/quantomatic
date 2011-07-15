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
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import quanto.core.Theory;
import quanto.core.data.BangBox;
import quanto.core.data.CoreGraph;
import quanto.core.data.Edge;
import quanto.core.data.Vertex;

public class GraphFragmentHandler extends DefaultFragmentHandler<CoreGraph> {
	private Theory theory;
	private CoreGraph graph;
	private Map<String,Vertex> verts;
	private Set<EdgeFragmentHandler.EdgeData> edges;
	private Set<BangBoxFragmentHandler.BangBoxData> bbs;
	private enum Mode {
		None,
		Graph,
		Component
	}
	private Mode mode = Mode.None;
	private FragmentHandler componentData = null;
	private int unknownElementDepth = 0;

	private static final String GRAPH_ELEM = "graph";
	private static final String VERTEX_ELEM = "vertex";
	private static final String EDGE_ELEM = "edge";
	private static final String BANGBOX_ELEM = "bangbox";

	public GraphFragmentHandler(Theory theory, CoreGraph graph) {
		this.theory = theory;
		this.graph = graph;
	}

	public GraphFragmentHandler(Theory theory, String name) {
		this.theory = theory;
		this.graph = new CoreGraph(name);
	}

	public boolean isComplete() {
		return mode == Mode.None;
	}

	public CoreGraph buildResult() throws SAXException {
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
		for (Vertex v : verts.values()) {
			graph.addVertex(v);
		}
		for (BangBoxFragmentHandler.BangBoxData b : bbs) {
			Set<Vertex> contents = new HashSet<Vertex>();
			for (String vName : b.vertexNames) {
				Vertex v = verts.get(vName);
				if (v == null) {
					throw new SAXParseException(
						"No such vertex \"" + vName +
						"\" in !-box \"" +
						b.bangBox.getCoreName() +
						"\"",
						locator);
				}
				contents.add(v);
			}
			graph.addBangBox(b.bangBox, contents);
		}
		for (EdgeFragmentHandler.EdgeData e : edges) {
			Vertex s = verts.get(e.sourceName);
			if (s == null) {
				throw new SAXParseException(
					"Source for edge \"" +
					e.edge.getCoreName() +
					"\" does not exist",
					locator);
			}
			Vertex t = verts.get(e.targetName);
			if (t == null) {
				throw new SAXParseException(
					"Target for edge \"" +
					e.edge.getCoreName() +
					"\" does not exist",
					locator);
			}
			graph.addEdge(e.edge, s, t);
		}
		graph.fireStateChanged();
		return graph;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		if (unknownElementDepth > 0) {
			++unknownElementDepth;
		} else if (mode == Mode.None) {
			if (!GRAPH_ELEM.equals(localName)) {
				throw new SAXParseException(
						"Root element was not 'graph'", locator);
			}
			verts = new HashMap<String, Vertex>();
			edges = new HashSet<EdgeFragmentHandler.EdgeData>();
			bbs = new HashSet<BangBoxFragmentHandler.BangBoxData>();
			mode = Mode.Graph;
		} else if (mode == Mode.Graph) {
			if (VERTEX_ELEM.equals(localName)) {
				componentData = new VertexFragmentHandler(theory);
			} else if (EDGE_ELEM.equals(localName)) {
				componentData = new EdgeFragmentHandler();
			} else if (BANGBOX_ELEM.equals(localName)) {
				componentData = new BangBoxFragmentHandler();
			} else {
				++unknownElementDepth;
				return;
			}
			mode = Mode.Component;
			componentData.setDocumentLocator(locator);
			componentData.startElement(uri, localName, qName, attributes);
		} else if (mode == Mode.Component) {
			componentData.startElement(uri, localName, qName, attributes);
		} else {
			++unknownElementDepth;
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		if (unknownElementDepth > 0) {
			--unknownElementDepth;
		} else if (mode == Mode.Component) {
			componentData.endElement(uri, localName, qName);
			if (componentData.isComplete()) {
				mode = Mode.Graph;
				if (componentData instanceof VertexFragmentHandler) {
					Vertex v = (Vertex)componentData.buildResult();
					verts.put(v.getCoreName(), v);
				} else if (componentData instanceof EdgeFragmentHandler) {
					edges.add(((EdgeFragmentHandler)componentData).buildResult());
				} else if (componentData instanceof BangBoxFragmentHandler) {
					bbs.add(((BangBoxFragmentHandler)componentData).buildResult());
				}
				componentData = null;
			}
		} else if (mode == Mode.Graph) {
			// complete
            assert(GRAPH_ELEM.equals(localName));
            mode = Mode.None;
		} else {
			throw new IllegalStateException("endElement cannot be called without a corresponding startElement; element was " + localName);
		}
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (mode == Mode.Component) {
			componentData.characters(ch, start, length);
		}
	}
}
