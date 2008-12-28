package quanto.gui;


import java.util.HashMap;
import java.util.Map;

import net.n3.nanoxml.*;

import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.util.EdgeType;

public class QuantoGraph extends SparseGraph<QVertex, QEdge>
implements HasName {
	private static final long serialVersionUID = -1519901566511300787L;
	protected String name;

	public QuantoGraph(String name) {
		this.name = name;
	}

	/**
	 * Use this constructor for unnamed graphs. The idea is you
	 * should do null checks before sending the name to the core.
	 */
	public QuantoGraph() {
		this.name = null;
	}

	public Map<String,QVertex> getVertexMap() {
		Map<String, QVertex> verts =
			new HashMap<String, QVertex>();
		for (QVertex v : getVertices()) {
			v.old = true;
			verts.put(v.getName(), v);
		}
		return verts;
	}

	public QuantoGraph fromXml(String xml) {
		try {
			IXMLParser parser = XMLParserFactory.createDefaultXMLParser(new StdXMLBuilder());
			parser.setReader(StdXMLReader.stringReader(xml));
			IXMLElement root = (IXMLElement)parser.parse();
			fromXml(root);
		} catch (XMLException e) {
			System.out.println("Error parsing XML.");
		} catch (ClassNotFoundException e) {
			throw new RuntimeException(e);
		} catch (InstantiationException e) {
			throw new RuntimeException(e);
		} catch (IllegalAccessException e) {
			throw new RuntimeException(e);
		}
		return this;
	}

	public QuantoGraph fromXml(IXMLElement graphNode) {
		synchronized (this) {
			Map<String, QVertex> verts = getVertexMap();

			for (Object obj : graphNode.getChildrenNamed("vertex")) {
				IXMLElement vertexNode = (IXMLElement)obj;
				QVertex v = new QVertex();
				
				IXMLElement ch;
				
				ch = vertexNode.getFirstChildNamed("colour");
				if (ch!=null) v.setVertexType(ch.getContent());
				ch = vertexNode.getFirstChildNamed("name");
				if (ch!=null) v.setName(ch.getContent());
				ch = vertexNode.getFirstChildNamed("boundary");
				if (ch!=null && ch.getContent().equals("true"))
					v.setVertexType(QVertex.Type.BOUNDARY);
				
				
				QVertex old_v = verts.get(v.getName());
				if (old_v == null) {
					verts.put(v.getName(), v);
					this.addVertex(v);
				} else {
					old_v.updateTo(v);
				}
			} // foreach vertex
			
			// Prune removed vertices
			for (QVertex v : verts.values()) {
				if (v.old) removeVertex(v);
			}


			for (Object obj : graphNode.getChildrenNamed("edge")) {
				IXMLElement edgeNode = (IXMLElement)obj;

				QVertex source = null, target = null;
				String ename = null;
				IXMLElement ch = null;
				
				ch = edgeNode.getFirstChildNamed("source");
				if (ch!=null) source = verts.get(ch.getContent());
				ch = edgeNode.getFirstChildNamed("target");
				if (ch!=null) target = verts.get(ch.getContent());
				ch = edgeNode.getFirstChildNamed("name");
				if (ch!=null) ename = ch.getContent();

				if (source != null || target != null) {
					this.addEdge(new QEdge(ename),
						source, target, EdgeType.DIRECTED);
				} else {
					throw new RuntimeException(
							"Edge ".concat(ename).concat(" has an undefined endpoint."));
				}
			} // foreach edge
		} // synchronised(this)
		return this;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
