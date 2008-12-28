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

	/**
	 * Parse XML from a string. If the core gives mal-formed XML,
	 * the front-end SHOULD crash, so throw QuantoCore.FatalError.
	 * @param xml
	 * @return
	 */
	public QuantoGraph fromXml(String xml) {
		try {
			IXMLParser parser = XMLParserFactory.createDefaultXMLParser(new StdXMLBuilder());
			parser.setReader(StdXMLReader.stringReader(xml));
			IXMLElement root = (IXMLElement)parser.parse();
			fromXml(root);
		} catch (XMLException e) {
			throw new QuantoCore.FatalError("Error parsing XML.");
		} catch (ClassNotFoundException e) {
			throw new QuantoCore.FatalError(e);
		} catch (InstantiationException e) {
			throw new QuantoCore.FatalError(e);
		} catch (IllegalAccessException e) {
			throw new QuantoCore.FatalError(e);
		}
		return this;
	}

	/**
	 * Populate this graph using a given DOM node. This is in
	 * a separate method so graph defs can be nested inside of
	 * bigger XML blocks, e.g. rewrites.
	 * @param graphNode
	 * @return
	 */
	public QuantoGraph fromXml(IXMLElement graphNode) {
		synchronized (this) {
			Map<String, QVertex> verts = getVertexMap();

			for (Object obj : graphNode.getChildrenNamed("vertex")) {
				IXMLElement vertexNode = (IXMLElement)obj;
				QVertex v = new QVertex();
				
				try {
					v.setVertexType(
							vertexNode
							.getFirstChildNamed("colour")
							.getContent());
					v.setName(vertexNode
							.getFirstChildNamed("name")
							.getContent());
					if (vertexNode
							.getFirstChildNamed("boundary")
							.getContent().equals("true"))
						v.setVertexType(QVertex.Type.BOUNDARY);
					
					IXMLElement expr = vertexNode
						.getFirstChildNamed("angleexpr");
					if (expr == null) {
						v.setAngle("0");
					} else {
						v.setAngle(expr
								.getFirstChildNamed("as_string")
								.getContent());
					}
				} catch (NullPointerException e) {
					/* if NullPointerException is thrown, the
					 * core has most likely neglected to include
					 * a required field, so the GUI should crash.
					 */
					e.printStackTrace();
					throw new QuantoCore.FatalError(
							"Error reading graph XML.");
				}
				
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

				if (source == null || target == null || ename == null)
					throw new QuantoCore.FatalError(
							"Bad edge definition in XML.");
				
				this.addEdge(new QEdge(ename),
					source, target, EdgeType.DIRECTED);
				
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
