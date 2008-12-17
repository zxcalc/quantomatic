package quanto.gui;

import java.io.*;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.*;
import org.w3c.dom.*;
import org.xml.sax.InputSource;

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
	
	public void fromXml(String xml) {
		synchronized (this) {
			try {
				DocumentBuilder db = DocumentBuilderFactory.newInstance()
						.newDocumentBuilder();
				Document doc = db.parse(new InputSource(new StringReader(xml)));
				NodeList vs = doc.getElementsByTagName("vertex");

				Map<String, QVertex> verts = getVertexMap();

				for (int i = 0; i < vs.getLength(); ++i) {
					QVertex v = new QVertex();
					NodeList children = vs.item(i).getChildNodes();
					String name, val;
					
					
					for (int j = 0; j < children.getLength(); ++j) {
						name = children.item(j).getNodeName();
						val = children.item(j).getTextContent();
						if (name == "colour"
								&& v.getVertexType() != QVertex.Type.BOUNDARY) {
							if (val.equals("red"))
								v.setVertexType(QVertex.Type.RED);
							else if (val.equals("green"))
								v.setVertexType(QVertex.Type.GREEN);
							else if (val.equals("H"))
								v.setVertexType(QVertex.Type.HADAMARD);
							else
								throw new Exception(
										"Invalid colour on vertex: \"".concat(
												val).concat("\""));
						} else if (name.equals("boundary")) {
							if (val.equals("true"))
								v.setVertexType(QVertex.Type.BOUNDARY);
						} else if (name.equals("name")) {
							v.setName(children.item(j).getTextContent());
						}
					}
					QVertex old_v = verts.get(v.getName());
					if (old_v == null) {
						verts.put(v.getName(), v);
						this.addVertex(v);
					} else {
						old_v.updateTo(v);
					}
				}

				NodeList es = doc.getElementsByTagName("edge");

				for (int i = 0; i < es.getLength(); ++i) {
					NodeList children = es.item(i).getChildNodes();

					QVertex source = null, target = null;
					String name = null, val, ename=null;
					for (int j = 0; j < children.getLength(); ++j) {
						name = children.item(j).getNodeName();
						val = children.item(j).getTextContent();
						
						if (name == "source")
							source = verts.get(val);
						else if (name == "target")
							target = verts.get(val);
						else if (name == "name")
							ename = val;
					}

					if (source == null || target == null)
						throw new Exception(
								"Edge declared before its vertices.");
					
					this.addEdge(new QEdge(ename),
							source, target, EdgeType.DIRECTED);
				}
				
				// Prune removed vertices
				for (QVertex v : verts.values()) {
					if (v.old) removeVertex(v);
				}

			} catch (Exception e) {
				System.err.println("Error parsing XML:");
				System.err.println(e.toString());
			}
		}

	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
