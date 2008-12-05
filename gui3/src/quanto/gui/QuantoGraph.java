package quanto.gui;

import java.io.*;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.*;
import org.w3c.dom.*;
import org.xml.sax.InputSource;

import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.util.EdgeType;

public class QuantoGraph extends SparseGraph<QuantoVertex, String> {
	private static final long serialVersionUID = -1519901566511300787L;

	public Map<String,QuantoVertex> getVertexMap() {
		Map<String, QuantoVertex> verts =
			new HashMap<String, QuantoVertex>();
		for (QuantoVertex v : getVertices()) {
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

				Map<String, QuantoVertex> verts = getVertexMap();

				for (int i = 0; i < vs.getLength(); ++i) {
					QuantoVertex v = new QuantoVertex();
					NodeList children = vs.item(i).getChildNodes();
					String name, val;
					
					
					for (int j = 0; j < children.getLength(); ++j) {
						name = children.item(j).getNodeName();
						val = children.item(j).getTextContent();
						if (name == "colour"
								&& v.getVertexType() != QuantoVertex.Type.BOUNDARY) {
							if (val.equals("red"))
								v.setVertexType(QuantoVertex.Type.RED);
							else if (val.equals("green"))
								v.setVertexType(QuantoVertex.Type.GREEN);
							else
								throw new Exception(
										"Invalid colour on vertex: \"".concat(
												val).concat("\""));
						} else if (name.equals("boundary")) {
							if (val.equals("true"))
								v.setVertexType(QuantoVertex.Type.BOUNDARY);
						} else if (name.equals("name")) {
							v.setName(children.item(j).getTextContent());
						}
					}
					QuantoVertex old_v = verts.get(v.getName());
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

					QuantoVertex source = null, target = null;
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
					
					this.addEdge(ename, source, target, EdgeType.DIRECTED);
				}
				
				// Prune removed vertices
				for (QuantoVertex v : verts.values()) {
					if (v.old) removeVertex(v);
				}

			} catch (Exception e) {
				System.err.println("Error parsing XML:");
				System.err.println(e.toString());
			}
		}

	}
}
