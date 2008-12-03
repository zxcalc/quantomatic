package quanto.gui;

import java.io.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.*;
import org.w3c.dom.*;
import org.xml.sax.InputSource;

import edu.uci.ics.jung.graph.*;

public class QuantoGraph extends SparseGraph<QuantoVertex, String> {
	private static final long serialVersionUID = -1519901566511300787L;

	public void fromXml(String xml) {
		synchronized (this) {
			Collection<QuantoVertex> old_vs =
				new ArrayList<QuantoVertex>(getVertices());
			for (QuantoVertex v : old_vs) removeVertex(v);
			
			
			try {
				DocumentBuilder db = DocumentBuilderFactory.newInstance()
						.newDocumentBuilder();
				Document doc = db.parse(new InputSource(new StringReader(xml)));
				NodeList vs = doc.getElementsByTagName("vertex");
				Map<String, QuantoVertex> verts = new HashMap<String, QuantoVertex>();

				for (int i = 0; i < vs.getLength(); ++i) {
					QuantoVertex v = new QuantoVertex();
					NodeList children = vs.item(i).getChildNodes();
					// System.out.println("vertex");
					String name, val;
					for (int j = 0; j < children.getLength(); ++j) {
						name = children.item(j).getNodeName();
						val = children.item(j).getTextContent();
						// System.out.println(name.concat(": ").concat(val));
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

					verts.put(v.getName(), v);
					this.addVertex(v);
				}

				NodeList es = doc.getElementsByTagName("edge");

				for (int i = 0; i < es.getLength(); ++i) {
					NodeList children = es.item(i).getChildNodes();

					QuantoVertex source = null, target = null;
					String name = null, val;
					for (int j = 0; j < children.getLength(); ++j) {
						name = children.item(j).getNodeName();
						val = children.item(j).getTextContent();
						if (name == "source")
							source = verts.get(val);
						else if (name == "target")
							target = verts.get(val);
					}

					if (source == null || target == null)
						throw new Exception(
								"Edge declared before its vertices.");
					this.addEdge(name, source, target);
				}

			} catch (Exception e) {
				System.err.println("Error parsing XML:");
				System.err.println(e.toString());
			}
		}

	}
}
