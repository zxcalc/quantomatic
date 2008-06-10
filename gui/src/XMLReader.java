import processing.xml.XMLElement;

/*
 * Class to interpret XML messages received from the back end
 */
public class XMLReader {

	private Graph g = null;

	class XMLReaderException extends RuntimeException {
		private static final long serialVersionUID = 1L;

		XMLReaderException(String m) {
			super(m);
		}
	}

	public Graph parseGraph(String s) {
		return parseGraph(mkXMLFromString(s));
	}

	public Graph parseGraph(XMLElement xml) {
		assertElement(xml, "graph");

		g = new Graph();

		// assume that the only interesting thing in a graph
		// are its nodes and edges -- probably need to modify
		// for !-boxes
		XMLElement[] vertices = xml.getChildren("vertex");
		for (int i = 0; i < vertices.length; i++) {
			g.addVertex(parseVertex(vertices[i]));
		}
		XMLElement[] edges = xml.getChildren("edge");
		for (int i = 0; i < edges.length; i++) {
			g.addEdge(parseEdge(edges[i]));
		}
		return g;
	}

	public Vertex parseVertex(String s) {
		return parseVertex(mkXMLFromString(s));
	}

	public Vertex parseVertex(XMLElement xml) {
		assertElement(xml, "vertex");

		String name = getChildContents(xml, "name", "ERROR", true);
		Vertex v = new Vertex(name, 0, 0);

		String bnd = getChildContents(xml, "boundary", "false");
		if (bnd.equals("true")) {
			v.setColor("boundary");
			// no need to set any other properties
		} else if (bnd.equals("false")) {
			String col = getChildContents(xml, "colour", "ERROR", true);
			if (col.equals("H")) {
				v.setColor(col);
			} else if (col.equals("red") || col.equals("green")) {
				v.setColor(col);
				// non-H vertices have angle exprs
				XMLElement angle_expr = xml.getChild("angleexpr");
				if (angle_expr != null)
					v.setAngle(parseAngleExpr(angle_expr));
				else
					v.setAngle("");
			} else { // colour was not valid
				badContents("colour", col);
			}
		} else { // boundary is neither true nor false!
			badContents("boundary", bnd);
		}
		return v;
	}

	public Edge parseEdge(String s) {
		return parseEdge(mkXMLFromString(s));
	}

	public Edge parseEdge(XMLElement xml) {
		assertElement(xml, "edge");
		String name = getChildContents(xml, "name", "ERROR", true);
		String target = getChildContents(xml, "target", "ERROR", true);
		String source = getChildContents(xml, "source", "ERROR", true);
		Vertex src = (Vertex) g.vertices.get(source);
		Vertex tgt = (Vertex) g.vertices.get(target);
		return new Edge(name, src, tgt);
	}

	public String parseAngleExpr(String s) {
		return parseAngleExpr(mkXMLFromString(s));
	}

	// angle expressiomns are not parsed at the moment.
	public String parseAngleExpr(XMLElement xml) {
		return "";
	}

	/** *************************************************************** */
	/** * down here are helper methods ** */

	private String getChildContents(XMLElement xml, String child, String def_val) {
		return getChildContents(xml, child, def_val, false);
	}

	@SuppressWarnings("unused")
	private String getChildContents(XMLElement xml, String child) {
		return getChildContents(xml, child, "", false);
	}

	private void assertElement(XMLElement xml, String wanted) {
		String elmt = xml.getName();
		if (!elmt.equals(wanted)) {
			throw new XMLReaderException("Parse failed!  Expected " + wanted
					+ " but saw " + elmt);
		}
	}

	private void missingChild(String parent, String child) {
		throw new XMLReaderException("Element " + parent
				+ " lacks required child element " + child + ".");
	}

	private void badContents(String elem, String value) {
		throw new XMLReaderException("String " + value
				+ " is not valid as contents of element " + elem + ".");
	}

	private String getChildContents(XMLElement xml, String child,
			String default_value, boolean required) {
		XMLElement ch = xml.getChild(child);
		boolean defined = ch != null;
		if (required && !defined)
			missingChild(xml.getName(), child); // aborts
		return defined ? ch.getContent() : default_value;
	}

	private XMLElement mkXMLFromString(String s) {
		XMLElement x = new XMLElement();
		x.parseString(s);
		return x;
	}

}
