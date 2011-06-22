/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.xml;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import quanto.core.data.Edge;

/**
 *
 * @author alex
 */
public class EdgeFragmentHandler extends CoreObjectFragmentHandler<EdgeFragmentHandler.EdgeData> {
	public static class EdgeData {
		public Edge edge;
		public String sourceName;
		public String targetName;
	}
	private enum Mode {
		None, Edge, Type, Data
	}
	private EdgeData edgeData;
	private DataHandler data;
	private StringBuilder typeName = null;
	private Mode mode = Mode.None;
	private int unknownElementDepth = 0;

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		if (unknownElementDepth > 0) {
			++unknownElementDepth;
		} else if (mode == Mode.None) {
			if (!"edge".equals(localName))
				throw new SAXException("Start of edge fragment expected");

			extractName(attributes);
			String source = attributes.getValue("", "source");
			if (source == null)
				throw new SAXParseException("Missing 'source' attribute",
						locator);
			source = source.trim();
			if (source.length() == 0)
				throw new SAXParseException("'source' attribute cannot be empty",
						locator);

			String target = attributes.getValue("", "target");
			if (target == null)
				throw new SAXParseException("Missing 'target' attribute",
						locator);
			target = target.trim();
			if (target.length() == 0)
				throw new SAXParseException("'target' attribute cannot be empty",
						locator);

			edgeData = new EdgeData();
			edgeData.edge = new Edge(name);
			edgeData.sourceName = source;
			edgeData.targetName = target;

			mode = Mode.Edge;
		} else if (mode == Mode.Edge) {
			if ("type".equals(localName)) {
				mode = Mode.Type;
				typeName = new StringBuilder();
			}
			else if ("data".equals(localName)) {
				if (data != null) {
					throw new SAXParseException(
							"Multiple 'data' elements", locator);
				}
				mode = Mode.Data;
				data = new DataHandler();
				data.setDocumentLocator(locator);
				data.startElement(uri, localName, qName, attributes);
			}
			else {
				++unknownElementDepth;
			}
		} else if (mode == Mode.Data) {
			data.startElement(uri, localName, qName, attributes);
		} else {
			++unknownElementDepth;
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		if (unknownElementDepth > 0) {
			--unknownElementDepth;
		} else if (mode == Mode.Type && "type".equals(localName)) {
			mode = Mode.Edge;
			// TODO: type info
		} else if (mode == Mode.Data) {
			data.endElement(uri, localName, qName);
			if (data.isComplete()) {
				mode = Mode.Edge;
			}
		} else if (mode == Mode.Edge && "edge".equals(localName)) {
			mode = Mode.None;
			// TODO: parse type/data info
		} else {
			throw new IllegalStateException("endElement cannot be called without a corresponding startElement; element was " + localName);
		}
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (mode == Mode.Type)
			typeName.append(ch, start, length);
		else if (mode == Mode.Data)
			data.characters(ch, start, length);
	}

	public boolean isComplete() {
		return mode == Mode.None;
	}

	public EdgeData buildResult() throws SAXException {
		return edgeData;
	}
}
