package quanto.core.xml;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import quanto.core.Theory;
import quanto.core.data.GraphElementData;
import quanto.core.data.Vertex;
import quanto.core.data.VertexType;

public class VertexFragmentHandler extends CoreObjectFragmentHandler<Vertex> {
	private enum Mode {
		None, Vertex, Type, Data
	}
	private Mode mode = Mode.None;
	private StringBuilder typeName = null;
	private VertexType type = null;
	private Vertex vertex = null;
	private DataHandler data = null;
	private Theory theory;
	private int unknownElementDepth = 0;

	public VertexFragmentHandler(Theory theory) {
		this.theory = theory;
	}

	public boolean isComplete() {
		return mode == Mode.None;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		if (mode == Mode.None) {
			if (!"vertex".equals(localName))
				throw new SAXException("Start of vertex fragment expected");
			extractName(attributes);
			mode = Mode.Vertex;
		} else if (mode == Mode.Data) {
			data.startElement(uri, localName, qName, attributes);
		} else if (mode == Mode.Vertex) {
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
		} else {
			++unknownElementDepth;
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		if (unknownElementDepth > 0) {
			--unknownElementDepth;
		} else if (mode == Mode.Type && "type".equals(localName)) {
			mode = Mode.Vertex;
			String sTypeName = this.typeName.toString();
			if (!sTypeName.equals("edge-point")) {
				type = theory.getVertexType(sTypeName);
				if (type == null) {
					throw new SAXParseException(
							"Unknown vertex type \"" + sTypeName + "\"",
							locator);
				}
			}
		} else if (mode == Mode.Data) {
			data.endElement(uri, localName, qName);
			if (data.isComplete())
				mode = Mode.Vertex;
		} else if (mode == Mode.Vertex && "vertex".equals(localName)) {
			if (this.typeName == null) {
				throw new SAXParseException("Missing vertex type declaration",
						locator);
			}

			if (type == null)
				vertex = Vertex.createBoundaryVertex(name);
			else {
				vertex = Vertex.createVertex(name, type);
				if (type.hasData()) {
					GraphElementData geData = null;
					if (data != null)
						geData = data.buildResult();
					if (geData == null) {
						throw new SAXParseException(
								"Missing vertex data", locator);
					}
					vertex.setData(geData);
				}
			}
			mode = Mode.None;
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

	public Vertex buildResult() throws SAXException {
		return vertex;
	}
}
