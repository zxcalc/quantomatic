package quanto.core.xml;

import java.util.HashMap;
import java.util.Map;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import quanto.core.data.Vertex;

public class VertexUserDataFragmentHandler extends CoreObjectFragmentHandler<Map<String, String>> {
	private enum Mode {
		None, VertexUData, Entry
	}
	private Mode mode = Mode.None;
	private Map<String, String> entries;
	private FragmentHandler entry = null;
	private int unknownElementDepth = 0;

	public VertexUserDataFragmentHandler() {}

	public boolean isComplete() {
		return mode == Mode.None;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		if (mode == Mode.None) {
			if (!"vertex_data".equals(localName))
				throw new SAXException("Start of vertex_data fragment expected");
			extractName(attributes);
			entries = new HashMap<String, String>();
			mode = Mode.VertexUData;
		} else if (mode == Mode.VertexUData) {
			entry = new UserDataEntryFragmentHandler();
			entry.setDocumentLocator(locator);
			entry.startElement(uri, localName, qName, attributes);
			mode = Mode.Entry;
		} else if (mode == Mode.Entry) {
			entry.startElement(uri, localName, qName, attributes);
		}
		else {
				++unknownElementDepth;
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		if (unknownElementDepth > 0) {
			--unknownElementDepth;
		} else if (mode == Mode.Entry && "entry".equals(localName)) {
			entry.endElement(uri, localName, qName);
			if (entry.isComplete()) {
				mode = Mode.VertexUData;
				entries.put(((UserDataEntryFragmentHandler)entry).name,
							((UserDataEntryFragmentHandler)entry).buildResult());
				entry = null;
			}
			mode = Mode.VertexUData;
		} else if (mode == Mode.VertexUData && "vertex_data".equals(localName)) {
			mode = Mode.None;
		} else {
			throw new IllegalStateException("endElement cannot be called without a corresponding startElement; element was " + localName);
		}
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (mode == Mode.Entry)
			entry.characters(ch, start, length);
	}

	public Map<String, String> buildResult() throws SAXException {
		return entries;
	}
}
