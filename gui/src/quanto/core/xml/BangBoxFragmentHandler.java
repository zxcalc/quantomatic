/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.xml;

import java.util.Collection;
import java.util.HashSet;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import quanto.core.data.BangBox;

/**
 *
 * @author alex
 */
public class BangBoxFragmentHandler extends CoreObjectFragmentHandler<BangBoxFragmentHandler.BangBoxData> {
	public static class BangBoxData {
		public BangBox bangBox;
		public Collection<String> vertexNames = new HashSet<String>();
	}
	enum Mode {
		None, BangBox, Vertex
	}
	private Mode mode = Mode.None;
	private BangBoxData bangBoxData = new BangBoxData();
	private StringBuilder currentVertex = new StringBuilder();
	private int unknownElementDepth = 0;

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		if (unknownElementDepth > 0) {
			++unknownElementDepth;
		} else if (mode == Mode.None) {
			if (!"bangbox".equals(localName))
				throw new SAXException("Start of bangbox fragment expected");

			extractName(attributes);

			bangBoxData.bangBox = new BangBox(name);

			mode = Mode.BangBox;
		} else if (mode == Mode.BangBox && "vertex".equals(localName)) {
			mode = Mode.Vertex;
			currentVertex.setLength(0);
		} else {
			++unknownElementDepth;
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		if (unknownElementDepth > 0) {
			--unknownElementDepth;
		} else if (mode == Mode.Vertex && "vertex".equals(localName)) {
			if (currentVertex.length() == 0) {
				throw new SAXParseException(
						"'vertex' element cannot be empty",
						locator);
			}
			bangBoxData.vertexNames.add(currentVertex.toString().trim());
			mode = Mode.BangBox;
		} else if (mode == Mode.BangBox && "bangbox".equals(localName)) {
			mode = Mode.None;
		} else {
			throw new IllegalStateException("endElement cannot be called without a corresponding startElement; element was " + localName);
		}
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (mode == Mode.Vertex)
			currentVertex.append(ch, start, length);
	}

	public boolean isComplete() {
		return mode == Mode.None;
	}

	public BangBoxData buildResult() throws SAXException {
		return bangBoxData;
	}
}
