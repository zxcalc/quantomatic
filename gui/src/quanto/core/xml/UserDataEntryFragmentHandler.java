
package quanto.core.xml;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class UserDataEntryFragmentHandler extends CoreObjectFragmentHandler<String> {
	private enum Mode {
		None, Data
	}
	private Mode mode = Mode.None;
	private StringBuilder data = null;
	private int unknownElementDepth = 0;

	public UserDataEntryFragmentHandler() {}

	public boolean isComplete() {
		return mode == Mode.None;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		if (mode == Mode.None) {
			if (!"entry".equals(localName))
				throw new SAXException("Start of user data entry fragment expected");
			extractName(attributes);
			data = new StringBuilder();
			mode = Mode.Data;
		} else {
			++unknownElementDepth;
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		if (unknownElementDepth > 0) {
			--unknownElementDepth;
		} else if (mode == Mode.Data && "entry".equals(localName)) {
			mode = Mode.None;
		} else {
			throw new IllegalStateException("endElement cannot be called without a corresponding startElement; element was " + localName);
		}
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (mode == Mode.Data)
			data.append(ch, start, length);
	}

	public String buildResult() throws SAXException {
		return data.toString();
	}
}
