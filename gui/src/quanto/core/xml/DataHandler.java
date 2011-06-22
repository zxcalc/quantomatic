package quanto.core.xml;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import quanto.core.data.GraphElementData;

public class DataHandler extends DefaultFragmentHandler<GraphElementData> {
	private StringBuilder asString;
	private GraphElementData data;
	private int depth = 0;

	public boolean isComplete() {
		return depth == 0;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		++depth;
		if ("string_of".equals(localName)) {
			asString = new StringBuilder();
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		if (asString != null && "string_of".equals(localName)) {
			data = new GraphElementData(asString.toString());
			asString = null;
		}
		--depth;
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (asString != null)
			asString.append(ch, start, length);
	}

	public GraphElementData buildResult() throws SAXException {
		return data;
	}
	
}
