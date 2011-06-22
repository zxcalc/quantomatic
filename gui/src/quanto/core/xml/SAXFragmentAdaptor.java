package quanto.core.xml;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class SAXFragmentAdaptor<T> extends DefaultHandler {
	private FragmentHandler<? extends T> handler;
	private T result = null;

	public SAXFragmentAdaptor(FragmentHandler<? extends T> handler) {
		this.handler = handler;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		handler.startElement(uri, localName, qName, attributes);
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {
		handler.endElement(uri, localName, qName);
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		handler.characters(ch, start, length);
	}

	@Override
	public void setDocumentLocator(Locator locator) {
		handler.setDocumentLocator(locator);
	}

	@Override
	public void endDocument() throws SAXException {
		result = handler.buildResult();
	}

	public T getResult() {
		return result;
	}
}
