/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.xml;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

/**
 *
 * @author alex
 */
public abstract class DefaultFragmentHandler<T> implements FragmentHandler<T> {
	protected Locator locator = null;

	public void setDocumentLocator(Locator locator) {
		this.locator = locator;
	}

	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
	}

	public void endElement(String uri, String localName, String qName) throws SAXException {
	}

	public void characters(char[] ch, int start, int length) throws SAXException {
	}
}
