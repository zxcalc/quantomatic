/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.data.xml;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

/**
 *
 * @author alex
 */
public interface FragmentHandler<T> {
	void setDocumentLocator(Locator locator);
	void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException;
	void endElement(String uri, String localName, String qName) throws SAXException;
	void characters(char[] ch, int start, int length) throws SAXException;
    void ignorableWhitespace(char[] ch, int start, int length) throws SAXException;
	boolean isComplete();
	T buildResult() throws SAXException;
}
