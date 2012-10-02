package quanto.core.data.xml;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 *
 * @author alex
 */
public abstract class CoreObjectFragmentHandler<T> extends DefaultFragmentHandler<T> {
	protected String name;
	protected void extractName(Attributes attributes) throws SAXException {
		this.name = attributes.getValue("", "name");
		if (name == null || name.length() == 0)
			throw new SAXParseException("Required attribute 'name' missing", locator);
	}
}
