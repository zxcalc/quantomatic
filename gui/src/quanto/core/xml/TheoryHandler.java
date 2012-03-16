/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.xml;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.LinkedList;
import org.xml.sax.*;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;
import quanto.core.data.SvgVertexVisualizationData;
import quanto.core.data.VertexType;
import uk.me.randomguy3.svg.SVGCache;

/**
 * Handles theory parsing
 *
 * @author alex
 */
public class TheoryHandler extends DefaultHandler {
    
    /**
     * Convenience method to parse a theory file
     *
     * @param file  the location of the theory file
     * @return the parsed data
     * @throws SAXException  if the file could not be parsed
     * @throws IOException  if the file could not be read
     */
    public static Data parse(File file) throws SAXException, IOException {
		return parse(file.toURI().toURL());
    }
    
    /**
     * Convenience method to parse a theory at a given URL
     *
     * @param url  the location of the theory file
     * @return the parsed data
     * @throws SAXException  if the file could not be parsed
     * @throws IOException  if the file could not be read
     */
    public static Data parse(URL url) throws SAXException, IOException {
		XMLReader reader = XMLReaderFactory.createXMLReader();

        TheoryHandler handler = new TheoryHandler();
		reader.setContentHandler(handler);

		reader.parse(new InputSource(url.toExternalForm()));
        
        return handler.getData();
    }

    public static class Data {
        public String name;
        public String coreName;
        public ArrayList<VertexType> vertices = new ArrayList<VertexType>();
        public LinkedList<URL> dependentResources = new LinkedList<URL>();
    }
    private Data data = new Data();
    private VertexType.DataType dataType;
    private URI svgdocURI;
    private URL baseURL;
    private URL locatorBaseURL;
    private String theoryName;
    private String implementedTheoryName;
    private String vertexName;
    private String labelFill;
    private String mnemonic;
    private Locator locator;
    private int unknownElementDepth = 0;
    private Mode mode = Mode.None;

    private enum Mode {
        None,
        Theory,
        Nodetype,
        Visualization
    }

    public TheoryHandler() {
    }

    public void setBaseURL(URL baseURL) {
        this.baseURL = baseURL;
    }

    public URL getBaseURL() {
        return baseURL;
    }

    public Data getData() {
        return data;
    }

    private URL resolveUrl(String url) throws MalformedURLException {
        if (baseURL != null)
            return new URL(baseURL, url);
        else if (locatorBaseURL != null)
            return new URL(locatorBaseURL, url);
        else
            return new URL(url);
    }

    @Override
    public void setDocumentLocator(Locator locator) {
        this.locator = locator;
        locatorBaseURL = null;
        if (locator != null) {
            try {
                locatorBaseURL = new URL(locator.getSystemId());
            } catch (MalformedURLException ex) {}
        }
    }

    @Override
    public void startDocument() throws SAXException {
        data = new Data();
    }

    @Override
    public void startElement(String namespaceUri, String localName,
            String qualifiedName, Attributes attributes) throws SAXException {
        if (unknownElementDepth > 0) {
            ++unknownElementDepth;
        } else if (mode == Mode.None) {
            if (!"theory".equals(localName)) {
                throw new SAXParseException("Root element was not theory", locator);
            }
            this.theoryName = attributes.getValue("name");
            this.implementedTheoryName = attributes.getValue("implements");
            mode = Mode.Theory;
        } else if (mode == Mode.Theory) {
            if (!"nodetype".equals(localName)) {
                throw new SAXParseException("Element Nodetype not found", locator);
            }
            this.vertexName = attributes.getValue("name");
            mode = Mode.Nodetype;
        } else if (mode == Mode.Nodetype) {
            if ("data".equals(localName)) {
                if (attributes.getValue("type") == null) {
                    throw new SAXParseException("Type attribute not found in element Data", locator);
                } else if (attributes.getValue("type").equals("MathExpression")) {
                    this.dataType = VertexType.DataType.MathExpression;
                } else if (attributes.getValue("type").equals("String")) {
                    this.dataType = VertexType.DataType.String;
                } else {
                    this.dataType = VertexType.DataType.None;
                }
            } else if ("visualization".equals(localName)) {
                mode = Mode.Visualization;
            } else if ("mnemonic".equals(localName)) {
                if (attributes.getValue("key") == null) {
                    throw new SAXParseException("Type attribute not found in element data", locator);
                } else if (attributes.getValue("key").length() == 1) {
                    this.mnemonic = attributes.getValue("key");
                } else {
                    throw new SAXParseException("The 'key' attribute in element mnemonic must be one character long.", null);
                }
            } else {
                throw new SAXParseException("Unexpected element found in Nodetype", locator);
            }
        } else if (mode == Mode.Visualization) {
            if ("node".equals(localName)) {
                if (attributes.getValue("svgFile") != null) {
                    //Then the representation is given in an external file
                    try {
                        URL svgUrl = resolveUrl(attributes.getValue("svgFile"));
                        svgdocURI = SVGCache.getSVGUniverse().loadSVG(svgUrl);
                        if (svgdocURI == null) {
                            throw new SAXParseException("Could not load SVG from '" + svgUrl + "'", locator);
                        }
                        data.dependentResources.add(svgUrl);
                    } catch (MalformedURLException e) {
                        throw new SAXParseException("Malformed URL for SVG file", locator);
                    }
                } else {
                    //Then we have an inline declaration, maybe
                    throw new SAXParseException("No svgFile attribute given", locator);
                }
            } else if ("label".equals(localName)) {
                this.labelFill = attributes.getValue("fill");
            } else {
                throw new SAXParseException("Unexpected element found in Visualization", locator);
            }
        } else {
            ++unknownElementDepth;
        }
    }

    @Override
    public void endElement(String namespaceUri, String localName,
            String qualifiedName) throws SAXException {
        if (unknownElementDepth > 0) {
            --unknownElementDepth;
        } else if ((mode == Mode.Visualization) && "visualization".equals(localName)) {
            mode = Mode.Nodetype;
            if (this.vertexName == null) {
                throw new SAXParseException("name attribute of nodetype cannot be null", locator);
            }
            data.vertices.add(new VertexType.GenericVertexType(this.vertexName, this.dataType, this.mnemonic,
                    new SvgVertexVisualizationData(svgdocURI,
                    new Color(Integer.parseInt(this.labelFill, 16)))));
            //Reset the optional mnemonic
            this.mnemonic = null;
        } else if ((mode == Mode.Nodetype) && ("nodetype".equals(localName))) {
            mode = Mode.Theory;
        } else if ((mode == Mode.Theory) && ("theory".equals(localName))) {
            if (this.theoryName == null) {
                data.name = "undef";
                throw new SAXParseException("Could not find attribute 'name' in element <theory>", locator);
            } else {
                data.name = this.theoryName;
            }

            if (this.implementedTheoryName == null) {
                throw new SAXParseException("Could not find attribute 'implements' in element <theory>", locator);
            }
            data.coreName = this.implementedTheoryName;
            mode = Mode.None;
        }
    }
}
