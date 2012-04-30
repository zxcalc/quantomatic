/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.xml;

import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;
import java.awt.Color;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.IIOException;
import javax.xml.namespace.NamespaceContext;
import javax.xml.stream.XMLEventWriter;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import org.apache.commons.collections15.map.IdentityMap;
import org.xml.sax.*;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;
import quanto.core.data.SvgVertexVisualizationData;
import quanto.core.data.VertexType;
import quanto.core.data.VertexVisualizationData;
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
    
    public interface ResourceHandler {
        URL getResource(URL original) throws IOException;
    }
    
    public static void write(Data data, File file, ResourceHandler resHand)
        throws IOException
    {
        FileWriter writer = new FileWriter(file);
        try {
            write(data, writer, resHand);
        } finally {
            writer.close();
        }
    }
    
    public static void write(Data data, Writer writer, ResourceHandler resHand)
        throws IOException
    {
        try {
            XMLOutputFactory outFact = XMLOutputFactory.newFactory();
            XMLStreamWriter w = outFact.createXMLStreamWriter(writer);
            w.writeStartDocument();
            w.writeStartElement("theory");
            w.writeAttribute("name", data.name);
            w.writeAttribute("implements", data.coreName);

            for (VertexType vt : data.vertices) {
                w.writeStartElement("nodetype");
                w.writeAttribute("name", vt.getTypeName());

                w.writeStartElement("data");
                w.writeAttribute("type", vt.getDataType().name());
                w.writeEndElement();
                
                if (vt.getMnemonic() != null) {
                    w.writeStartElement("mnemonic");
                    w.writeAttribute("key", vt.getMnemonic().toString());
                    w.writeEndElement();
                }

                w.writeStartElement("visualization");
                VertexVisualizationData visdata = vt.getVisualizationData();
                w.writeStartElement("node");
                URL origURL = data.svgFiles.get(visdata);
                URL newURL = resHand.getResource(origURL);
                w.writeAttribute("svgFile", newURL.toExternalForm());
                w.writeEndElement();
                if (visdata.getLabelColour() != null) {
                    w.writeStartElement("label");
                    Color fill = visdata.getLabelColour();
                    w.writeAttribute("fill", String.format("#%1$02x%2$02x%3$02x",
                            fill.getRed(), fill.getGreen(), fill.getBlue()));
                    w.writeEndElement();
                }
                w.writeEndElement();

                w.writeEndElement();
            }

            w.writeEndElement();
            w.writeEndDocument();
            w.close();
        } catch (XMLStreamException ex) {
            throw new IOException(ex);
        }
        writer.flush();
    }

    public static class Data {
        /** The user-visible theory name */
        public String name;
        /** The core theory that backs this theory */
        public String coreName;
        /** The vertices defined by this theory */
        public ArrayList<VertexType> vertices = new ArrayList<VertexType>();
        /** The external SVG files */
        public Map<VertexVisualizationData, URL> svgFiles =
                new IdentityMap<VertexVisualizationData, URL>();
    }
    private Data data = new Data();
    private URL baseURL;
    private URL locatorBaseURL;
    private Locator locator;
    private int unknownElementDepth = 0;
    private Mode mode = Mode.None;
    
    // vertex
    private String vertexName;
    private VertexType.DataType dataType;
    private Character mnemonic;
    private VertexVisualizationData visdata;

    // vertex vis
    private URI svgdocURI;
    private URL svgURL;
    private Color labelFill;

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

    private SAXException missingAttrEx(String attr, String element) {
        return new SAXParseException(
                "Missing '" + attr + "' attribute in <" + element + "> element",
                locator);
    }

    private SAXException emptyAttrEx(String attr, String element) {
        return new SAXParseException(
                "Attribute '" + attr + "' cannot be empty in <" + element + "> element",
                locator);
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

            data.name = attributes.getValue("name");
            if (data.name == null) {
                throw missingAttrEx("name", "theory");
            } else if (data.name.isEmpty()) {
                throw emptyAttrEx("name", "theory");
            }

            data.coreName = attributes.getValue("implements");
            if (data.coreName == null) {
                throw missingAttrEx("implements", "theory");
            } else if (data.coreName.isEmpty()) {
                throw emptyAttrEx("implements", "theory");
            }

            mode = Mode.Theory;
        } else if (mode == Mode.Theory) {
            if ("nodetype".equals(localName)) {
                this.vertexName = attributes.getValue("name");
                if (vertexName == null) {
                    throw missingAttrEx("name", "nodetype");
                } else if (vertexName.isEmpty()) {
                    throw emptyAttrEx("name", "nodetype");
                }
                mode = Mode.Nodetype;
            } else {
                ++unknownElementDepth;
            }
        } else if (mode == Mode.Nodetype) {
            if ("data".equals(localName)) {
                String dtype = attributes.getValue("type");
                if (dtype == null) {
                    throw missingAttrEx("type", "element");
                } else if (dtype.isEmpty()) {
                    throw emptyAttrEx("type", "element");
                }
                try {
                    this.dataType = VertexType.DataType.valueOf(dtype);
                } catch (IllegalArgumentException ex) {
                    throw new SAXParseException("Unknown data type '" + dtype + "'", locator);
                }
            } else if ("visualization".equals(localName)) {
                mode = Mode.Visualization;
            } else if ("mnemonic".equals(localName)) {
                String key = attributes.getValue("key");
                if (key == null) {
                    throw missingAttrEx("key", "mnemonic");
                } else if (key.isEmpty()) {
                    throw emptyAttrEx("key", "mnemonic");
                } else if (key.length() == 1) {
                    this.mnemonic = key.charAt(0);
                } else {
                    throw new SAXParseException("The 'key' attribute in element mnemonic must be one character long.", locator);
                }
            } else {
                ++unknownElementDepth;
            }
        } else if (mode == Mode.Visualization) {
            if ("node".equals(localName)) {
                String svgFile = attributes.getValue("svgFile");
                if (svgFile != null && !svgFile.isEmpty()) {
                    //Then the representation is given in an external file
                    try {
                        svgURL = resolveUrl(svgFile);
                        svgdocURI = SVGCache.getSVGUniverse().loadSVG(svgURL);
                        if (svgdocURI == null) {
                            throw new SAXParseException("Could not load SVG from '" + svgURL + "'", locator);
                        }
                    } catch (MalformedURLException e) {
                        throw new SAXParseException("Malformed URL for SVG file", locator);
                    }
                } else {
                    //Then we have an inline declaration, maybe
                    throw new SAXParseException("No svgFile attribute given", locator);
                }
            } else if ("label".equals(localName)) {
                String fillSpec = attributes.getValue("fill");
                if (fillSpec != null && !fillSpec.isEmpty()) {
                    this.labelFill = uk.me.randomguy3.svg.xml.ColorTable.parseColor(fillSpec);
                    if (this.labelFill == null) {
                        throw new SAXParseException(
                                "Invalid color '" +  fillSpec + 
                                "' in 'fill' attribute of 'label' element",
                                locator);
                    }
                }
            } else {
                ++unknownElementDepth;
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
            if (svgdocURI == null) {
                throw new SAXParseException("<visualization> element missing SVG data", locator);
            }
            // labelFill may be omitted
            visdata = new SvgVertexVisualizationData(svgdocURI, labelFill);
            data.svgFiles.put(visdata, svgURL);

            svgdocURI = null;
            svgURL = null;
            labelFill = null;

            mode = Mode.Nodetype;
        } else if ((mode == Mode.Nodetype) && ("nodetype".equals(localName))) {
            if (visdata == null) {
                throw new SAXParseException("<nodetype> element has no <visualization> element", locator);
            }
            if (dataType == null) {
                throw new SAXParseException("<nodetype> element has no <data> element", locator);
            }
            VertexType vt = new VertexType.GenericVertexType(
                    vertexName, dataType, mnemonic, visdata);
            data.vertices.add(vt);

            mnemonic = null;
            visdata = null;
            dataType = null;
            vertexName = null;

            mode = Mode.Theory;
        } else if ((mode == Mode.Theory) && ("theory".equals(localName))) {
            mode = Mode.None;
        }
    }
}
