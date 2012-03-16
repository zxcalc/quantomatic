package quanto.core.xml;

import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;
import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.SAXException;
import org.xml.sax.InputSource;

import quanto.core.data.SvgVertexVisualizationData;
import quanto.core.data.VertexType;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.xml.sax.Locator;
import uk.me.randomguy3.svg.SVGCache;



public class TheoryParser {
	private InputSource theoryInputSource;
    private URL theoryURL;
	private ArrayList<VertexType> theoryVertices = new ArrayList<VertexType>();
	private String theoryName;
	private String implementedTheory;
	private LinkedList<URL> dependentResources = new LinkedList<URL>();

	public TheoryParser(String theoryFilePath) throws SAXException, IOException {
        this(new File(theoryFilePath));
    }
	public TheoryParser(File theoryFile) throws SAXException, IOException {
        this(theoryFile.toURI().toURL());
    }
	public TheoryParser(URL theoryURL) throws SAXException, IOException {

        this.theoryURL = theoryURL;
		this.theoryInputSource = new InputSource(theoryURL.toString());

		XMLReader reader = XMLReaderFactory.createXMLReader();

        TheoryDataHandler handler = new TheoryDataHandler(this);
		reader.setContentHandler(handler);
		reader.setErrorHandler(handler);

		reader.parse(this.theoryInputSource);
	}

	public Collection<VertexType> getTheoryVertices() {
		return Collections.unmodifiableCollection(theoryVertices);
	}

    public Collection<URL> getDependentResources() {
		return Collections.unmodifiableCollection(dependentResources);
    }

    void addDependentResource(URL location) {
        dependentResources.add(location);
    }

	public void addVertex(VertexType vertex) {
		this.theoryVertices.add(vertex);
	}

	public void setTheoryName(String theoryName) {
		this.theoryName = theoryName;
	}

	public String getTheoryName() {
		return this.theoryName;
	}

	public void setImplementedTheoryName(String implementedTheoryName) {
		this.implementedTheory = implementedTheoryName;
	}

	public String getImplementedTheoryName() {
		return this.implementedTheory;
	}

	public URL getTheoryURL() {
		return theoryURL;
	}
}

class TheoryDataHandler extends DefaultHandler
{
    private static Logger logger = Logger.getLogger("quanto.core.xml.TheoryParser");
	private TheoryParser theoryParser;
	private VertexType.DataType dataType;
	private URI svgdocURI;
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
	
	public TheoryDataHandler(TheoryParser theoryParser) {
		super();
		this.theoryParser = theoryParser;
	}

    @Override
    public void setDocumentLocator(Locator locator) {
        this.locator = locator;
    }

	@Override
	public void startElement (String namespaceUri, String localName,
			String qualifiedName, Attributes attributes) throws SAXException {
		if (unknownElementDepth > 0) {
			++unknownElementDepth;
		} else if (mode == Mode.None) {
			if(!"theory".equals(localName)) {
				throw new SAXParseException("Root element was not theory", locator);
			}
			this.theoryName = attributes.getValue("name");
			this.implementedTheoryName = attributes.getValue("implements");
			mode = Mode.Theory;
		} else if(mode == Mode.Theory) {
			if (!"nodetype".equals(localName))
				throw new SAXParseException("Element Nodetype not found", locator);
			this.vertexName = attributes.getValue("name");
			mode = Mode.Nodetype;
		} else if(mode == Mode.Nodetype) {
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
		} else if(mode == Mode.Visualization) {
			if("node".equals(localName)) {
				if (attributes.getValue("svgFile") != null) {
					//Then the representation is given in an external file
					try {
                        URL svgUrl = new URL(theoryParser.getTheoryURL(), attributes.getValue("svgFile"));
                        svgdocURI = SVGCache.getSVGUniverse().loadSVG(svgUrl);
                        if (svgdocURI == null) {
                            throw new SAXParseException("Could not load SVG from '" + svgUrl + "'", null);
                        }
                        theoryParser.addDependentResource(svgUrl);
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
	public void endElement (String namespaceUri, String localName,
			String qualifiedName) throws SAXException {
		if (unknownElementDepth > 0) {
			--unknownElementDepth;
		} else if ((mode == Mode.Visualization) && "visualization".equals(localName)) {
			mode = Mode.Nodetype;
			if (this.vertexName == null) {
				throw new SAXParseException("name attribute of nodetype cannot be null", locator);
            }
            theoryParser.addVertex(new VertexType.GenericVertexType(this.vertexName, this.dataType, this.mnemonic,
                    new SvgVertexVisualizationData(svgdocURI, 
                            new Color(Integer.parseInt(this.labelFill, 16)))));
			//Reset the optional mnemonic
			this.mnemonic = null;
		} else if ((mode == Mode.Nodetype) && ("nodetype".equals(localName))) {
			mode = Mode.Theory;
		} else if ((mode == Mode.Theory) && ("theory".equals(localName))) {
			if (this.theoryName == null) {
				theoryParser.setTheoryName("undef");
				throw new SAXParseException("Could not find attribute 'name' in element <theory>", locator);
			} else {
			theoryParser.setTheoryName(this.theoryName);
			}
			
			if (this.implementedTheoryName == null) {
				theoryParser.setImplementedTheoryName(null);
				throw new SAXParseException("Could not find attribute 'implements' in element <theory>", locator);
			}
			theoryParser.setImplementedTheoryName(this.implementedTheoryName);		
			mode = Mode.None;
		} 
	}

	@Override
	public void error (SAXParseException e) throws SAXException {
		logger.log(Level.FINE, "Recoverable error during parse of theory file", e);
	}

	@Override
	public void fatalError (SAXParseException e) throws SAXException {
		throw e;
	}

	@Override
	public void warning (SAXParseException e) throws SAXException {
		logger.log(Level.FINE, "Warning during parse of theory file", e);
	}
}