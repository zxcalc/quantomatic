package quanto.core;

import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLFilterImpl;
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
import java.net.URL;
import java.util.ArrayList;

import javax.swing.JOptionPane;

public class TheoryParser {

	private InputSource theoryInputSource;
	private String theoryFilePath;
	private ArrayList<VertexType> theoryVertices;
	private String theoryName;
	private String implementedTheory;

	public TheoryParser(String theoryFilePath){

		this.theoryFilePath = theoryFilePath;
		this.theoryInputSource = new InputSource(theoryFilePath);
		this.theoryVertices = new ArrayList<VertexType>();

		try {
			XMLReader reader = XMLReaderFactory.createXMLReader();

			reader.setContentHandler(new TheoryDataProcessor(this));
			reader.setErrorHandler(new TheoryErrorProcessor());

			reader.parse(this.theoryInputSource);

		} catch(SAXException se) {
			System.out.println("SAX Exception: "+se.getMessage());
		} catch(IOException ioe) {
			System.out.println("IO Exception: "+ioe.getMessage());
		}
	}

	public ArrayList<VertexType> getTheoryVertices() {
		return this.theoryVertices;
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

	public String getTheoryFilePath() {
		return this.theoryFilePath;
	}
}

class TheoryDataProcessor extends DefaultHandler
{
	private TheoryParser theoryParser;
	private VertexType.DataType dataType;
	private URL svgFileURL;
	private String theoryName;
	private String implementedTheoryName;
	private String vertexName;
	private String labelFill;

	public TheoryDataProcessor(TheoryParser theoryParser) {
		super();
		this.theoryParser = theoryParser;
	}

	public void startElement (String namespaceUri, String localName,
			String qualifiedName, Attributes attributes) {
		if(localName.equals("theory")) {
			this.theoryName = attributes.getValue("name");
			this.implementedTheoryName = attributes.getValue("implements");
		} else if (localName.equals("nodetype")) {
			this.vertexName = attributes.getValue("name");
		} else if (localName.equals("data")) {
			if (attributes.getValue("type").equals("MathExpression")) {
				this.dataType = VertexType.DataType.MathExpression;
			} else if (attributes.getValue("type").equals("String")) {
				this.dataType = VertexType.DataType.String;
			} else {
				this.dataType = VertexType.DataType.None;
			}
		}
		else if (localName.equals("label")) {
			this.labelFill = attributes.getValue("fill");
		} else if (localName.equals("node")) {
			/*
			 * A relative path to the svg file is given 
			 */
			//FIXME : will use a parser, when it's implemented
			File tmp = new File(theoryParser.getTheoryFilePath());
			File tmp2 = new File(tmp.getParent() + "/" + attributes.getValue("svgFile"));
			try {
				this.svgFileURL = tmp2.toURL();
			} catch (MalformedURLException e) {
				e.printStackTrace();
			}
		} else if (localName.equals("svg")) {
			/*
			 * Inline declaration of the svg file, use the filter
			 * The inline declaration overwrites the svg repr given in "node"
			 */
			//TODO: Use the filter and give it the SVG constructor
		}
	}

	public void endElement (String namespaceUri, String localName,
			String qualifiedName) {
		if(localName.equals("visualization")) {
			theoryParser.addVertex(new VertexType.GenericVertexType(this.vertexName, this.dataType, 
					new SvgVertexVisualizationData(
							this.svgFileURL,
							new Color(Integer.parseInt(labelFill, 16))
					)));
		} else if (localName.equals("theory")) {
			theoryParser.setTheoryName(this.theoryName);
			theoryParser.setImplementedTheoryName(this.implementedTheoryName);
		}

	}
}

class TheoryErrorProcessor extends DefaultHandler
{

	public TheoryErrorProcessor ()
	{
		super();
	}

	public void error (SAXParseException e) {
		JOptionPane.showMessageDialog(null, e.getMessage(), "Parsing Error", JOptionPane.ERROR_MESSAGE);
	}

	public void fatalError (SAXParseException e) {
		JOptionPane.showMessageDialog(null, e.getMessage(), "Parsing Error", JOptionPane.ERROR_MESSAGE);
	}

	public void warning (SAXParseException e) {
		JOptionPane.showMessageDialog(null, e.getMessage(), "Parsing Warning", JOptionPane.WARNING_MESSAGE);
	}

}

class TheoryDataFilter extends XMLFilterImpl
{

	private String nodeName;
	private Boolean inSVG;
	private Boolean inNodenodeName;

	public TheoryDataFilter(String nodeName) {
		this.nodeName = nodeName;
		this.inSVG = false;
	}

	public void startElement (String namespaceUri, String localName,
			String qualifiedName, Attributes attributes)
	throws SAXException
	{
		if ((localName.equals("nodetype")) && (attributes.getValue("name").equals(nodeName))) {
			inNodenodeName = true;
		} else if (inNodenodeName && (localName.equals("svg"))){
			inSVG = true;
			super.startElement(namespaceUri, localName, qualifiedName, 
					attributes);
		} else if (inSVG) {
			super.startElement(namespaceUri, localName, qualifiedName, 
					attributes);
		} else {
			return;
		}
	}

	public void endElement (String namespaceUri, String localName,
			String qualifiedName)
	throws SAXException
	{
		if (localName.equals("nodetype")) {
			inNodenodeName = true;
		} else if (localName.equals("svg")){
			inSVG = false;
			super.endElement(namespaceUri, localName, qualifiedName);
		} else if (inSVG) {
			super.endElement(namespaceUri, localName, qualifiedName);
		} else {
			return;
		}
	}
}