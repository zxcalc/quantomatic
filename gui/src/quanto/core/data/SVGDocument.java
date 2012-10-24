/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.data;

import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.lindenb.awt.Dimension2D;
import org.lindenb.lang.InvalidXMLException;
import org.lindenb.svg.SVGRenderer;
import org.lindenb.svg.SVGUtils;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

/**
 *
 * @author alex
 */
public class SVGDocument {

	private Document document;
	private SVGRenderer renderer = new SVGRenderer();

	private void testParse() throws InvalidXMLException {
		BufferedImage img = new BufferedImage(24, 24, BufferedImage.TYPE_INT_ARGB);
		renderer.paint(img.createGraphics(), document,
				new Rectangle2D.Double(0, 0, img.getWidth(), img.getHeight()));
	}

	public SVGDocument(Document document) throws InvalidXMLException {
		this.document = document;
		testParse();
	}

	private static DocumentBuilder createDocumentBuilder() {
		try {
			DocumentBuilderFactory domFactory = DocumentBuilderFactory.newInstance();
			domFactory.setCoalescing(true);
			domFactory.setExpandEntityReferences(true);
			domFactory.setIgnoringComments(true);
			domFactory.setNamespaceAware(true);
			domFactory.setValidating(false);
			// we turn off external DTD loading, since that slows things right down
			domFactory.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false);
			domFactory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
			return domFactory.newDocumentBuilder();
		} catch (ParserConfigurationException ex) {
			throw new Error(ex);
		}

	}

	public SVGDocument(File file) throws SAXException, IOException, InvalidXMLException {
		DocumentBuilder domBuilder = createDocumentBuilder();
		document = domBuilder.parse(file);
		testParse();
	}

	public SVGDocument(URL url) throws SAXException, IOException, InvalidXMLException {
		DocumentBuilder domBuilder = createDocumentBuilder();
		InputStream is = url.openStream();
		try {
			document = domBuilder.parse(is, url.toExternalForm());
		} finally {
			is.close();
		}
		testParse();
	}

	public SVGDocument(String uri) throws SAXException, IOException, InvalidXMLException {
		DocumentBuilder domBuilder = createDocumentBuilder();
		document = domBuilder.parse(uri);
		testParse();
	}

	public Icon createIcon() {
		Dimension2D size = getSize();
		return createIcon((int)Math.ceil(size.getWidth()),
				          (int)Math.ceil(size.getHeight()));
	}

	public Icon createIcon(int width, int height) {
		try {
			BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
			Graphics2D g = img.createGraphics();
			g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			renderer.paint(g, document,
					new Rectangle2D.Double(0, 0, width, height));
			return new ImageIcon(img);
		} catch (InvalidXMLException err) {
			// this should already have been caught at constructor time
			throw new IllegalStateException(err);
		}
	}

	public Dimension2D getSize() {
		try {
			Element svgRoot = document.getDocumentElement();
			Attr viewBoxAttr = svgRoot.getAttributeNode("viewBox");
			if (viewBoxAttr != null) {
				String tokens[] = viewBoxAttr.getValue().trim().split("[ \t\n]+");
				Rectangle2D viewBox = new Rectangle2D.Double(
						Double.parseDouble(tokens[0]),
						Double.parseDouble(tokens[1]),
						Double.parseDouble(tokens[2]),
						Double.parseDouble(tokens[3]));
				return new Dimension2D.Double(viewBox.getWidth(), viewBox.getHeight());
			}
			return SVGUtils.getSize(svgRoot);
		} catch (InvalidXMLException ex) {
			throw new IllegalStateException(ex);
		}
	}
	
	public Rectangle2D getBounds() {
		try {
			Element svgRoot = document.getDocumentElement();
			Attr viewBoxAttr = svgRoot.getAttributeNode("viewBox");
			if (viewBoxAttr != null) {
				String tokens[] = viewBoxAttr.getValue().trim().split("[ \t\n]+");
				return new Rectangle2D.Double(
						Double.parseDouble(tokens[0]),
						Double.parseDouble(tokens[1]),
						Double.parseDouble(tokens[2]),
						Double.parseDouble(tokens[3]));
			} else {
				Dimension2D dim = SVGUtils.getSize(svgRoot);
				return new Rectangle2D.Double(
						0,
						0,
						dim.getWidth(),
						dim.getHeight());
			}
		} catch (InvalidXMLException ex) {
			throw new IllegalStateException(ex);
		}
	}

	/**
	 * Get the shape of an element
	 * 
	 * Not all elements are supported.  In particular, this will return null
	 * if the element is a text element.
	 *
	 * @param elementID  the XML ID of the element
	 * @return the element shape, or null if there was no such element or it
	 *         did not have a shape
	 */
	public Shape getElementShape(String elementID) {
		Element e = document.getElementById(elementID);
		if (e == null) {
			return null;
		}

		String shapeName = e.getLocalName();
		Shape shape = null;
		if (shapeName.equals("path")) {
			Attr d = e.getAttributeNode("d");
			if (d != null) {
				shape = SVGUtils.pathToShape(d.getValue());
			}
		} else if (shapeName.equals("polyline")) {
			Attr points = e.getAttributeNode("points");
			if (points != null) {
				shape = SVGUtils.polylineToShape(points.getValue());
			}
		} else if (shapeName.equals("polygon")) {
			Attr points = e.getAttributeNode("points");
			if (points != null) {
				shape = SVGUtils.polygonToShape(points.getValue());
			}
		} else if (shapeName.equals("rect")) {

			Attr x = e.getAttributeNode("x");
			Attr y = e.getAttributeNode("y");
			Attr w = e.getAttributeNode("width");
			Attr h = e.getAttributeNode("height");
			if (x != null && y != null && w != null && h != null) {
				shape = new Rectangle2D.Double(
						Double.parseDouble(x.getValue()),
						Double.parseDouble(y.getValue()),
						Double.parseDouble(w.getValue()),
						Double.parseDouble(h.getValue()));
			}
		} else if (shapeName.equals("line")) {
			Attr x1 = e.getAttributeNode("x1");
			Attr y1 = e.getAttributeNode("y1");
			Attr x2 = e.getAttributeNode("x2");
			Attr y2 = e.getAttributeNode("y2");
			if (x1 != null && y1 != null && x2 != null && y2 != null) {
				shape = new Line2D.Double(
						Double.parseDouble(x1.getValue()),
						Double.parseDouble(y1.getValue()),
						Double.parseDouble(x2.getValue()),
						Double.parseDouble(y2.getValue()));
			}
		} else if (shapeName.equals("circle")) {
			Attr cx = e.getAttributeNode("cx");
			Attr cy = e.getAttributeNode("cy");
			Attr r = e.getAttributeNode("r");
			if (cx != null && cy != null && r != null) {
				double radius = Double.parseDouble(r.getValue());
				shape = new Ellipse2D.Double(
						Double.parseDouble(cx.getValue()) - radius,
						Double.parseDouble(cy.getValue()) - radius,
						radius * 2,
						radius * 2);
			}
		} else if (shapeName.equals("ellipse")) {
			Attr cx = e.getAttributeNode("cx");
			Attr cy = e.getAttributeNode("cy");
			Attr rx = e.getAttributeNode("rx");
			Attr ry = e.getAttributeNode("ry");
			if (cx != null && cy != null && rx != null && ry != null) {
				double radiusx = Double.parseDouble(rx.getValue());
				double radiusy = Double.parseDouble(ry.getValue());
				shape = new Ellipse2D.Double(
						Double.parseDouble(cx.getValue()) - radiusx,
						Double.parseDouble(cy.getValue()) - radiusy,
						radiusx * 2,
						radiusy * 2);
			}
		}
		if (shape == null) {
			return null;
		}

		AffineTransform transform = new AffineTransform();
		applyAllTransforms(e, transform);
		return transform.createTransformedShape(shape);
	}
	
	private void applyAllTransforms(Element e, AffineTransform a) {
		Node parent = e.getParentNode();
		if (parent == null || parent.getNodeType() != Node.ELEMENT_NODE)
			return;
		applyAllTransforms((Element)e.getParentNode(), a);
		if (e.hasAttributes()) {
			NamedNodeMap atts = e.getAttributes();
			for (int i = 0; i < atts.getLength(); ++i) {

				Attr att = Attr.class.cast(atts.item(i));
				if (att.getNamespaceURI() != null) {
					continue;
				}
				String s = att.getName();
				String value = att.getValue();
				if (s.equals("style")) {
					for (String styles : value.split("[;]+")) {
						int j = styles.indexOf(':');
						if (j != -1) {
							if(styles.substring(0, j).trim().equals("transform"))
								a.concatenate(SVGUtils.svgToaffineTransform(styles.substring(j + 1).trim()));
						}
					}

				} else {
					if(s.equals("transform"))
						a.concatenate(SVGUtils.svgToaffineTransform(att.getValue()));
				}
			}
		}
	}

	public Document getDocument() {
		return document;
	}
}
