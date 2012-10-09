/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.data;

import java.awt.Color;
import java.awt.Shape;
import java.awt.geom.Rectangle2D;
import javax.swing.Icon;
import org.lindenb.awt.Dimension2D;

/**
 *
 * @author alemer
 */
public class SvgVertexVisualizationData implements VertexVisualizationData {

	Icon cachedIcon;
	Color labelColor = null;
	Rectangle2D lastBoundsForGetShape;
	Shape shape = null;
	SVGDocument svgDoc;

	public SvgVertexVisualizationData(SVGDocument doc, Color labelColor) {
		this.svgDoc = doc;
		this.labelColor = labelColor;
		shape = doc.getElementShape("boundary");
		if (shape == null) {
			Dimension2D dim = doc.getSize();
			shape = new Rectangle2D.Double(0, 0, dim.getWidth(), dim.getHeight());
		}
		cachedIcon = doc.createIcon(24, 24);
	}

	public SVGDocument getSvgDocument() {
		return svgDoc;
	}

	public Shape getShape() {
		return shape;
	}

	public Color getFillColour() {
		return Color.yellow;
	}

	public Color getLabelColour() {
		return labelColor;
	}

	public Icon getIcon() {
		return cachedIcon;
	}
	
}
