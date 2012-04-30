/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.data;

import uk.me.randomguy3.svg.SVGDiagram;
import uk.me.randomguy3.svg.ShapeElement;
import uk.me.randomguy3.svg.components.SVGIcon;
import java.awt.Color;
import java.awt.Shape;
import java.awt.geom.Rectangle2D;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URI;
import java.net.URL;
import javax.swing.Icon;

/**
 *
 * @author alemer
 */
public class SvgVertexVisualizationData implements VertexVisualizationData {

	SVGIcon cachedIcon = new SVGIcon();
	Color labelColor = null;
	Rectangle2D lastBoundsForGetShape;
	Shape shape = null;
	ShapeElement boundsElement;
	Rectangle2D diagramBounds;

	public SvgVertexVisualizationData(URI svgdocURI, Color labelColor) {
		this.labelColor = labelColor;
		cachedIcon.setSvgURI(svgdocURI);
		cachedIcon.setScaleToFit(true);
		cachedIcon.setAntiAlias(true);
		SVGDiagram dia = cachedIcon.getSvgUniverse().getDiagram(svgdocURI);
		boundsElement = (ShapeElement)dia.getElement("boundary");
		diagramBounds = dia.getViewRect();
	}

	public boolean isAntiAliasingOn()
	{
		return cachedIcon.getAntiAlias();
	}

	public void setAntiAliasingOn(boolean on)
	{
		cachedIcon.setAntiAlias(on);
	}

	public Shape getShape() {
		if (boundsElement == null) 
			return diagramBounds;
		return boundsElement.getShape();
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
