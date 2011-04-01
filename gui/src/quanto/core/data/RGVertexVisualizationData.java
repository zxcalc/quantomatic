package quanto.core.data;

import java.awt.Color;
import java.awt.Shape;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;

public class RGVertexVisualizationData implements VertexVisualizationData {
	private Color color;
	
	public RGVertexVisualizationData(Color color) {
		this.color = color;
	}
	
	@Override
	public Color fillColour() {
		return color;
	}
	
	@Override
	public Shape getShape(Rectangle2D bounds) {
		return new Ellipse2D.Double(bounds.getX(), bounds.getY(), bounds.getWidth(), bounds.getHeight());
	}

	@Override
	public Color labelColour() {
		// FIXME: hack
		if (color == Color.red) {
			return new Color(255, 170, 170);
		} else if (color == Color.green) {
			return new Color(150, 255, 150);
		}
		return null;
	}
}
