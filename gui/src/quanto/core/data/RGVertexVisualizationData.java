package quanto.core.data;

import java.awt.Color;
import java.awt.Shape;
import java.awt.geom.Dimension2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import javax.swing.Icon;

public class RGVertexVisualizationData implements VertexVisualizationData {
	private Color color;

	public RGVertexVisualizationData(Color color) {
		this.color = color;
	}

	public Icon getIcon(Dimension2D size) {
		return null;
	}

	public Color getFillColour() {
		return color;
	}

	public Shape getShape(Rectangle2D bounds) {
		return new Ellipse2D.Double(bounds.getX(), bounds.getY(), bounds.getWidth(), bounds.getHeight());
	}

	public Color getLabelColour() {
		// FIXME: hack
		if (color == Color.red) {
			return new Color(255, 170, 170);
		} else if (color == Color.green) {
			return new Color(150, 255, 150);
		}
		return null;
	}
}
