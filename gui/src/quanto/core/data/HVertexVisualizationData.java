package quanto.core.data;

import java.awt.Color;
import java.awt.Shape;
import java.awt.geom.Rectangle2D;

public class HVertexVisualizationData implements VertexVisualizationData {

	public Shape getShape(Rectangle2D bounds) {
		return bounds;
	}

	public Color fillColour() {
		return Color.yellow;
	}

	public Color labelColour() {
		return null;
	}

	public String fillText() {
		return "H";
	}
}
