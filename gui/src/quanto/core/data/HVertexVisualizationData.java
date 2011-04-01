package quanto.core.data;

import java.awt.Color;
import java.awt.Shape;
import java.awt.geom.Rectangle2D;

public class HVertexVisualizationData implements VertexVisualizationData {

	@Override
	public Shape getShape(Rectangle2D bounds) {
		return bounds;
	}

	@Override
	public Color fillColour() {
		return Color.yellow;
	}

	@Override
	public Color labelColour() {
		return null;
	}
}
