package quanto.core.data;

import java.awt.Color;
import java.awt.Shape;
import java.awt.geom.Rectangle2D;

public interface VertexVisualizationData {
	Shape getShape(Rectangle2D bounds);
	Color fillColour();
	Color labelColour();
}
