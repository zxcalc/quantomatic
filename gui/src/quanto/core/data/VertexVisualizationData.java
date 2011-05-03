package quanto.core.data;

import java.awt.Color;
import java.awt.Shape;
import java.awt.geom.Dimension2D;
import java.awt.geom.Rectangle2D;
import javax.swing.Icon;

public interface VertexVisualizationData {
	Shape getShape(Rectangle2D bounds);
	Color getLabelColour();
	Icon  getIcon(Dimension2D size);
	Color getFillColour();
}
