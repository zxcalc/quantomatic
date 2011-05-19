package quanto.core.data;

import java.awt.Color;
import java.awt.Shape;
import javax.swing.Icon;

public interface VertexVisualizationData {
	Shape getShape();
	Color getLabelColour();
	Icon  getIcon();
}
