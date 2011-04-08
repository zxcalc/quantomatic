/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui.graphhelpers;

import java.awt.Shape;
import java.awt.geom.Rectangle2D;
import javax.swing.JLabel;
import org.apache.commons.collections15.Transformer;

import quanto.core.data.Vertex;

/**
 * 
 * @author alemer
 */
public class QVertexShapeTransformer implements Transformer<Vertex, Shape> {
	private static final Rectangle2D vertexBounds = new Rectangle2D.Double(-7, -7, 14, 14);

	public QVertexShapeTransformer() {
	}

	public Shape transform(Vertex v) {
		if (v.isBoundaryVertex()) {
			String text = v.getCoreName();
			double width = new JLabel(text).getPreferredSize().getWidth();
			width = Math.max(width, 14);
			return new Rectangle2D.Double(-(width / 2), -7, width, 14);
		} else {
			return v.getVertexType().getVisualizationData().getShape(vertexBounds);
		}
	}
}
