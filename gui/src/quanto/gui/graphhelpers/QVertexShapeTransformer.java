/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui.graphhelpers;

import java.awt.Shape;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import javax.swing.JLabel;
import org.apache.commons.collections15.Transformer;

import quanto.core.Theory;
import quanto.core.data.Vertex;

/**
 * 
 * @author alemer
 */
public class QVertexShapeTransformer implements Transformer<Vertex, Shape> {
	private Theory theory;
	private static final Rectangle2D vertexBounds = new Rectangle2D.Double(-7, -7, 14, 14);

	public QVertexShapeTransformer(Theory theory) {
		this.theory = theory;
	}

	public Shape transform(Vertex v) {
		if (v.isBoundaryVertex()) {
			String text = v.getLabel();
			double width = new JLabel(text).getPreferredSize().getWidth();
			width = Math.max(width, 14);
			return new Rectangle2D.Double(-(width / 2), -7, width, 14);
		} else {
			return theory.getVertexVisualizationData(v.getVertexType()).getShape(vertexBounds);
		}
	}
}
