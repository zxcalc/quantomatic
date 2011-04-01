/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui.graphhelpers;

import edu.uci.ics.jung.visualization.renderers.VertexLabelRenderer;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import javax.swing.JComponent;
import javax.swing.JLabel;

import quanto.core.data.Vertex;

/**
 * 
 * @author alemer
 */
public class QVertexAngleLabeler implements VertexLabelRenderer {
	public QVertexAngleLabeler() {
	}

	public <T> Component getVertexLabelRendererComponent(JComponent vv,
			Object value, Font font, boolean isSelected, T vertex) {

		if (value != null && vertex instanceof Vertex) {
			Vertex v = (Vertex) vertex;
			if (v.isBoundaryVertex()) {
				return new JLabel();
			}
			JLabel lab = new JLabel(value.toString());
			Color colour = v.getVertexType().getVisualizationData().labelColour();
			if (colour != null) {
				lab.setBackground(colour);
				lab.setOpaque(true);
			}
			return lab;
		} else {
			return new JLabel("");
		}
	}
}
