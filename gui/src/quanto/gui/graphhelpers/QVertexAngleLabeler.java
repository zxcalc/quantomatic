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
	private JLabel dummyLabel = new JLabel();
	private JLabel realLabel = new JLabel();

	public QVertexAngleLabeler() {
		realLabel.setOpaque(true);
	}

	public <T> Component getVertexLabelRendererComponent(JComponent vv,
			Object value, Font font, boolean isSelected, T vertex) {

		if (value == null) {
			return dummyLabel;
		} else {
			realLabel.setBackground(Color.white);
			if (vertex instanceof Vertex) {
				Vertex v = (Vertex) vertex;
				// we render boundary labels differently
				if (v.isBoundaryVertex()) {
					return dummyLabel;
				}
				Color colour = v.getVertexType().getVisualizationData().getLabelColour();
				if (colour != null) {
					realLabel.setBackground(colour);
				}
			}
			realLabel.setText(value.toString());
			return realLabel;
		}
	}
}
