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
import quanto.core.RGVertex;
import quanto.gui.TexConstants;

/**
 *
 * @author alemer
 */
public class QVertexAngleLabeler implements VertexLabelRenderer {
        public QVertexAngleLabeler() {
        }

        public <T> Component getVertexLabelRendererComponent(
                JComponent vv, Object value, Font font,
                boolean isSelected, T vertex) {
                if (value instanceof String
                        && vertex instanceof RGVertex) {
                        String val = TexConstants.translate((String) value);

                        RGVertex qv = (RGVertex) vertex;
                        if (qv.getVertexType() == RGVertex.Type.BOUNDARY) {
                                return new JLabel();
                        }

                        JLabel lab = new JLabel(val);
                        Color col = null;
                        if (qv.getVertexType() == RGVertex.Type.RED) {
                                col = new Color(255, 170, 170);
                                lab.setBackground(col);
                                lab.setOpaque(true);
                        }
                        else if (qv.getVertexType() == RGVertex.Type.GREEN) {
                                col = new Color(150, 255, 150);
                                lab.setBackground(col);
                                lab.setOpaque(true);
                        }

                        return lab;
                }
                else {
                        return new JLabel("");
                }
        }
}
