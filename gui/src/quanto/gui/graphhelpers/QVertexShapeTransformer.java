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
import quanto.gui.QVertex;

/**
 *
 * @author alemer
 */
public class QVertexShapeTransformer implements Transformer<QVertex, Shape>
{
        public Shape transform(QVertex v) {
                if (v.getVertexType() == QVertex.Type.BOUNDARY) {
                        String text = v.getLabel();
                        double width =
                                new JLabel(text).getPreferredSize().getWidth();
                        width = Math.max(width, 14);
                        return new Rectangle2D.Double(-(width / 2), -7, width, 14);
                }
                else if (v.getVertexType() == QVertex.Type.HADAMARD) {
                        return new Rectangle2D.Double(-7, -7, 14, 14);
                }
                else {
                        return new Ellipse2D.Double(-7, -7, 14, 14);
                }
        }
}
