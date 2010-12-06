/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui.graphhelpers;

import java.awt.Color;
import java.awt.Paint;
import org.apache.commons.collections15.Transformer;
import quanto.gui.QVertex;

/**
 *
 * @author alemer
 */
public class QVertexColorTransformer implements Transformer<QVertex, Paint>
{
        public Paint transform(QVertex v) {
                switch (v.getVertexType()) {
                        case RED:
                                return Color.red;
                        case GREEN:
                                return Color.green;
                        case HADAMARD:
                                return Color.yellow;
                        default:
                                return Color.lightGray;
                }
        }
}
