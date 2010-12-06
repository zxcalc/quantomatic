/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui.graphhelpers;

import org.apache.commons.collections15.Transformer;
import quanto.gui.QVertex;

/**
 *
 * @author alemer
 */
public class QVertexLabelTransformer implements Transformer<QVertex, String>
{
        public String transform(QVertex v) {
                if (v.getVertexType() == QVertex.Type.HADAMARD) {
                        return null;
                } else {
                        return v.getLabel();
                }
        }
}
