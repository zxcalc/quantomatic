/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui.graphhelpers;

import org.apache.commons.collections15.Transformer;
import quanto.core.RGVertex;

/**
 *
 * @author alemer
 */
public class QVertexLabelTransformer implements Transformer<RGVertex, String>
{
        public String transform(RGVertex v) {
                if (v.getVertexType() == RGVertex.Type.HADAMARD) {
                        return null;
                } else {
                        return v.getLabel();
                }
        }
}
