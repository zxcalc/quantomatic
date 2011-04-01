/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui.graphhelpers;

import org.apache.commons.collections15.Transformer;

import quanto.core.data.Vertex;
import quanto.core.data.VertexType;
import quanto.gui.TexConstants;

/**
 * 
 * @author alemer
 */
public class QVertexLabelTransformer implements Transformer<Vertex, String> {
	public QVertexLabelTransformer() {
	}
	
	public String transform(Vertex v) {
		if (v.isBoundaryVertex()) {
			// FIXME: what to do with boundary vertices?
			return "0";
		} else if (v.getVertexType().hasData()) {
			if (v.getVertexType().getDataType() == VertexType.DataType.MathExpression)
				return TexConstants.translate(v.getData().getStringValue());
			else
				return v.getData().getStringValue();
		} else {
			return null;
		}
	}
}
