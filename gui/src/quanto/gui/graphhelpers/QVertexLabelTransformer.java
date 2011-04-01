/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui.graphhelpers;

import org.apache.commons.collections15.Transformer;

import quanto.core.Theory;
import quanto.core.Theory.DataType;
import quanto.core.data.Vertex;
import quanto.gui.TexConstants;

/**
 * 
 * @author alemer
 */
public class QVertexLabelTransformer implements Transformer<Vertex, String> {
	private Theory theory;
	
	public QVertexLabelTransformer(Theory theory) {
		this.theory = theory;
	}
	
	public String transform(Vertex v) {
		if (v.isBoundaryVertex()) {
			// FIXME: what to do with boundary vertices?
			return "0";
		} else if (theory.vertexHasData(v.getVertexType())) {
			if (theory.vertexDataType(v.getVertexType()) == DataType.MathExpression)
				return TexConstants.translate(v.getData().getStringValue());
			else
				return v.getData().getStringValue();
		} else {
			return null;
		}
	}
}
