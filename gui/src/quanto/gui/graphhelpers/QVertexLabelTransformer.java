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
			return v.getCoreName();
		} else if (v.getVertexType().hasData()) {
			if (v.getVertexType().getDataType() == VertexType.DataType.MathExpression) {
				return TexConstants.translate(v.getData().getStringValue());
			} else {
				return v.getData().getStringValue();
			}
		} else {
			return null;
		}
	}
}
