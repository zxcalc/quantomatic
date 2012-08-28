package quanto.gui.graphhelpers;

import java.awt.Color;
import java.awt.Paint;
import org.apache.commons.collections15.Transformer;

import quanto.core.data.Vertex;

/**
 * 
 * @author alemer
 */
public class QVertexColorTransformer implements Transformer<Vertex, Paint> {

	public QVertexColorTransformer() {
	}

	public Paint transform(Vertex v) {
		if (v.isBoundaryVertex()) {
			return Color.lightGray;
		} else {
			return Color.blue;
		}
	}
}
