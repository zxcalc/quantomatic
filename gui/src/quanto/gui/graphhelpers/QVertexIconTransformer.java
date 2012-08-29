package quanto.gui.graphhelpers;

import javax.swing.Icon;
import org.apache.commons.collections15.Transformer;
import quanto.core.data.Vertex;

/**
 *
 * @author alemer
 */
public class QVertexIconTransformer implements Transformer<Vertex, Icon> {

	public Icon transform(Vertex input) {
		if (input.isBoundaryVertex()) {
			return null;
		} else {
			return input.getVertexType().getVisualizationData().getIcon();
		}
	}
}
