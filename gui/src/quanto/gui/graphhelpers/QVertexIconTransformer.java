/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui.graphhelpers;

import java.awt.Dimension;
import java.awt.geom.Dimension2D;
import javax.swing.Icon;
import org.apache.commons.collections15.Transformer;
import quanto.core.data.Vertex;

/**
 *
 * @author alemer
 */
public class QVertexIconTransformer implements Transformer<Vertex, Icon> {
	private static final Dimension2D vertexSize = new Dimension(14, 14);

	public Icon transform(Vertex input) {
		return input.getVertexType().getVisualizationData().getIcon(vertexSize);
	}
	
}
