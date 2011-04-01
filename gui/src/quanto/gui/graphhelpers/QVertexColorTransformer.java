/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui.graphhelpers;

import java.awt.Color;
import java.awt.Paint;
import org.apache.commons.collections15.Transformer;

import quanto.core.Theory;
import quanto.core.data.Vertex;

/**
 * 
 * @author alemer
 */
public class QVertexColorTransformer implements Transformer<Vertex, Paint> {
	private Theory theory;

	public QVertexColorTransformer(Theory theory) {
		this.theory = theory;
	}

	public Paint transform(Vertex v) {
		if (v.isBoundaryVertex()) {
			return Color.lightGray;
		} else {
			return theory.getVertexVisualizationData(v.getVertexType()).fillColour();
		}
	}
}
