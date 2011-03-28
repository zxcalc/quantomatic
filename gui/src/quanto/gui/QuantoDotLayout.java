/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui;

import edu.uci.ics.jung.contrib.AbstractBangBoxDotLayout;
import quanto.core.QBangBox;
import quanto.core.QEdge;
import quanto.core.QGraph;
import quanto.core.QVertex;

/**
 *
 * @author alex
 */
public class QuantoDotLayout extends AbstractBangBoxDotLayout<QVertex, QEdge, QBangBox> {
	public static final double PADDING = 20.0;

	public QuantoDotLayout(QGraph graph) {
		super(graph, PADDING, PADDING);
	}

	@Override
	protected String getBangBoxDotKey(QBangBox bangbox) {
		return bangbox.getCoreName();
	}

	@Override
	protected String getVertexDotKey(QVertex vertex) {
		return vertex.getCoreName();
	}

}
