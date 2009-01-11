package quanto.gui;

import java.awt.Dimension;

import edu.uci.ics.jung.contrib.DotLayout;

public class QuantoLayout extends DotLayout<QVertex,QEdge> {
	
	public QuantoLayout(QuantoGraph graph, Dimension size) {
		super(graph, size);
	}

	public QuantoLayout(QuantoGraph graph) {
		super(graph);
	}

}
