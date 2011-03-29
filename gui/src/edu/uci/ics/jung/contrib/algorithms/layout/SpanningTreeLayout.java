package edu.uci.ics.jung.contrib.algorithms.layout;

import java.awt.geom.Point2D;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.AbstractLayout;
import edu.uci.ics.jung.algorithms.layout.StaticLayout;
import edu.uci.ics.jung.algorithms.layout.TreeLayout;
import edu.uci.ics.jung.algorithms.shortestpath.MinimumSpanningForest2;
import edu.uci.ics.jung.graph.*;

public class SpanningTreeLayout<V,E> extends AbstractLayout<V,E> {
	protected StaticLayout<V, E> layout;
	
	public SpanningTreeLayout(Graph<V,E> graph) {
		super(graph);
	}

	public void initialize() {
		Transformer<E,Double> weights = new Transformer<E,Double>() {
			public Double transform(E input) {
				return 1.0;
			}
		};
		
		MinimumSpanningForest2<V,E> prim = 
        	new MinimumSpanningForest2<V,E>(getGraph(),
        		new DelegateForest<V,E>(),
        		DelegateTree.<V,E>getFactory(),
        		weights);
        
        Forest<V,E> tree = prim.getForest();
        layout = new StaticLayout<V,E>(getGraph(),new TreeLayout<V,E>(tree));
	}

	public void reset() {}
	
	public Point2D transform(V v) {
		return layout.transform(v);
	}

}
