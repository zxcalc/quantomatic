/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.algorithms.layout;

import edu.uci.ics.jung.contrib.graph.DirectedBangBoxGraph;
import edu.uci.ics.jung.visualization.layout.ObservableCachingLayout;
import java.awt.geom.Rectangle2D;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.collections15.Transformer;
import org.apache.commons.collections15.functors.ChainedTransformer;
import org.apache.commons.collections15.functors.CloneTransformer;
import org.apache.commons.collections15.map.LazyMap;

/**
 *
 * @author alex
 */
public class ObservableCachingBangBoxLayout<V,E,B>
	extends ObservableCachingLayout<V, E>
	implements BangBoxLayout<V, E, B>
{
	protected Map<B,Rectangle2D> bbMap;
	private Transformer<B, Rectangle2D> bbTransformer = new Transformer<B, Rectangle2D>() {
		@SuppressWarnings("unchecked")
		public Rectangle2D transform(B i) {
			return ((BangBoxLayout<V, E, B>)delegate).transformBangBox(i);
		}
	};

	public ObservableCachingBangBoxLayout(BangBoxLayout<V, E, B> delegate) {
		super(delegate);
		this.bbMap = LazyMap.<B,Rectangle2D>decorate(
			new HashMap<B,Rectangle2D>(),
			new ChainedTransformer<B, Rectangle2D>(
				new Transformer[] {
					bbTransformer,
					CloneTransformer.<Rectangle2D>getInstance()
				}));

	}

	@Override
	@SuppressWarnings("unchecked")
	public DirectedBangBoxGraph<V, E, B> getGraph() {
		return (DirectedBangBoxGraph<V, E, B>)super.getGraph();
	}

	public Rectangle2D transformBangBox(B bb) {
		return bbMap.get(bb);
	}
}
