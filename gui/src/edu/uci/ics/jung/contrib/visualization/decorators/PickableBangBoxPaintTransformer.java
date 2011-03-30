/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.uci.ics.jung.contrib.visualization.decorators;

import edu.uci.ics.jung.visualization.picking.PickedInfo;
import java.awt.Paint;
import org.apache.commons.collections15.Transformer;

/**
 *
 * @author alemer
 */
public class PickableBangBoxPaintTransformer<B> implements Transformer<B, Paint> {

	protected PickedInfo<B> pi;
	protected Paint draw_paint;
	protected Paint picked_paint;

	public PickableBangBoxPaintTransformer(PickedInfo<B> pi, Paint draw_paint, Paint picked_paint) {
		if (pi == null) {
			throw new IllegalArgumentException("PickedInfo instance must be non-null");
		}

		this.pi = pi;
		this.draw_paint = draw_paint;
		this.picked_paint = picked_paint;
	}

	public Paint transform(B b) {
		if (pi.isPicked(b)) {
			return picked_paint;
		}
		else {
			return draw_paint;
		}
	}
}
