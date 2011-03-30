/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.visualization.decorators;

import edu.uci.ics.jung.visualization.picking.PickedInfo;
import java.awt.Stroke;
import org.apache.commons.collections15.Transformer;

/**
 *
 * @author alemer
 */
public class PickableElementStrokeTransformer<E> implements Transformer<E, Stroke>
{
	protected PickedInfo<E> pi;
	protected Stroke normal_stroke;
	protected Stroke picked_stroke;

	public PickableElementStrokeTransformer(PickedInfo<E> pi, Stroke normal_stroke, Stroke picked_stroke) {
		if (pi == null) {
			throw new IllegalArgumentException("PickedInfo instance must be non-null");
		}

		this.pi = pi;
		this.normal_stroke = normal_stroke;
		this.picked_stroke = picked_stroke;
	}

	public Stroke transform(E b) {
		if (pi.isPicked(b)) {
			return picked_stroke;
		}
		else {
			return normal_stroke;
		}
	}
}
