package edu.uci.ics.jung.contrib.visualization;

import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.RenderContext;
import java.awt.geom.Point2D;
import org.apache.commons.collections15.Transformer;

public class LayerTransformer implements Transformer<Point2D, Point2D> {

	private RenderContext rc;
	private Layer layer;

	public LayerTransformer(RenderContext rc, Layer layer) {
		this.rc = rc;
		this.layer = layer;
	}

	public Point2D transform(Point2D i) {
		return rc.getMultiLayerTransformer().transform(layer, i);
	}
}
