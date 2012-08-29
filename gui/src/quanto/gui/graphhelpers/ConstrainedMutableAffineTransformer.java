package quanto.gui.graphhelpers;

import edu.uci.ics.jung.visualization.transform.MutableAffineTransformer;
import java.awt.geom.Point2D;

/**
 * Makes sure the origin never moves in a positive direction on either axis
 *
 * @author alemer
 */
public class ConstrainedMutableAffineTransformer extends MutableAffineTransformer {

	private void checkAndAdjust() {
		Point2D p = transform(new Point2D.Double(0.0, 0.0));
		double dx = (p.getX() > 0) ? -p.getX() : 0.0;
		double dy = (p.getY() > 0) ? -p.getY() : 0.0;
		if (dx != 0.0 || dy != 0.0) {
			inverse = null;
			transform.translate(dx, dy);
		}
	}

	@Override
	public void fireStateChanged() {
		checkAndAdjust();
		super.fireStateChanged();
	}
}
