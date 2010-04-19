package quanto.gui;

import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import edu.uci.ics.jung.visualization.control.AbstractGraphMousePlugin;

public class BangBoxGraphMousePlugin extends AbstractGraphMousePlugin
implements MouseListener, MouseMotionListener {
	private InteractiveGraphView igv;
	public BangBoxGraphMousePlugin(InteractiveGraphView igv) {
		super(0);
		this.igv = igv;
	}

	public void mouseClicked(MouseEvent e) {
		QuantoGraph gr = igv.getGraph();
		Point2D pt = igv.getVisualization().getRenderContext()
			.getMultiLayerTransformer().inverseTransform(e.getPoint());
		
		
		if ((e.getModifiers() & InputEvent.SHIFT_MASK) != InputEvent.SHIFT_MASK) {
			igv.getPickedBangBoxState().clear();
		}
		
		synchronized (gr) {
			for (BangBox bb : gr.getBangBoxes()) {
				Rectangle2D rect = igv.getVisualization().transformBangBox(bb);
				if (rect.contains(pt)) {
					System.out.printf("Found !-box: %s\n", bb.getName());
					igv.getPickedBangBoxState().pick(bb, true);
					break;
				}
			}
		}
	}

	public void mouseEntered(MouseEvent e) {}
	public void mouseExited(MouseEvent e) {}
	public void mousePressed(MouseEvent e) {}
	public void mouseReleased(MouseEvent e) {}
	public void mouseDragged(MouseEvent e) {}
	public void mouseMoved(MouseEvent e) {}
}
