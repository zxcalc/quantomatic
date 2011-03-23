package quanto.gui;

import quanto.core.BangBox;
import java.awt.geom.Rectangle2D;
import java.util.Set;

import edu.uci.ics.jung.algorithms.layout.Layout;

public interface LockableBangBoxLayout<V,E> extends Layout<V,E> {
	public void lock(Set<V> verts);
	public void unlock(Set<V> verts);
	public boolean isLocked(V vert);
	public void updateBangBoxes(Layout<V,E> layout);
	public Rectangle2D transformBangBox(BangBox bb);
	public void recalculateBounds();
}
