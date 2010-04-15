package quanto.gui;

import java.awt.Dimension;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.*;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.AKDotLayout;
import edu.uci.ics.jung.graph.DirectedGraph;

public class JavaQuantoLayout
extends AKDotLayout<QVertex,QEdge>
implements LockableBangBoxLayout<QVertex, QEdge>
{
	private Map<BangBox, Rectangle2D> bbRects;
	private volatile Set<String> lockedNames = null;
	public JavaQuantoLayout(QuantoGraph graph, Dimension size) {
		super((DirectedGraph<QVertex,QEdge>)graph);
		lockedNames = new HashSet<String>();
	}

	public JavaQuantoLayout(QuantoGraph graph) {
		this(graph, new Dimension(800,600));
	}
	
	
	
	public void lock(Set<QVertex> verts) {
		synchronized (getGraph()) {
			for (QVertex v : verts) lockedNames.add(v.getName());
		}
	}
	
	public void unlock(Set<QVertex> verts) {
		synchronized (getGraph()) {
			for (QVertex v : verts) lockedNames.remove(v.getName());
		}
	}
	
	public boolean isLocked(QVertex vert) {
		return lockedNames.contains(vert.getName());
	}
	
	public void updateBangBoxes(Layout<QVertex,QEdge> layout) {
		synchronized (getGraph()) {
			for (BangBox bb : ((QuantoGraph)getGraph()).getBangBoxes()) {
				if (! bb.isEmpty()) {
					Point2D p = layout.transform(bb.first());
					Rectangle2D rect =
						new Rectangle2D.Double(p.getX(),p.getY(),0,0);
					for (QVertex v : bb) rect.add(layout.transform(v));
					rect.setRect(rect.getX()-20, rect.getY()-20,
						rect.getWidth()+40, rect.getHeight()+40);
			
					bbRects.put(bb, rect);
				}
			}
		}
	}
	
	public Rectangle2D transformBangBox(BangBox bb) {
		return bbRects.get(bb);
	}

}

