package quanto.gui;

import java.awt.Dimension;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.DotLayout;
import java.awt.Rectangle;

public class QuantoLayout
extends DotLayout<QVertex,QEdge>
implements LockableBangBoxLayout<QVertex, QEdge>
{
	private static final double VERTEX_PADDING = 20;
	private static final double INIT_BOUNDS_PADDING = 20;
	private Map<BangBox, Rectangle2D> bbRects;
	private volatile Set<String> lockedNames = null;
	private Rectangle boundingRect = new Rectangle(0, 0, 0, 0);

	public QuantoLayout(QuantoGraph graph, Dimension size) {
		super(graph, size);
		lockedNames = new HashSet<String>();
	}

	public QuantoLayout(QuantoGraph graph) {
		this(graph, new Dimension(800,600));
	}

	@Override
	public Dimension getSize() {
		return boundingRect.getSize();
	}
	
	public void initialize() {
		try {
			synchronized (getGraph()) {
				String viz = "digraph {\n" +
					graphToDot() + bangBoxDot() + "\n}\n\n";
				
				// don't bother running dot if there are no vertices. this speeds
				//  up startup time.
				Map<String,Point2D> coords =
					(getGraph().getVertexCount() == 0) ?
							new HashMap<String, Point2D>() :
							getCoordMap(viz);

				bbRects = new HashMap<BangBox, Rectangle2D>();
				
				synchronized (getGraph()) {
					// clean up the locked set, removing old vertices
					Set<String> newLocked = new HashSet<String>(lockedNames.size());

					for (QVertex v : getGraph().getVertices()) {
						if (!lockedNames.contains(v.getName())) {
							Point2D point = coords.get("layoutvert_" + v.getName());
							setLocation(v, point);
						} else {
							newLocked.add(v.getName());
							boundingRect.add(transform(v));
						}
					}
					
					lockedNames = newLocked;
					
					for (BangBox bb : ((QuantoGraph)getGraph()).getBangBoxes()) {
						Rectangle2D rect = new Rectangle2D.Double();
						Point2D init;
						if (bb.isEmpty()) {
							init = coords.get("emptybb_vert_" + bb.getName());
							rect.setRect(init.getX(), init.getY(), 0, 0);
						}
					}
					boundingRect.setSize(
						(int)(boundingRect.getWidth() + INIT_BOUNDS_PADDING),
						(int)(boundingRect.getHeight() + INIT_BOUNDS_PADDING));
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
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
					rect.setRect(rect.getX()-VERTEX_PADDING, rect.getY()-VERTEX_PADDING,
						rect.getWidth()+(2* VERTEX_PADDING), rect.getHeight()+(2* VERTEX_PADDING));
			
					bbRects.put(bb, rect);
				}
			}
		}
	}
	
	protected String bangBoxDot() {
		StringBuffer buf = new StringBuffer();
		synchronized (getGraph()) {
			for (BangBox bb : ((QuantoGraph)getGraph()).getBangBoxes()) {
				buf.append("subgraph cluster_");
				buf.append(bb.getName());
				buf.append(" {\n");
				if (bb.size()>0) {
					for (QVertex v : bb) {
						buf.append("layoutvert_");
						buf.append(v.getName());
						buf.append("; ");
					}
				} else {
					buf.append("emptybb_vert_");
					buf.append(bb.getName());
					buf.append(" [label=\"\"]; ");
				}
				buf.append("\n}\n");
			}
		}
		return buf.toString();
	}
	
	public Rectangle2D transformBangBox(BangBox bb) {
		return bbRects.get(bb);
	}

	@Override
	public void setLocation(QVertex picked, Point2D p) {
		if (p.getX() < 20)
			p.setLocation(20, p.getY());
		if (p.getY() < 20)
			p.setLocation(p.getX(), 20);
		super.setLocation(picked, p);
		boundingRect.add(new Rectangle2D.Double(
			p.getX()-VERTEX_PADDING,
			p.getY()-VERTEX_PADDING,
			2*VERTEX_PADDING,
			2*VERTEX_PADDING));
	}

	@Override
	public void setLocation(QVertex picked, double x, double y) {
		if (x < 20)
			x = 20;
		if (y < 20)
			y = 20;
		super.setLocation(picked, x, y);
		boundingRect.add(new Rectangle2D.Double(
			x-VERTEX_PADDING,
			y-VERTEX_PADDING,
			2*VERTEX_PADDING,
			2*VERTEX_PADDING));
	}
}
