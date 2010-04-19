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

public class QuantoLayout
extends DotLayout<QVertex,QEdge>
implements LockableBangBoxLayout<QVertex, QEdge>
{
	private Map<BangBox, Rectangle2D> bbRects;
	private volatile Set<String> lockedNames = null;
	public QuantoLayout(QuantoGraph graph, Dimension size) {
		super(graph, size);
		lockedNames = new HashSet<String>();
	}

	public QuantoLayout(QuantoGraph graph) {
		this(graph, new Dimension(800,600));
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
							setLocation(v, coords.get("layoutvert_" + v.getName()));
						} else {
							newLocked.add(v.getName());
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
					rect.setRect(rect.getX()-20, rect.getY()-20,
						rect.getWidth()+40, rect.getHeight()+40);
			
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

}
