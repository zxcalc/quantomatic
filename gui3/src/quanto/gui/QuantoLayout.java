package quanto.gui;

import java.awt.Dimension;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.DotLayout;

public class QuantoLayout extends DotLayout<QVertex,QEdge> {
	Map<BangBox, Rectangle2D> bbRects;
	public QuantoLayout(QuantoGraph graph, Dimension size) {
		super(graph, size);
	}

	public QuantoLayout(QuantoGraph graph) {
		super(graph);
	}
	
	public void initialize() {
		try {
			synchronized (getGraph()) {
				if (getGraph().getVertexCount() == 0) return;
				
				String viz = "digraph {\n" +
					graphToDot() + bangBoxDot() + "\n}\n\n";
				
				Map<String,Point2D> coords = getCoordMap(viz);
				bbRects = new HashMap<BangBox, Rectangle2D>();
				
				synchronized (getGraph()) {
					for (QVertex v : getVertices()) {
						setLocation(v, coords.get("layoutvert_" + v.getName()));
					}
					
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
