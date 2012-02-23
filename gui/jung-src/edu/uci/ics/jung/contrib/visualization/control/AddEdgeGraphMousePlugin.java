package edu.uci.ics.jung.contrib.visualization.control;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;

import edu.uci.ics.jung.algorithms.layout.GraphElementAccessor;
import edu.uci.ics.jung.visualization.VisualizationServer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.AbstractGraphMousePlugin;

public class AddEdgeGraphMousePlugin<V,E> extends AbstractGraphMousePlugin
implements MouseListener, MouseMotionListener {
	protected VisualizationViewer<V,E> vis;
	protected Adder<V> adder;
	private Point2D current;
	
	public static interface Adder<V> {
		void addEdge(V s, V t);
	}
	
	public AddEdgeGraphMousePlugin(VisualizationViewer<V,E> vis,
			Adder<V> adder, int modifiers) {
		super(modifiers);
		this.vis = vis;
		this.adder = adder;
		vis.addPostRenderPaintable(new LinePaintable());
	}
	public void mouseClicked(MouseEvent e) {}
	public void mouseEntered(MouseEvent e) {}
	public void mouseExited(MouseEvent e) {}
	public void mouseMoved(MouseEvent e) {}
	
	public void mousePressed(MouseEvent e) {
		if (checkModifiers(e)) down = e.getPoint();
	}
	
	private V vertexAt(Point2D p) {
		GraphElementAccessor<V, E> ps = vis.getPickSupport();
		if (p == null) return null;
		if (ps == null) return null;
		return ps.getVertex(vis.getGraphLayout(), p.getX(), p.getY());
	}
	
	public void mouseReleased(MouseEvent e) {
		if (checkModifiers(e)) {
			V s = vertexAt(down);
			V t = vertexAt(e.getPoint());
			if (s!=null && t!=null) {
				adder.addEdge(s, t);
			}
		}
		down = null;
		current = null;
		vis.repaint();
	}
	public void mouseDragged(MouseEvent e) {
		if (checkModifiers(e)) {
			current = e.getPoint();
			vis.repaint();
		}
	}
	
	class LinePaintable implements VisualizationServer.Paintable {
        public void paint(Graphics g) {
            if(down != null && current != null) {
                Color oldColor = g.getColor();
                g.setColor(Color.red);
                ((Graphics2D)g).fill(new Ellipse2D.Double
                		(down.getX()-5,down.getY()-5,10,10));
                ((Graphics2D)g).draw(new Line2D.Double(down,current));
                ((Graphics2D)g).fill(new Ellipse2D.Double
                		(current.getX()-5,current.getY()-5,10,10));
                g.setColor(oldColor);
            }
        }
        
        public boolean useTransform() {
            return false;
        }
    }
	
}
