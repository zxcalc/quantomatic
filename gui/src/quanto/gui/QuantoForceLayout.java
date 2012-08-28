package quanto.gui;

import java.awt.Dimension;
import java.awt.geom.Point2D;
import java.util.HashMap;
import java.util.Map;


import org.apache.commons.collections15.Transformer;

import quanto.core.data.CoreGraph;
import quanto.core.data.Edge;
import quanto.core.data.Vertex;


import edu.uci.ics.jung.algorithms.layout.AbstractLayout;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.algorithms.layout.util.VisRunner;
import edu.uci.ics.jung.algorithms.util.IterativeContext;
import edu.uci.ics.jung.graph.DirectedGraph;

public class QuantoForceLayout extends AbstractLayout<Vertex, Edge> implements IterativeContext {

	protected Map<Vertex, Point2D> vertexVelocities;
	protected double vertexSpacing = 20.0;
	private double damping = 0.65;
	private double timestep = 0.01;
	private int done;
	private boolean modify;
	CoreGraph gr;

	protected QuantoForceLayout(DirectedGraph<Vertex, Edge> graph, Transformer<Vertex, Point2D> initializer,
			double vertexSpacing) {
		super(graph, new Dimension((int) Math.ceil(2 * vertexSpacing), (int) Math.ceil(2 * vertexSpacing)));
		setInitializer(initializer);
		vertexVelocities = new HashMap<Vertex, Point2D>();
		modify = true;
	}

	public QuantoForceLayout(CoreGraph graph, QuantoDotLayout quantoDotLayout) {
		this(graph, quantoDotLayout, 20.0);
		gr = graph;
	}

	protected void beginLayout() {
	}

	protected void endLayout() {
	}

	@Override
	public Dimension getSize() {
		return size;
	}

	public void forgetPositions() {
		locations.clear();
	}

	@Override
	public void setLocation(Vertex picked, Point2D p) {
		if (p.getX() < 20) {
			p.setLocation(20, p.getY());
		}
		if (p.getY() < 20) {
			p.setLocation(p.getX(), 20);
		}
		super.setLocation(picked, p);
		if (p.getX() + vertexSpacing > size.width) {
			size.width = (int) Math.ceil(p.getX() + vertexSpacing);
		}
		if (p.getY() + vertexSpacing > size.height) {
			size.height = (int) Math.ceil(p.getY() + vertexSpacing);
		}
	}

	protected Point2D coulombRepulsion(Vertex v1, Vertex v2) {
		Point2D p1 = locations.get(v1);
		Point2D p2 = locations.get(v2);
		double distSq = (p1.getX() - p2.getX()) * (p1.getX() - p2.getX())
				+ (p1.getY() - p2.getY()) * (p1.getY() - p2.getY());
		return new Point2D.Double(200 * (p1.getX() - p2.getX()) / distSq, 200 * (p1.getY() - p2.getY()) / distSq);
	}

	protected Point2D hookeAttraction(Vertex v1, Vertex v2) {
		Point2D p1 = locations.get(v1);
		Point2D p2 = locations.get(v2);
		double atr = 0.06;
		return new Point2D.Double(atr * (p1.getX() - p2.getX()), atr * (p1.getY() - p2.getY()));
	}

	public void step() {
		done++;
		for (Vertex v : graph.getVertices()) {
			if (!isLocked(v)) {
				Point2D netForce = new Point2D.Double(0, 0);
				vertexVelocities.put(v, new Point2D.Double(0, 0));
				for (Vertex u : graph.getVertices()) {
					if (v != u) {
						Point2D q = coulombRepulsion(u, v);
						netForce.setLocation(netForce.getX() - q.getX(), netForce.getY() - q.getY());
					}
				}
				for (Vertex u : graph.getSuccessors(v)) {
					Point2D q = hookeAttraction(u, v);
					netForce.setLocation(netForce.getX() + q.getX(), netForce.getY() + q.getY());
				}
				Point2D p = vertexVelocities.get(v);
				vertexVelocities.put(v, new Point2D.Double((p.getX() + netForce.getX() * timestep) * damping,
						(p.getY() + netForce.getY() * timestep) * damping));
				p = vertexVelocities.get(v);
				Point2D q = locations.get(v);
				Point2D r = new Point2D.Double(q.getX() + p.getX() * timestep, q.getY() + p.getY() * timestep);
				setLocation(v, r);
			}
		}
	}

	public boolean done() {
		return done > 10000;
	}

	protected boolean isWorkToDo() {
		return getGraph().getVertexCount() > 0;
	}

	public void initialize() {
		//Needed to be run on top of DotLayout to avoid an unnecessary forceLayout iteration
		Relaxer relaxer = new VisRunner(this);
		relaxer.prerelax();
		recalculateSize();
	}

	public void reset() {
		recalculateSize();
		if (modify) {
			int i = 1;
			Point2D p = new Point2D.Double(0, 0);
			for (Vertex v : graph.getVertices()) {
				if (locations.get(v).equals(p)) {
					setLocation(v, size.width + vertexSpacing, vertexSpacing * i);
					i++;
				}
			}
			recalculateSize();
		} else {
			if (!isWorkToDo()) {
				return;
			}
			beginLayout();
			for (Vertex v : graph.getVertices()) {
				vertexVelocities.put(v, new Point2D.Double(0, 0));
			}
			done = 0;
			while (!done()) {
				step();
			}
			endLayout();
			recalculateSize();
		}
	}

	//flag to distinguish between the cases when a vertex is added and other actions performed
	public void startModify() {
		modify = false;
	}

	public void endModify() {
		modify = true;
	}

	protected double vertexWidth(Vertex v) {
		return 14;
	}

	protected double vertexHeight(Vertex v) {
		return 14;
	}

	public void recalculateSize() {
		double right = vertexSpacing;
		double bottom = vertexSpacing;
		for (Vertex v : getGraph().getVertices()) {
			Point2D point = transform(v);
			right = Math.max(right, point.getX() + vertexWidth(v) / 2.0);
			bottom = Math.max(bottom, point.getY() + vertexHeight(v) / 2.0);
		}
		right += vertexSpacing;
		bottom += vertexSpacing;
		size.setSize(Math.ceil(right), Math.ceil(bottom));
	}
}