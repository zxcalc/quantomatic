package edu.uci.ics.jung.contrib;

import java.awt.geom.Point2D;

import edu.uci.ics.jung.algorithms.layout.*;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.algorithms.layout.util.VisRunner;
import edu.uci.ics.jung.algorithms.util.IterativeContext;


public class SmoothLayoutDecorator<V,E> extends LayoutDecorator<V, E> {
	private double lastTick;
	private double speed;
	private StaticLayout<V,E> currentState;
	public SmoothLayoutDecorator(Layout<V,E> delegate) {
		super(delegate);
		speed = 0.0002;
		initialize(); // make sure delegate is initialized before we go
		currentState = new StaticLayout<V,E>(delegate.getGraph());
	}
	
	public void initialize() {
		lastTick = -1.0;
		getDelegate().initialize();
		
		if (getDelegate() instanceof IterativeContext) {
			Relaxer relaxer = new VisRunner((IterativeContext)getDelegate());
			relaxer.prerelax();
		}
	}
	
	public Point2D transform(V v) {
		return currentState.transform(v);
	}
	
	@Override
	public void setLocation(V v, Point2D location) {
		super.setLocation(v, location);
		currentState.setLocation(v, location);
	}
	
	// check out strange condition here
	public void step() {
		synchronized (getGraph()) {
			boolean moved = false;
			for (V v : getGraph().getVertices()) moved = tick(v) || moved;
			//done = ! moved;
		}
	}
	
	public boolean done() {
		synchronized (getGraph()) {
			for (V v : getGraph().getVertices()) {
				if (! currentState.transform(v).equals(getDelegate().transform(v))) {
					return false;
				}
			}
		}
		return true;
	}
	
	private double millis() {
		return (double)System.currentTimeMillis();
	}
	
	private boolean tick(V v) {
		Point2D source = currentState.transform(v);
		Point2D dest = getDelegate().transform(v);
		if (lastTick == -1.0) lastTick = millis();
		
		double thisTick = millis();
		double rate = (thisTick - lastTick) * speed;
		if (rate>1) rate = 1;
		double dx = dest.getX() - source.getX();
		double dy = dest.getY() - source.getY();

		if (Math.floor(dx)==0 && Math.floor(dy)==0) {
			currentState.setLocation(v, dest);
			return false;
		} else {
			currentState.setLocation(v, new Point2D.Double(
					source.getX() + (dx*rate),
					source.getY() + (dy*rate)
			));
			return true;
		}
	}
}
