package edu.uci.ics.jung.contrib.algorithms.layout;

import java.awt.Dimension;
import java.awt.geom.Point2D;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.collections15.Transformer;

import quanto.core.data.Vertex;
import quanto.gui.QuantoAutoLayout;


import edu.uci.ics.jung.algorithms.layout.AbstractLayout;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.graph.DirectedGraph;

public abstract class AbstractForceLayout <V,E> extends AbstractLayout<V, E> implements DynamicBoundsLayout{

	protected Map<V,Point2D > vertexVelocities;
	protected Map<V,Point2D > vertexPositions;
	protected double vertexSpacing = 20.0;
	Transformer<V, Point2D> initializer;
	
	protected AbstractForceLayout(DirectedGraph<V, E> graph, Transformer<V, Point2D> initializer,
			double vertexSpacing ) {
		super(graph, new Dimension((int)Math.ceil(2*vertexSpacing), (int)Math.ceil(2*vertexSpacing)));
		this.initializer=initializer;
	}


	protected void beginLayout() {}

	protected void endLayout() {}
	
	@Override
	public Dimension getSize() {
		return size;
	}


	@Override
	public void setLocation(V picked, Point2D p) {
		if (p.getX() < 20)
			p.setLocation(20, p.getY());
		if (p.getY() < 20)
			p.setLocation(p.getX(), 20);
		super.setLocation(picked, p);
		if (p.getX() + vertexSpacing > size.width) {
			size.width = (int)Math.ceil(p.getX() + vertexSpacing);
		}
		if (p.getY() + vertexSpacing > size.height) {
			size.height = (int)Math.ceil(p.getY() + vertexSpacing);
		}
	}

	protected Point2D coulombRepulsion(V v1, V v2){	
		Point2D p1= locations.get(v1);
		Point2D p2= locations.get(v2);
		double distSq=(p1.getX()-p2.getX())*(p1.getX()-p2.getX())+				
				(p1.getY()-p2.getY())*(p1.getY()-p2.getY());
		//distSq=Math.sqrt(distSq);
		return new Point2D.Double(200*(p1.getX()-p2.getX())/distSq, 200*(p1.getY()-p2.getY())/distSq ); 
	}
	
	protected Point2D hookeAttraction(V v1, V v2){
		Point2D p1= locations.get(v1);
		Point2D p2= locations.get(v2);
		double atr=0.06;
		return new Point2D.Double(atr*(p1.getX()-p2.getX()), atr*(p1.getY()-p2.getY()));
	}
	
	protected void forceLayout(){		
		double kineticEnergy=graph.getVertices().size();
		double damping=0.65;
		double timestep=0.01;
		vertexVelocities= new HashMap<V, Point2D>();
		vertexPositions= new HashMap<V, Point2D>();
		for (V v : graph.getVertices()) 
			vertexVelocities.put(v, new Point2D.Double(0, 0));
		int i=0;
		//int sq=graph.getVertices().size()*graph.getVertices().size();
		while(i<10000){
			kineticEnergy=0;
			for (V v : graph.getVertices()) {
				if (!isLocked(v)){
				Point2D netForce=new Point2D.Double(0, 0);
				vertexVelocities.put(v, new Point2D.Double(0, 0));
				for (V u : graph.getVertices()) {
					if(v!=u){
						Point2D q=coulombRepulsion(u, v);
						netForce.setLocation(netForce.getX()-q.getX(), netForce.getY()-q.getY());
					}
				}
				for(V u : graph.getSuccessors(v)){
					Point2D	q=hookeAttraction(u, v);
					netForce.setLocation(netForce.getX()+q.getX(), netForce.getY()+q.getY());
				}
				Point2D p=vertexVelocities.get(v);
				vertexVelocities.put(v, new Point2D.Double((p.getX()+netForce.getX()*timestep)*damping , 
									(p.getY()+netForce.getY()*timestep)*damping ));
				p=vertexVelocities.get(v);
				Point2D q=locations.get(v);
				Point2D r= new Point2D.Double(q.getX()+p.getX()*timestep, q.getY()+p.getY()*timestep);
				setLocation(v, r);
				kineticEnergy+=p.getX()*p.getX()+p.getY()*p.getY();
				}
			}
			i++;
		}
	}
	
	
	protected boolean isWorkToDo() {
		return getGraph().getVertexCount() > 0;
	}

	public void initialize() {
	setInitializer(initializer);
	
	
	/*
		Point2D p= new Point2D.Double(0, 0);
	for(V v : graph.getVertices())
		if(!transform(v).equals(p))

	for (V v : graph.getVertices())
		if(transform(v).equals(p))
			setLocation(v, initializer.transform(v));
	*/
	}


	public void reset() {
	if (!isWorkToDo()) return;				
		beginLayout();		
		forceLayout();
		//layoutGraph();
		endLayout();
		recalculateSize();
	}

	

	protected void layoutGraph() {
	for(V v : graph.getVertices())
		setLocation(v, vertexPositions.get(v));
	}
	
	protected double vertexWidth(V v) { return 14; }
	protected double vertexHeight(V v) { return 14; }

	public void recalculateSize() {
		double right = vertexSpacing;
		double bottom = vertexSpacing;
		for (V v : getGraph().getVertices()) {
			Point2D point = transform(v);
			right = Math.max(right, point.getX() + vertexWidth(v)/2.0);
			bottom = Math.max(bottom, point.getY() + vertexHeight(v)/2.0);
		}
		right += vertexSpacing;
		bottom += vertexSpacing;
		size.setSize(Math.ceil(right), Math.ceil(bottom));
	}
	
	}
