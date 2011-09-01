package quanto.gui;

import java.awt.Dimension;
import java.awt.geom.Point2D;

import javax.swing.JOptionPane;

import quanto.core.data.CoreGraph;
import quanto.core.data.Edge;
import quanto.core.data.Vertex;
import edu.uci.ics.jung.algorithms.layout.AbstractLayout;


public class QuantoAutoLayout extends AbstractLayout<Vertex, Edge>{

	protected double vertexSpacing=20.0;
	private QuantoDotLayout dotLayout;
	
	protected QuantoAutoLayout(CoreGraph graph) {
		super(graph,  new Dimension(40, 40));;
		dotLayout=new QuantoDotLayout(graph);
	}
	
	protected double vertexWidth(Vertex v) { return 14; }
	protected double vertexHeight(Vertex v) { return 14; }
	
	public void setLocation(Vertex picked, Point2D p) {
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
	
	public void recalculateSize() {
		double right = vertexSpacing;
		double bottom = vertexSpacing;
		for (Vertex v : getGraph().getVertices()) {
			Point2D point = transform(v);
			right = Math.max(right, point.getX() + vertexWidth(v)/2.0);
			bottom = Math.max(bottom, point.getY() + vertexHeight(v)/2.0);
		}
		right += vertexSpacing;
		bottom += vertexSpacing;
		size.setSize(Math.ceil(right), Math.ceil(bottom));
	}
	
	protected boolean isWorkToDo() {
		return getGraph().getVertexCount() > 0;
	}
	
	private boolean check(){
		for(Vertex v : graph.getVertices())
		{
			if(v.getPosition()!=null)
				return true;
		}
	return false;
	}
	
	@Override
	public void initialize() {
	if (!isWorkToDo()) return;
	
	recalculateSize();
	
	if(check()){
		synchronized (getGraph()) {
			int i=1;
			Point2D p= new Point2D.Double(0, 0);
			for (Vertex v : graph.getVertices()) 
				if(transform(v).equals(p)){
					setLocation(v, size.width + vertexSpacing, vertexSpacing*i);					
					i++;
				}				
		}
	}
	else{		
		dotLayout.initialize();
		for(Vertex v: graph.getVertices())
			setLocation(v, dotLayout.transform(v));
		}
	recalculateSize();	
	}

	public void imposeLocation(Vertex v, Point2D p){
		setLocation(v, p);
	}
	
	public void reset() {
		dotLayout.reset();
	}

}
