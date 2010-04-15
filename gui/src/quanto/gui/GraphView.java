package quanto.gui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTextField;

import org.apache.commons.collections15.Predicate;
import org.apache.commons.collections15.Transformer;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.ByteBuffer;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfWriter;

import edu.uci.ics.jung.algorithms.layout.*;
import edu.uci.ics.jung.contrib.BalancedEdgeIndexFunction;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.graph.util.EdgeIndexFunction;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.VisualizationServer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.decorators.AbstractEdgeShapeTransformer;
import edu.uci.ics.jung.visualization.decorators.EdgeShape;
import edu.uci.ics.jung.visualization.decorators.EdgeShape.IndexedRendering;
import edu.uci.ics.jung.visualization.picking.PickedState;
import edu.uci.ics.jung.visualization.renderers.VertexLabelRenderer;
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel;
import edu.uci.ics.jung.visualization.transform.MutableTransformer;
import edu.uci.ics.jung.visualization.transform.shape.GraphicsDecorator;

public class GraphView extends VisualizationViewer<QVertex,QEdge> {
	private static final long serialVersionUID = -1915610684250038897L;
	public QuantoGraph graph;
	private LockableBangBoxLayout<QVertex, QEdge> quantoLayout;
	private VisualizationServer.Paintable boundsPaint;
	private BangBoxPaintable bangBoxPainter;
	
	public GraphView(QuantoGraph g) {
		this(g, new Dimension(800, 600));
	}
	
	public GraphView(QuantoGraph g, Dimension size) {
		super(new StaticLayout<QVertex, QEdge>(g));
		
		if (QuantoApp.useExperimentalLayout) {
			quantoLayout = new JavaQuantoLayout(g);
		} else {
			quantoLayout = new QuantoLayout(g);
		}
		
		setGraphLayout(quantoLayout);
		setPreferredSize(size);
		getGraphLayout().initialize();
		setBackground(new Color(0.97f,0.97f,0.97f));
		this.graph = g;
		
        getRenderContext().setVertexFillPaintTransformer(
        		new Transformer<QVertex,Paint>() {
					public Paint transform(QVertex v) {
						return v.getColor();
					}
        		});
        
        
        getRenderContext().setParallelEdgeIndexFunction(
        		BalancedEdgeIndexFunction.<QVertex,QEdge>getInstance());
        
        /**
         * Swing seems to have trouble with bezier curves with no inflection,
         * so we use the line transformer to draw straight edges and the
         * QuadCurve transformer otherwise.
         */
        class MixedShapeTransformer<V,E>
        extends AbstractEdgeShapeTransformer<V, E>
        implements IndexedRendering<V, E> {
        	EdgeShape.QuadCurve<V, E> quad = new EdgeShape.QuadCurve<V,E>();
        	EdgeShape.Line<V, E> line = new EdgeShape.Line<V, E>();
        	private EdgeIndexFunction<V, E> peif=null;
			public Shape transform(Context<Graph<V, E>, E> input) {
				// if we have no index function, or the index is -1 (straight)
				// then draw a straight line.
				if (peif == null ||
					peif.getIndex(input.graph, input.element)==-1)
					return line.transform(input);
				// otherwise draw a quadratic curve
				else return quad.transform(input);
			}

			public EdgeIndexFunction<V, E> getEdgeIndexFunction() {
				return peif;
			}
			public void setEdgeIndexFunction(
					EdgeIndexFunction<V, E> peif) {
				this.peif = peif;
				quad.setEdgeIndexFunction(peif);
			}
        }
        
        getRenderContext().setEdgeShapeTransformer(
        		new MixedShapeTransformer<QVertex,QEdge>());
        
        
        getRenderContext().setEdgeArrowPredicate(
        		new Predicate<Context<Graph<QVertex,QEdge>,QEdge>>() {
					public boolean evaluate(Context<Graph<QVertex, QEdge>, QEdge> object) {
						return QuantoApp.getInstance().getPreference(QuantoApp.DRAW_ARROW_HEADS);
					}
        		});
        
        getRenderContext().setVertexLabelTransformer(
        		new Transformer<QVertex, String>() {
					public String transform(QVertex v) {
						if (v.getVertexType()==QVertex.Type.BOUNDARY)
							return Integer.toString(getGraph().getBoundaryIndex(v));
						else if (v.getVertexType()==QVertex.Type.HADAMARD)
							return "H";
						else return v.getAngle();
					}
        		});
        
        getRenderContext().setVertexLabelRenderer(new AngleLabeler());
        
        getRenderer().getVertexLabelRenderer().setPosition(
        		VertexLabel.Position.CNTR);
        
        getRenderContext().setVertexShapeTransformer(
        		new Transformer<QVertex, Shape>() {
        			public Shape transform(QVertex v) {
        				if (v.getVertexType()==QVertex.Type.BOUNDARY) {
        					String text = getRenderContext()
        					.getVertexLabelTransformer().transform(v);
        					double width = 
        						new JTextField(text).getPreferredSize().getWidth();
        					return new Rectangle2D.Double(-(width/2),-6,width,12);
        				} else if (v.getVertexType()==QVertex.Type.HADAMARD) {
        					return new Rectangle2D.Double(-7,-7,14,14);
        				} else {
        					return new Ellipse2D.Double(-7,-7,14,14);
        				}
        			}
        		});
        
        
        bangBoxPainter = new BangBoxPaintable();
        addPreRenderPaintable(bangBoxPainter);
	}
	
	/**
	 * Draw a bounding box around the graph.
	 */
	public void enableBoundingBox() {
		if (boundsPaint == null) boundsPaint = new BoundsPaintable();
		addPostRenderPaintable(boundsPaint);
	}
	
	/**
	 * Disable the bounding box.
	 */
	public void disableBoundingBox() {
		removePostRenderPaintable(boundsPaint);
	}
	
	public QuantoGraph getGraph() {
		return graph;
	}
	public void setGraph(QuantoGraph graph) {
		this.graph = graph;
	}
	
	/**
	 * Compute the bounding box of the graph under its current layout.
	 * @return
	 */
	public Rectangle2D getGraphBounds() {
		Rectangle2D bounds = null;
		synchronized (getGraph()) {
			bounds = getSubgraphBounds(getGraph().getVertices());
		}
		return bounds;
	}
	
	/**
	 * Compute the bounding box of the subgraph under its current layout.
	 * @return
	 */
	public Rectangle2D getSubgraphBounds(Collection<QVertex> subgraph) {
		Layout<QVertex, QEdge> layout = getGraphLayout();
		Rectangle2D bounds = null;
		synchronized (getGraph()) {
			for (QVertex v : subgraph) {
				Point2D p = layout.transform(v);
				if (bounds == null)
					bounds = new Rectangle2D.Double(p.getX(),p.getY(),0,0);
				else bounds.add(p);
			}
			if (bounds == null)
				bounds = new Rectangle2D.Double(0,0,10,10);
			bounds.setRect(bounds.getX()-20, bounds.getY()-20,
					bounds.getWidth()+40, bounds.getHeight()+40);
		}
		
		if (bounds == null)
			return new Rectangle2D.Double(0.0d, 0.0d, 20.0d, 20.0d);
		else return bounds;
	}
	
	/**
	 * Compute a bounding box and scale such that the largest dimension fits within the
	 * view port.
	 */
	public void zoomToFit() {
		MutableTransformer mt = getRenderContext()
			.getMultiLayerTransformer().getTransformer(Layer.VIEW);
		Rectangle2D gb = getGraphBounds();
		Dimension vb = getPreferredSize();
		double centerX = vb.getWidth()/2.0;
		double centerY = vb.getHeight()/2.0; 
		mt.translate(
				centerX - gb.getCenterX(),
				centerY - gb.getCenterY());
		float scale = Math.min(
				(float)(vb.getWidth() / gb.getWidth()),
				(float)(vb.getHeight() / gb.getHeight()));
		if (scale < 1)
				mt.scale(scale, scale, new Point2D.Double(centerX,centerY));
	}
	
	/**
	 * A red box that surrounds the graph. NOTE: this doesn't appear correctly if the graph is
	 * transformed.
	 */
	class BoundsPaintable implements VisualizationServer.Paintable {
        public void paint(Graphics g) {
        	Color oldColor = g.getColor();
            g.setColor(Color.red);
        	((Graphics2D)g).draw(getGraphBounds());
        	g.setColor(oldColor);
        }
        
        public boolean useTransform() {return false;}
    }

	public LockableBangBoxLayout<QVertex, QEdge> getQuantoLayout() {
		return quantoLayout;
	}

	public void setQuantoLayout(LockableBangBoxLayout<QVertex, QEdge> quantoLayout) {
		this.quantoLayout = quantoLayout;
	}
	
	
	public byte[] exportPdf() {
		// save values in case we're using this GraphView for other stuff
		GraphicsDecorator gc = getRenderContext().getGraphicsContext();
		JComponent vv = getRenderContext().getScreenDevice();
		try {
			Rectangle2D bounds = getGraphBounds();
			final int width = (int)(bounds.getMaxX())+20;
			final int height = (int)(bounds.getMaxY())+20;
			
			ByteBuffer buf = new ByteBuffer();
			//BufferedOutputStream file = new BufferedOutputStream(buf);
			Document doc = new Document(new com.lowagie.text.Rectangle(width,height));
			//FileOutputStream fw = new FileOutputStream("/Users/aleks/itexttest2.pdf");
			
			PdfWriter writer = PdfWriter.getInstance(doc, buf);
			
			doc.open();
			
			PdfContentByte cb = writer.getDirectContent();
	    	Graphics2D g2 = cb.createGraphicsShapes(width, height);
	    	
	    	GraphicsDecorator pdfGr = new GraphicsDecorator(g2);
			getRenderContext().setGraphicsContext(pdfGr);
			
			// create a virtual screen so Jung doesn't freak
			JComponent virtual = new JComponent() {
				private static final long serialVersionUID = 1L;
				public Dimension getSize() {
					// make sure nothing gets clipped
					return new Dimension(width,height);
				}
			};
			
			
			getRenderContext().setScreenDevice(virtual);
			getRenderer().render(getRenderContext(), getGraphLayout());
	    	g2.dispose();
	    	doc.close();
	    	
	    	byte[] minbuf = new byte[buf.size()];
	    	byte[] oldbuf = buf.getBuffer();
	    	for (int i = 0; i<buf.size(); ++i) minbuf[i] = oldbuf[i];
	    	
	    	return minbuf;
		} catch (DocumentException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (gc!=null) getRenderContext().setGraphicsContext(gc);
			if (vv!=null) getRenderContext().setScreenDevice(vv);
		}

		return null;
	}
	
	public BangBoxPaintable getBangBoxPainter() {
		return bangBoxPainter;
	}
	
	public class BangBoxPaintable implements VisualizationServer.Paintable {
		private PickedState<BangBox> pickedState;
		
		public BangBoxPaintable() {
			pickedState = null;
		}
		
		public void setPickedState(PickedState<BangBox> pickedState) {
			this.pickedState = pickedState;
		}
		
		public void paint(Graphics g) {
			getQuantoLayout().updateBangBoxes(getGraphLayout());
			Color oldColor = g.getColor();
			Stroke oldStroke = ((Graphics2D)g).getStroke();
			for (BangBox bb : getGraph().getBangBoxes()) {
				Rectangle2D rect = 
					getQuantoLayout().transformBangBox(bb);
				
				if (rect != null) {
					Shape draw = getRenderContext()
						.getMultiLayerTransformer().transform(rect);
				
					
					g.setColor(Color.lightGray);
					
					((Graphics2D)g).fill(draw);
					
					if (pickedState != null && pickedState.isPicked(bb)) {
						((Graphics2D)g).setStroke(new BasicStroke(2));
						g.setColor(Color.blue);
					} else {
						((Graphics2D)g).setStroke(new BasicStroke(1));
						g.setColor(Color.gray);
					}
					
					((Graphics2D)g).draw(draw);
				}
			}
			g.setColor(oldColor);
			((Graphics2D)g).setStroke(oldStroke);
		}

		public boolean useTransform() {
			return false;
		}
	}
	
	private class AngleLabeler implements VertexLabelRenderer {
		Map<QVertex,Labeler> components;
		
		public AngleLabeler () {
		}
		
		public <T> Component getVertexLabelRendererComponent(
				JComponent vv, Object value, Font font,
				boolean isSelected, T vertex)
		{
			if (value instanceof String &&
				vertex instanceof QVertex)
			{
				String val = TexConstants.translate((String)value);
				
				JLabel lab = new JLabel(val);
				Color col = null;
				QVertex qv = (QVertex)vertex;
				if (val.equals("0") && qv.getVertexType()!=QVertex.Type.BOUNDARY)
					return new JLabel();
				if (qv.getColor().equals(Color.red)) {
					col = new Color(255,170,170);
					lab.setBackground(col);
					lab.setOpaque(true);
				} else if (qv.getColor().equals(Color.green)) {
					col = new Color(150,255,150);
					lab.setBackground(col);
					lab.setOpaque(true);
				}
				
				return lab;
			} else {
				return new JLabel("");
			}
		}
	}
}
