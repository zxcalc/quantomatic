/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.ByteBuffer;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfWriter;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.contrib.BalancedEdgeIndexFunction;
import edu.uci.ics.jung.contrib.SmoothLayoutDecorator;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.graph.util.EdgeIndexFunction;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.RenderContext;
import edu.uci.ics.jung.visualization.VisualizationServer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.decorators.AbstractEdgeShapeTransformer;
import edu.uci.ics.jung.visualization.decorators.EdgeShape;
import edu.uci.ics.jung.visualization.decorators.EdgeShape.IndexedRendering;
import edu.uci.ics.jung.visualization.picking.MultiPickedState;
import edu.uci.ics.jung.visualization.picking.PickedState;
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel;
import edu.uci.ics.jung.visualization.renderers.VertexLabelRenderer;
import edu.uci.ics.jung.visualization.transform.MutableTransformer;
import edu.uci.ics.jung.visualization.transform.shape.GraphicsDecorator;
import edu.uci.ics.jung.visualization.util.ChangeEventSupport;
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
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import org.apache.commons.collections15.Predicate;
import org.apache.commons.collections15.Transformer;

/**
 *
 * @author alex
 */
public class GraphVisualizationViewer
       extends VisualizationViewer<QVertex, QEdge>
{
	private static final long serialVersionUID = -1723894723956293847L;
	private QuantoGraph graph;
	private BangBoxPaintable bangBoxPainter;
	private VisualizationServer.Paintable boundsPaint;
	private boolean boundsPaintingEnabled = false;
	private LockableBangBoxLayout<QVertex, QEdge> layout;
	private SmoothLayoutDecorator<QVertex, QEdge> smoothLayout;
	/**
	 * Holds the state of which bang boxes of the graph are currently
	 * "picked"
	 */
	protected PickedState<BangBox> pickedBangBoxState;
	private Color pageBackground = new Color(0.99f, 0.99f, 0.99f);


	public GraphVisualizationViewer(QuantoGraph graph) {
		this(QuantoApp.useExperimentalLayout ? new JavaQuantoLayout(graph) : new QuantoLayout(graph));
	}

	public GraphVisualizationViewer(LockableBangBoxLayout<QVertex, QEdge> layout) {
		super(layout);

		this.layout = layout;

		if (!(layout.getGraph() instanceof QuantoGraph)) {
			throw new IllegalArgumentException("Only QuantoGraphs are supported");
		}
		this.graph = (QuantoGraph) layout.getGraph();

		layout.initialize();
		setBackground(new Color(0.97f, 0.97f, 0.97f));

		setupRenderContext(getRenderContext());

		// For debugging: show a grid behind the graph
		//addPreRenderPaintable(new GridPaintable(Color.gray, false));
		//addPreRenderPaintable(new GridPaintable());

		bangBoxPainter = new BangBoxPaintable();
		addPreRenderPaintable(bangBoxPainter);
		setPickedBangBoxState(new MultiPickedState<BangBox>());

		setPreferredSize(calculateGraphSize());
	}

	private Dimension calculateGraphSize()
	{
		Dimension size = layout.getSize();
		Rectangle2D rect = new Rectangle2D.Double(0, 0, size.getWidth(), size.getHeight());
		getRenderContext().getMultiLayerTransformer().transform(rect);
		size.setSize(rect.getWidth(), rect.getHeight());
		return size;
	}

	/**
	 * Compute a bounding box and scale such that the largest dimension fits within the
	 * view port.
	 */
	public void zoomToFit(Dimension size) {
		MutableTransformer mt = getRenderContext().getMultiLayerTransformer().getTransformer(Layer.VIEW);
		Rectangle2D gb = getGraphBounds();
		double centerX = size.getWidth() / 2.0;
		double centerY = size.getHeight() / 2.0;
		mt.translate(
			centerX - gb.getCenterX(),
			centerY - gb.getCenterY());
		float scale = Math.min(
			(float) (size.getWidth() / gb.getWidth()),
			(float) (size.getHeight() / gb.getHeight()));
		if (scale < 1) {
			mt.scale(scale, scale, new Point2D.Double(centerX, centerY));
		}
	}

	private void setupRenderContext(RenderContext<QVertex, QEdge> context) {
		final RenderContext<QVertex, QEdge> origContext = context;
		context.setVertexFillPaintTransformer(
			new Transformer<QVertex, Paint>()
			{
				public Paint transform(QVertex v) {
					return v.getColor();
				}
			});

		context.setParallelEdgeIndexFunction(
			BalancedEdgeIndexFunction.<QVertex, QEdge>getInstance());

		/**
		 * Swing seems to have trouble with bezier curves with no inflection,
		 * so we use the line transformer to draw straight edges and the
		 * QuadCurve transformer otherwise.
		 */
		class MixedShapeTransformer<V, E>
			extends AbstractEdgeShapeTransformer<V, E>
			implements IndexedRendering<V, E>
		{
			EdgeShape.QuadCurve<V, E> quad = new EdgeShape.QuadCurve<V, E>();
			EdgeShape.Line<V, E> line = new EdgeShape.Line<V, E>();
			private EdgeIndexFunction<V, E> peif = null;

			public Shape transform(Context<Graph<V, E>, E> input) {
				// if we have no index function, or the index is -1 (straight)
				// then draw a straight line.
				if (peif == null
					|| peif.getIndex(input.graph, input.element) == -1) {
					return line.transform(input);
				}
				// otherwise draw a quadratic curve
				else {
					return quad.transform(input);
				}
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

		context.setEdgeShapeTransformer(
			new MixedShapeTransformer<QVertex, QEdge>());

		context.setEdgeArrowPredicate(
			new Predicate<Context<Graph<QVertex, QEdge>, QEdge>>()
			{
				public boolean evaluate(Context<Graph<QVertex, QEdge>, QEdge> object) {
					return QuantoApp.getInstance().getPreference(QuantoApp.DRAW_ARROW_HEADS);
				}
			});

		context.setVertexLabelTransformer(
			new Transformer<QVertex, String>()
			{
				public String transform(QVertex v) {
					if (v.getVertexType() == QVertex.Type.BOUNDARY) {
						return Integer.toString(getGraph().getBoundaryIndex(v));
					}
					else if (v.getVertexType() == QVertex.Type.HADAMARD) {
						return "H";
					}
					else {
						return v.getAngle();
					}
				}
			});

		context.setVertexLabelRenderer(new AngleLabeler());

		// FIXME: not sure what to do about this, since it isn't
		// really part of the render context, but is linked to
		// the previous line
		getRenderer().getVertexLabelRenderer().setPosition(
			VertexLabel.Position.CNTR);

		context.setVertexShapeTransformer(
			new Transformer<QVertex, Shape>()
			{
				public Shape transform(QVertex v) {
					if (v.getVertexType() == QVertex.Type.BOUNDARY) {
						String text = origContext.getVertexLabelTransformer().transform(v);
						double width =
							new JTextField(text).getPreferredSize().getWidth();
						return new Rectangle2D.Double(-(width / 2), -6, width, 12);
					}
					else if (v.getVertexType() == QVertex.Type.HADAMARD) {
						return new Rectangle2D.Double(-7, -7, 14, 14);
					}
					else {
						return new Ellipse2D.Double(-7, -7, 14, 14);
					}
				}
			});
	}

	public QuantoGraph getGraph() {
		return graph;
	}

	public boolean isLocked(QVertex v) {
		return layout.isLocked(v);
	}

	public void lock(Set<QVertex> verts) {
		layout.lock(verts);
	}

	public void unlock(Set<QVertex> verts) {
		layout.unlock(verts);
	}

	public Rectangle2D transformBangBox(BangBox bb) {
		return layout.transformBangBox(bb);
	}

	public boolean isLayoutSmoothingEnabled() {
		return smoothLayout != null;
	}

	public void setLayoutSmoothingEnabled(boolean enable) {
		if (enable && smoothLayout == null) {
			smoothLayout = new SmoothLayoutDecorator<QVertex,QEdge>(layout);
			smoothLayout.setOrigin(new Point2D.Double(0.0,0.0));
			super.setGraphLayout(smoothLayout);
			setLayout(null);  // ?
		}
		else if (!enable && smoothLayout != null) {
			smoothLayout = null;
			super.setGraphLayout(layout);
			setLayout(null);  // ?
		}
	}

	public void setSmoothingOrigin(Point2D o) {
		if (smoothLayout != null)
			smoothLayout.setOrigin(o);
	}

	public void setSmoothingOrigin(double x, double y) {
		if (smoothLayout != null)
			smoothLayout.setOrigin(x, y);
	}

	@Override
	public void setGraphLayout(Layout<QVertex, QEdge> layout) {
		if (!(layout instanceof LockableBangBoxLayout)) {
			throw new IllegalArgumentException("Layout must be a LockableBangBoxLayout");
		}
		super.setGraphLayout(layout);
		this.layout = (LockableBangBoxLayout<QVertex, QEdge>) layout;
	}

	/**
	 * Draw a bounding box around the graph.
	 */
	public void setBoundingBoxEnabled(boolean enabled) {
		if (enabled != boundsPaintingEnabled) {
			if (enabled) {
				if (boundsPaint == null) {
					boundsPaint = new BoundsPaintable();
				}
				prependPreRenderPaintable(boundsPaint);
			}
			else {
				removePreRenderPaintable(boundsPaint);
			}
		}
	}

	public boolean isBoundingBoxEnabled() {
		return boundsPaintingEnabled;
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
				if (bounds == null) {
					bounds = new Rectangle2D.Double(p.getX(), p.getY(), 0, 0);
				}
				else {
					bounds.add(p);
				}
			}
			if (bounds != null) {
				bounds.setRect(bounds.getX() - 20,
				               bounds.getY() - 20,
				               bounds.getWidth() + 40,
				               bounds.getHeight() + 40);
			}
		}

		if (bounds == null) {
			return new Rectangle2D.Double(0.0d, 0.0d, 20.0d, 20.0d);
		}
		else {
			return bounds;
		}
	}

	public byte[] exportPdf() {
		// save values in case we're using this GraphView for other stuff
		GraphicsDecorator gc = getRenderContext().getGraphicsContext();
		JComponent vv = getRenderContext().getScreenDevice();
		try {
			Rectangle2D bounds = getGraphBounds();
			final int width = (int) (bounds.getMaxX()) + 20;
			final int height = (int) (bounds.getMaxY()) + 20;

			ByteBuffer buf = new ByteBuffer();
			//BufferedOutputStream file = new BufferedOutputStream(buf);
			Document doc = new Document(new com.lowagie.text.Rectangle(width, height));
			//FileOutputStream fw = new FileOutputStream("/Users/aleks/itexttest2.pdf");

			PdfWriter writer = PdfWriter.getInstance(doc, buf);

			doc.open();

			PdfContentByte cb = writer.getDirectContent();
			Graphics2D g2 = cb.createGraphicsShapes(width, height);

			GraphicsDecorator pdfGr = new GraphicsDecorator(g2);
			getRenderContext().setGraphicsContext(pdfGr);

			// create a virtual screen so Jung doesn't freak
			JComponent virtual = new JComponent()
			{
				private static final long serialVersionUID = 1L;

				public Dimension getSize() {
					// make sure nothing gets clipped
					return new Dimension(width, height);
				}
			};


			getRenderContext().setScreenDevice(virtual);
			getRenderer().render(getRenderContext(), getGraphLayout());
			g2.dispose();
			doc.close();

			byte[] minbuf = new byte[buf.size()];
			byte[] oldbuf = buf.getBuffer();
			for (int i = 0; i < buf.size(); ++i) {
				minbuf[i] = oldbuf[i];
			}

			return minbuf;
		}
		catch (DocumentException e) {
			e.printStackTrace();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		finally {
			if (gc != null) {
				getRenderContext().setGraphicsContext(gc);
			}
			if (vv != null) {
				getRenderContext().setScreenDevice(vv);
			}
		}

		return null;
	}

	/*@Override
	public Dimension getPreferredSize() {
		return layout.getSize();
	}*/

	public void relayout() {
		getGraphLayout().initialize();

		((ChangeEventSupport) getGraphLayout()).fireStateChanged();

		Relaxer relaxer = getModel().getRelaxer();
		if (relaxer != null) {
			relaxer.relax();
		}

		setPreferredSize(calculateGraphSize());

		revalidate();
	}

	public void update() {
		revalidate();
		repaint();
	}

	public void setPickedBangBoxState(PickedState<BangBox> pickedBangBoxState) {
		if (pickEventListener != null && this.pickedBangBoxState != null) {
			this.pickedBangBoxState.removeItemListener(pickEventListener);
		}
		this.pickedBangBoxState = pickedBangBoxState;
		if (pickEventListener == null) {
			pickEventListener = new ItemListener() {
				public void itemStateChanged(ItemEvent e) {
					repaint();
				}
			};
		}
		pickedBangBoxState.addItemListener(pickEventListener);
		bangBoxPainter.setPickedState(pickedBangBoxState);
	}

	public PickedState<BangBox> getPickedBangBoxState() {
		return pickedBangBoxState;
	}

	public Color getPageBackground() {
		return pageBackground;
	}

	public void setPageBackground(Color pageBackground) {
		this.pageBackground = pageBackground;
		repaint();
	}

	/**
	 * A red box that surrounds the graph. NOTE: this doesn't appear correctly if the graph is
	 * transformed.
	 */
	private class BoundsPaintable implements VisualizationServer.Paintable
	{
		public void paint(Graphics g) {
			Graphics2D gr = (Graphics2D)g;
			Color oldColor = g.getColor();
			Dimension size = layout.getSize();
			Rectangle2D bounds = new Rectangle2D.Double(0, 0, size.getWidth(), size.getHeight());
			g.setColor(pageBackground);
			gr.fill(bounds);
			g.setColor(Color.black);
			gr.draw(bounds);
			g.setColor(oldColor);
		}

		public boolean useTransform() {
			return true;
		}
	}

	/**
	 * Shows a grid behind the graph, for debugging purposes
	 */
	private class GridPaintable implements VisualizationServer.Paintable
	{
		private static final int SPACING = 20;
		private boolean useTransform = true;
		private Color color = Color.green;

		public GridPaintable() {
		}

		public GridPaintable(boolean useTransform) {
			this.useTransform = useTransform;
		}

		public GridPaintable(Color color) {
			this.color = color;
		}

		public GridPaintable(Color color, boolean useTransform) {
			this.color = color;
			this.useTransform = useTransform;
		}

		public void paint(Graphics g) {
			Color oldColor = g.getColor();
			Stroke oldStroke = ((Graphics2D) g).getStroke();
			Font oldfont = g.getFont();
			Rectangle2D bounds = getGraphBounds();
			if (bounds.getHeight() > 0 && bounds.getWidth() > 0)
			{
				g.setFont(oldfont.deriveFont(2));
				((Graphics2D)g).setStroke(new BasicStroke(1));
				int left = (int)bounds.getMinX();
				int right = (int)bounds.getMaxX() + 1;
				int top = (int)bounds.getMinY();
				int bottom = (int)bounds.getMaxY() + 1;
				for (int row = top; row <= bottom; row += SPACING)
				{
					g.setColor(color);
					g.drawLine(left, row, right, row);
					g.setColor(Color.black);
					g.drawString(String.valueOf(row), right+2, row+6);
					g.drawString(String.valueOf(row), left-30, row+6);
				}
				g.setColor(color);
				for (int col = left; col <= right; col += SPACING)
				{
					g.drawLine(col, top, col, bottom);
				}
				g.setColor(Color.black);
				for (int col = left; col <= right; col += SPACING)
				{
					g.translate(col-3, bottom+5);
					((Graphics2D)g).rotate(Math.PI/2);
					g.drawString(String.valueOf(col), 0, 0);
					((Graphics2D)g).rotate(-Math.PI/2);
					g.translate(0, (top-30)-(bottom+5));
					((Graphics2D)g).rotate(Math.PI/2);
					g.drawString(String.valueOf(col), 0, 0);
					((Graphics2D)g).rotate(-Math.PI/2);
					g.translate(-(col-3), -(top-30));
				}
			}
			g.setColor(oldColor);
			((Graphics2D) g).setStroke(oldStroke);
			g.setFont(oldfont);
		}

		public boolean useTransform() {
			return useTransform;
		}
	}

	private class BangBoxPaintable implements VisualizationServer.Paintable
	{
		private PickedState<BangBox> pickedState;

		public BangBoxPaintable() {
			pickedState = null;
		}

		public void setPickedState(PickedState<BangBox> pickedState) {
			this.pickedState = pickedState;
		}

		public void paint(Graphics g) {
			layout.updateBangBoxes(getGraphLayout());
			Color oldColor = g.getColor();
			Stroke oldStroke = ((Graphics2D) g).getStroke();
			for (BangBox bb : getGraph().getBangBoxes()) {
				Rectangle2D rect =
					layout.transformBangBox(bb);

				if (rect != null) {
					Shape draw = getRenderContext().getMultiLayerTransformer().transform(rect);


					g.setColor(Color.lightGray);

					((Graphics2D) g).fill(draw);

					if (pickedState != null && pickedState.isPicked(bb)) {
						((Graphics2D) g).setStroke(new BasicStroke(2));
						g.setColor(Color.blue);
					}
					else {
						((Graphics2D) g).setStroke(new BasicStroke(1));
						g.setColor(Color.gray);
					}

					((Graphics2D) g).draw(draw);
				}
			}
			g.setColor(oldColor);
			((Graphics2D) g).setStroke(oldStroke);
		}

		public boolean useTransform() {
			return false;
		}
	}

	private class AngleLabeler implements VertexLabelRenderer
	{
		Map<QVertex, Labeler> components;

		public AngleLabeler() {
		}

		public <T> Component getVertexLabelRendererComponent(
			JComponent vv, Object value, Font font,
			boolean isSelected, T vertex) {
			if (value instanceof String
				&& vertex instanceof QVertex) {
				String val = TexConstants.translate((String) value);

				JLabel lab = new JLabel(val);
				Color col = null;
				QVertex qv = (QVertex) vertex;
				if (val.equals("0") && qv.getVertexType() != QVertex.Type.BOUNDARY) {
					return new JLabel();
				}
				if (qv.getColor().equals(Color.red)) {
					col = new Color(255, 170, 170);
					lab.setBackground(col);
					lab.setOpaque(true);
				}
				else if (qv.getColor().equals(Color.green)) {
					col = new Color(150, 255, 150);
					lab.setBackground(col);
					lab.setOpaque(true);
				}
				
				JPanel pan = new JPanel();
				pan.setLayout(null);
				pan.setOpaque(false);
				pan.add(lab);
				
				Dimension sz = lab.getPreferredSize();
				int w = sz.width;
				int h = sz.height;
				
				Point2D p1 = 
					getRenderContext()
					.getMultiLayerTransformer()
					.transform(new Point2D.Double(0.0d,0.0d));
				Point2D p2 =
					getRenderContext()
					.getMultiLayerTransformer()
					.transform(new Point2D.Double(20.0d,0.0d));
				int sep = (int)(p2.getX()-p1.getX());
				
				lab.setBounds(0, 2*sep, w, h);
				pan.setBounds(0, 0, w, 2*sep + h);
				
				return pan;
			}
			else {
				return new JLabel("");
			}
		}
	}
}
