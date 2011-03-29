/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui;

import quanto.core.RGVertex;
import quanto.core.BasicEdge;
import quanto.core.RGGraph;
import com.itextpdf.text.Document;
import com.itextpdf.text.DocumentException;
import com.itextpdf.text.pdf.PdfContentByte;
import com.itextpdf.text.pdf.PdfWriter;
import edu.uci.ics.jung.contrib.BalancedEdgeIndexFunction;
import edu.uci.ics.jung.contrib.BangBoxLayout;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.visualization.BasicVisualizationServer;
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel;
import edu.uci.ics.jung.visualization.transform.shape.GraphicsDecorator;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.io.OutputStream;
import javax.swing.JComponent;
import org.apache.commons.collections15.Predicate;
import quanto.core.BasicBangBox;
import quanto.gui.graphhelpers.QVertexAngleLabeler;
import quanto.gui.graphhelpers.BangBoxPaintable;
import quanto.gui.graphhelpers.QVertexColorTransformer;
import quanto.gui.graphhelpers.QVertexLabelTransformer;
import quanto.gui.graphhelpers.QVertexRenderer;
import quanto.gui.graphhelpers.QVertexShapeTransformer;

/**
 *
 * @author alemer
 */
public class PdfGraphVisualizationServer
        extends BasicVisualizationServer<RGVertex, BasicEdge>
{
	private final RGGraph graph;
	private BangBoxLayout<RGVertex, BasicEdge, BasicBangBox> layout;
	private BangBoxPaintable bangBoxPainter;
        private boolean arrowHeadsShown = false;

	public PdfGraphVisualizationServer(RGGraph graph) {
		this(QuantoApp.useExperimentalLayout ? new JavaQuantoDotLayout(graph) : new QuantoDotLayout(graph));
	}

	public PdfGraphVisualizationServer(BangBoxLayout<RGVertex, BasicEdge, BasicBangBox> layout) {
		super(layout);

		this.layout = layout;

		if (!(layout.getGraph() instanceof RGGraph)) {
			throw new IllegalArgumentException("Only QuantoGraphs are supported");
		}
		this.graph = (RGGraph) layout.getGraph();

		layout.initialize();
		this.bangBoxPainter = new BangBoxPaintable(layout, graph, this);

                setupRendering();
        }

	private void  setupRendering() {
		getRenderContext().setParallelEdgeIndexFunction(
			BalancedEdgeIndexFunction.<RGVertex, BasicEdge>getInstance());

		getRenderContext().setEdgeArrowPredicate(
			new Predicate<Context<Graph<RGVertex, BasicEdge>, BasicEdge>>()
			{
				public boolean evaluate(Context<Graph<RGVertex, BasicEdge>, BasicEdge> object) {
					return QuantoApp.getInstance().getPreference(QuantoApp.DRAW_ARROW_HEADS);
				}
			});

		getRenderContext().setVertexLabelTransformer(new QVertexLabelTransformer());
		getRenderContext().setVertexLabelRenderer(new QVertexAngleLabeler());
		getRenderContext().setVertexFillPaintTransformer(new QVertexColorTransformer());
		getRenderContext().setVertexShapeTransformer(new QVertexShapeTransformer());

		getRenderer().setVertexRenderer(new QVertexRenderer());
		getRenderer().getVertexLabelRenderer().setPosition(
			VertexLabel.Position.S);

		// For debugging: show a grid behind the graph
		//addPreRenderPaintable(new GridPaintable(new GridPaintable.BoundsCalculator() {
                //              public Rectangle2D getBounds() { return getGraphBounds(); }
                //}));

		addPreRenderPaintable(bangBoxPainter);
	}

        public boolean isArrowHeadsShown() {
                return arrowHeadsShown;
        }

        public void setArrowHeadsShown(boolean arrowHeadsShown) {
                this.arrowHeadsShown = arrowHeadsShown;
        }

        public void renderToPdf(OutputStream output) throws DocumentException {
                Rectangle2D bounds = getGraphBounds();
                final int width = (int) (bounds.getMaxX()) + 20;
                final int height = (int) (bounds.getMaxY()) + 20;

                Document doc = new Document(new com.itextpdf.text.Rectangle(width, height));

                PdfWriter writer = PdfWriter.getInstance(doc, output);

                doc.open();

                PdfContentByte cb = writer.getDirectContent();
                Graphics2D g2 = cb.createGraphicsShapes(width, height);

                GraphicsDecorator pdfGr = new GraphicsDecorator(g2);
                getRenderContext().setGraphicsContext(pdfGr);

                // create a virtual screen so Jung doesn't freak
                JComponent virtual = new JComponent()
                {
                        private static final long serialVersionUID = 1L;

                        @Override
                        public Dimension getSize() {
                                // make sure nothing gets clipped
                                return new Dimension(width, height);
                        }
                };


                getRenderContext().setScreenDevice(virtual);
                getRenderer().render(getRenderContext(), getGraphLayout());

                g2.dispose();
                doc.close();
        }

	/**
	 * Compute the bounding box of the graph under its current layout.
	 * @return
	 */
	public Rectangle2D getGraphBounds() {
		Rectangle2D bounds = null;
		synchronized (graph) {
			bounds = GraphVisualizationViewer.getSubgraphBounds(getGraphLayout(), graph.getVertices());
		}
		return bounds;
	}
}
