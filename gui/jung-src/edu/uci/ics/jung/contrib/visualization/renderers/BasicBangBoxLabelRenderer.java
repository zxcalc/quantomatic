package edu.uci.ics.jung.contrib.visualization.renderers;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.graph.BangBoxGraph;
import edu.uci.ics.jung.contrib.visualization.BangBoxGraphRenderContext;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.RenderContext;
import edu.uci.ics.jung.visualization.renderers.VertexLabelRenderer;
import edu.uci.ics.jung.visualization.renderers.BasicVertexLabelRenderer.OutsidePositioner;
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position;
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Positioner;
import edu.uci.ics.jung.visualization.transform.BidirectionalTransformer;
import edu.uci.ics.jung.visualization.transform.shape.GraphicsDecorator;
import edu.uci.ics.jung.visualization.transform.shape.ShapeTransformer;
import edu.uci.ics.jung.visualization.transform.shape.TransformingGraphics;

public class BasicBangBoxLabelRenderer<V, E, B>
implements BangBoxGraphRenderer.BangBoxLabel<V, E, B> {

     protected Position position = Position.SE;
     private Positioner positioner = new OutsidePositioner();
     
     public BasicBangBoxLabelRenderer() {
          super();
     }

     public Component prepareRenderer(BangBoxGraphRenderContext<V,E,B> rc, BangBoxLabelRenderer bangLabelRenderer, Object value, 
               boolean isSelected, B bangBox) {
          return rc.getBangBoxLabelRenderer().<B>getBangBoxLabelRendererComponent(rc.getScreenDevice(), value, 
                    null, isSelected, bangBox);
     }

     /**
      * Labels the specified vertex with the specified label.  
      * Uses the font specified by this instance's 
      * <code>VertexFontFunction</code>.  (If the font is unspecified, the existing
      * font for the graphics context is used.)  If vertex label centering
      * is active, the label is centered on the position of the vertex; otherwise
     * the label is offset slightly.
     */
    public void labelBangBox(BangBoxGraphRenderContext<V,E,B> rc, Layout<V,E> layout, B b, String label) {
     Graph<V,E> graph = layout.getGraph();
        
     prepareRenderer(rc, rc.getBangBoxLabelRenderer(), label,
               rc.getPickedBangBoxState().isPicked(b), b);
        GraphicsDecorator g = rc.getGraphicsContext();
    }
}
