/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui.graphhelpers;

import edu.uci.ics.jung.visualization.BasicVisualizationServer;
import edu.uci.ics.jung.visualization.VisualizationServer;
import edu.uci.ics.jung.visualization.picking.PickedState;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.Rectangle2D;
import quanto.core.BangBox;
import quanto.gui.LockableBangBoxLayout;
import quanto.core.QEdge;
import quanto.core.QVertex;
import quanto.core.QGraph;

/**
 *
 * @author alemer
 */
public class BangBoxPaintable implements VisualizationServer.Paintable
{
        private PickedState<BangBox> pickedState;
        private LockableBangBoxLayout<QVertex, QEdge> layout;
        private QGraph graph;
        private BasicVisualizationServer<QVertex, QEdge> server;

        public BangBoxPaintable(LockableBangBoxLayout<QVertex, QEdge> layout,
                                QGraph graph,
                                BasicVisualizationServer<QVertex, QEdge> server) {
                pickedState = null;
                this.layout = layout;
                this.graph = graph;
                this.server = server;
        }

        public void setPickedState(PickedState<BangBox> pickedState) {
                this.pickedState = pickedState;
        }

        public void paint(Graphics g) {
                layout.updateBangBoxes(server.getGraphLayout());
                Color oldColor = g.getColor();
                Stroke oldStroke = ((Graphics2D) g).getStroke();
                for (BangBox bb : graph.getBangBoxes()) {
                        Rectangle2D rect =
                                layout.transformBangBox(bb);

                        if (rect != null) {
                                Shape draw = server.getRenderContext().getMultiLayerTransformer().transform(rect);


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
