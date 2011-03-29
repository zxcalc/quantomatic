/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.visualization.decorators;

import edu.uci.ics.jung.visualization.VisualizationServer;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.geom.Rectangle2D;

/**
 * Shows a grid behind the graph, for debugging purposes
 *
 * @author alemer
 */
@SuppressWarnings("unused")
public class GridPaintable implements VisualizationServer.Paintable
{
        public interface BoundsCalculator {
                Rectangle2D getBounds();
        }

        private static final int SPACING = 20;
        private boolean useTransform = true;
        private Color color = Color.green;
        private BoundsCalculator boundsCalc;

        public GridPaintable(BoundsCalculator boundsCalc) {
                this.boundsCalc = boundsCalc;
        }

        public Color getColor() {
                return color;
        }

        public void setColor(Color color) {
                this.color = color;
        }

        public boolean isSubjectToTransform() {
                return useTransform;
        }

        public void setSubjectToTransform(boolean useTransform) {
                this.useTransform = useTransform;
        }

        public BoundsCalculator getBoundsCalculator() {
                return boundsCalc;
        }

        public void setBoundsCalculator(BoundsCalculator boundsCalc) {
                this.boundsCalc = boundsCalc;
        }

        public void paint(Graphics g) {
                Color oldColor = g.getColor();
                Stroke oldStroke = ((Graphics2D) g).getStroke();
                Font oldfont = g.getFont();
                Rectangle2D bounds = boundsCalc.getBounds();
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
