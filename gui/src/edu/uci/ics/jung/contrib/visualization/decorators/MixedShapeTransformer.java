/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.visualization.decorators;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.util.Context;
import edu.uci.ics.jung.graph.util.EdgeIndexFunction;
import edu.uci.ics.jung.visualization.decorators.AbstractEdgeShapeTransformer;
import edu.uci.ics.jung.visualization.decorators.EdgeShape;
import edu.uci.ics.jung.visualization.decorators.EdgeShape.IndexedRendering;
import java.awt.Shape;

/**
 * Swing seems to have trouble with bezier curves with no inflection,
 * so we use the line transformer to draw straight edges and the
 * QuadCurve transformer otherwise.
 */
public class MixedShapeTransformer<V, E>
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
