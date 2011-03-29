/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.algorithms.layout;

import edu.uci.ics.jung.contrib.graph.DirectedBangBoxGraph;
import edu.uci.ics.jung.algorithms.layout.Layout;
import java.awt.geom.Rectangle2D;

/**
 *
 * @author alex
 */
public interface BangBoxLayout<V,E,B> extends Layout<V,E> {
	Rectangle2D transformBangBox(B bb);
	DirectedBangBoxGraph<V, E, B> getGraph();
}
