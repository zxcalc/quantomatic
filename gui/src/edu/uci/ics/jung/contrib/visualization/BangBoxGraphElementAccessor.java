/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.visualization;

import edu.uci.ics.jung.algorithms.layout.GraphElementAccessor;
import edu.uci.ics.jung.algorithms.layout.Layout;

/**
 *
 * @author alemer
 */
public interface BangBoxGraphElementAccessor<V,E,B>
	extends GraphElementAccessor<V, E>
{
	B getBangBox(Layout<V,E> layout, double x, double y);
}
