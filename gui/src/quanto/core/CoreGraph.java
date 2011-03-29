/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import edu.uci.ics.jung.contrib.graph.DirectedBangBoxGraph;
import edu.uci.ics.jung.visualization.util.ChangeEventSupport;

/**
 *
 * @author alemer
 */
public interface CoreGraph<V extends CoreVertex, E extends CoreObject, B extends CoreObject>
	extends DirectedBangBoxGraph<V, E, B>,
	        CoreObject,
		ChangeEventSupport
{
	// currently Core cares about this; it probably shouldn't
	void setFileName(String filename);
}
