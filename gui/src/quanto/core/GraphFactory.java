/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import quanto.core.data.CoreGraph;
import quanto.core.data.CoreVertex;
import quanto.core.data.CoreObject;

/**
 *
 * @author alemer
 */
public interface GraphFactory<G extends CoreGraph<V,E,B>,
	                      V extends CoreVertex,
			      E extends CoreObject,
			      B extends CoreObject>
{
	public G createGraph(String name);
	public V createVertex(String name, String vertexType);
	public V createBoundaryVertex(String name);
	public E createEdge(String name);
	public B createBangBox(String name);
}
