/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import net.n3.nanoxml.IXMLElement;

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
	public E createEdge(String name);
	public B createBangBox(String name);

	public G createGraphFromXml(String name, String xml)
		throws ParseException;
	public G createGraphFromXml(String name, IXMLElement xml)
		throws ParseException;
	public void updateGraphFromXml(G graph, String xml)
		throws ParseException;
	public void updateGraphFromXml(G graph, IXMLElement xml)
		throws ParseException;
}
