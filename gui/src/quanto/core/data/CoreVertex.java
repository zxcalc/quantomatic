/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core.data;

/**
 *
 * @author alemer
 */
public interface CoreVertex extends CoreObject {
	boolean isBoundaryVertex();
	String getCoreVertexType();
	String getDataAsString();
	void setData(String data);
	void updateTo(CoreVertex v);
}
