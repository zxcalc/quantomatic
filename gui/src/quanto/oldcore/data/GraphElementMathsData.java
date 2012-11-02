/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.oldcore.data;

/**
 *
 * @author alemer
 */
public class GraphElementMathsData extends GraphElementData {

	public GraphElementMathsData(String value) {
		super(value);
	}

	@Override
	public String getDisplayString() {
		return TexConstants.translate(getEditableString());
	}
}
