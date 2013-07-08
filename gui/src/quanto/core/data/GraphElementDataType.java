/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.data;

import com.fasterxml.jackson.databind.JsonNode;
import quanto.core.ParseException;

/**
 *
 * @author alemer
 */
public abstract class GraphElementDataType {
	protected String findLabel(JsonNode node) throws ParseException {
		node = node.path("label");
		if (!node.isTextual())
			throw new ParseException("Expected string value");
		return node.asText();
	}
	public abstract String getTypeName();
	
	public abstract GraphElementData parseData(JsonNode node) throws ParseException;
	
	public static class StringData extends GraphElementDataType {
		@Override
		public GraphElementData parseData(JsonNode node) throws ParseException {
			return new GraphElementData(findLabel(node));
		}

		@Override
		public String getTypeName() {
			return "String";
		}
	}
	
	public static class MathsData extends GraphElementDataType {
		@Override
		public GraphElementData parseData(JsonNode node) throws ParseException {
			return new GraphElementMathsData(findLabel(node));
		}

		@Override
		public String getTypeName() {
			return "MathExpression";
		}
	}
}
