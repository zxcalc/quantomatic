/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.oldcore.data;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.MissingNode;
import quanto.oldcore.ParseException;

/**
 *
 * @author alemer
 */
public abstract class GraphElementDataType {
	protected String dataPath;

	public GraphElementDataType(String dataPath) {
		this.dataPath = dataPath;
	}
	
	protected JsonNode findData(JsonNode node) {
		if (dataPath == null)
			return MissingNode.getInstance();
		if (dataPath.length() == 0)
			return node;
		for (String fieldName : dataPath.split("\\.")) {
			node = node.path(fieldName);
		}
		return node;
	}

	public String getDataPath() {
		return dataPath;
	}

	public void setDataPath(String dataPath) {
		this.dataPath = dataPath;
	}
	
	public abstract String getTypeName();
	
	public abstract GraphElementData parseData(JsonNode node) throws ParseException;
	
	public static class StringData extends GraphElementDataType {
		public StringData(String dataPath) {
			super(dataPath);
		}

		@Override
		public GraphElementData parseData(JsonNode node) throws ParseException {
			JsonNode strNode = findData(node);
			if (!strNode.isTextual())
				throw new ParseException("Expected string value");
			return new GraphElementData(strNode.asText());
		}

		@Override
		public String getTypeName() {
			return "String";
		}
	}
	
	public static class MathsData extends GraphElementDataType {
		public MathsData(String dataPath) {
			super(dataPath);
		}

		@Override
		public GraphElementData parseData(JsonNode node) throws ParseException {
			JsonNode strNode = findData(node);
			if (!strNode.isTextual())
				throw new ParseException("Expected string value");
			return new GraphElementMathsData(strNode.asText());
		}

		@Override
		public String getTypeName() {
			return "MathExpression";
		}
	}
}
