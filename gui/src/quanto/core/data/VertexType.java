package quanto.core.data;

import java.awt.Color;

public interface VertexType {
	enum DataType {
		MathExpression,
		String,
		None
	}
	VertexVisualizationData getVisualizationData();
	String getTypeName();
	/**
	 * Equivalent to (getDataType() != DataType.None)
	 * @return
	 */
	boolean hasData();
	DataType getDataType();
	GraphElementData createDefaultData();

	public static class Red implements VertexType {
		VertexVisualizationData visData = new RGVertexVisualizationData(Color.red);
		public VertexVisualizationData getVisualizationData() { return visData; }
		public String getTypeName() { return "red"; }
		public boolean hasData() { return true; }
		public DataType getDataType() { return DataType.MathExpression; }
		public GraphElementData createDefaultData() {
			GraphElementData data = new GraphElementData();
			data.setValue("0");
			return data;
		}
	}

	public static class Green implements VertexType {
		VertexVisualizationData visData = new RGVertexVisualizationData(Color.green);
		public VertexVisualizationData getVisualizationData() { return visData; }
		public String getTypeName() { return "green"; }
		public boolean hasData() { return true; }
		public DataType getDataType() { return DataType.MathExpression; }
		public GraphElementData createDefaultData() {
			GraphElementData data = new GraphElementData();
			data.setValue("0");
			return data;
		}
	}

	public static class Hadamard implements VertexType {
		VertexVisualizationData visData = new HVertexVisualizationData();
		public VertexVisualizationData getVisualizationData() { return visData; }
		public String getTypeName() { return "hadamard"; }
		public boolean hasData() { return false; }
		public DataType getDataType() { return DataType.None; }
		public GraphElementData createDefaultData() { return null; }
	}
}
