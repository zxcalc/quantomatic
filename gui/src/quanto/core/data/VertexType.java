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
	
	public class GenericVertexType implements VertexType{
		
		private String name;
		private DataType dataType;
		private VertexVisualizationData visData;
		
		public GenericVertexType(String name, DataType dataType, VertexVisualizationData visData) {
			this.name = name;
			this.dataType = dataType;
			this.visData = visData;
		}
		public VertexVisualizationData getVisualizationData() {return this.visData;}
		public String getTypeName() {return this.name;}
		public boolean hasData() {return !this.dataType.equals(DataType.None);}
		public DataType getDataType() {return this.dataType;}
	}
	
	public static class X implements VertexType {
		VertexVisualizationData visData = new SvgVertexVisualizationData(
				VertexType.class.getResource("resources/red.svg"),
				new Color(255, 170, 170)
				);
		public VertexVisualizationData getVisualizationData() { return visData; }
		public String getTypeName() { return "X"; }
		public boolean hasData() { return true; }
		public DataType getDataType() { return DataType.MathExpression; }
	}

	public static class Z implements VertexType {
		VertexVisualizationData visData = new SvgVertexVisualizationData(
				VertexType.class.getResource("resources/green.svg"),
				new Color(150, 255, 150)
				);
		public VertexVisualizationData getVisualizationData() { return visData; }
		public String getTypeName() { return "Z"; }
		public boolean hasData() { return true; }
		public DataType getDataType() { return DataType.MathExpression; }
	}

	public static class Hadamard implements VertexType {
		SvgVertexVisualizationData visData;

		public Hadamard() {
			visData = new SvgVertexVisualizationData(
				VertexType.class.getResource("resources/hadamard.svg"),
				null
			);
			visData.setAntiAliasingOn(false);
		}
		public VertexVisualizationData getVisualizationData() { return visData; }
		public String getTypeName() { return "hadamard"; }
		public boolean hasData() { return false; }
		public DataType getDataType() { return DataType.None; }
	}
}
