package quanto.core.data;

public interface VertexType {
	enum DataType {
		MathExpression,
		String,
		None
	}
	VertexVisualizationData getVisualizationData();
	String getTypeName();
    /**
     * The mnemonic for adding the vertex
     * @return a character, or null if there is no mnemonic
     */
	Character getMnemonic();
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
		private Character mnemonic;
		
		public GenericVertexType(String name, DataType dataType, Character mnemonic, VertexVisualizationData visData) {
			this.name = name;
			this.dataType = dataType;
			this.visData = visData;
			this.mnemonic = mnemonic;
		}

		public VertexVisualizationData getVisualizationData() {return this.visData;}
		public String getTypeName() {return this.name;}
		public Character getMnemonic() {return this.mnemonic;}
		public boolean hasData() {return !this.dataType.equals(DataType.None);}
		public DataType getDataType() {return this.dataType;}
	}
}
