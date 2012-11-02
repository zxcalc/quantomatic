package quanto.oldcore.data;

public class VertexType {
	private String typeName;
	private GraphElementDataType dataType;
	private VertexVisualizationData visualizationData;
	private Character mnemonic;

	public VertexType(String typeName) {
		this.typeName = typeName;
	}

	public String getTypeName() {
		return typeName;
	}

	public void setTypeName(String typeName) {
		this.typeName = typeName;
	}

	public VertexVisualizationData getVisualizationData() {
		return visualizationData;
	}

	public void setVisualizationData(VertexVisualizationData visualizationData) {
		this.visualizationData = visualizationData;
	}

    /**
     * The mnemonic for adding the vertex
     * @return a character, or null if there is no mnemonic
     */
	public Character getMnemonic() {
		return mnemonic;
	}

	public void setMnemonic(Character mnemonic) {
		this.mnemonic = mnemonic;
	}

	/**
	 * Equivalent to (getDataType() != null)
	 */
	public boolean hasData() {
		return dataType != null;
	}

	/**
	 * The type of data found at dataPath
	 */
	public GraphElementDataType getDataType() {
		return dataType;
	}

	public void setDataType(GraphElementDataType dataType) {
		this.dataType = dataType;
	}
}
