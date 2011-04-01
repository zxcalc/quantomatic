package quanto.core;

import java.util.Collection;

import quanto.core.data.GraphElementData;
import quanto.core.data.VertexVisualizationData;

public interface Theory {
	enum DataType {
		MathExpression,
		String,
		None
	}
	VertexVisualizationData getVertexVisualizationData(String vertexType);
	Collection<String> getVertexTypes();
	GraphElementData createDefaultData(String vertexType);
	DataType vertexDataType(String vertexType);
	/**
	 * Equivalent to (vertexDataType(vertexType) != DataType.None)
	 * @param vertexType
	 * @return
	 */
	boolean vertexHasData(String vertexType);
}
