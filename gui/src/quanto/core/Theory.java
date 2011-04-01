package quanto.core;

import java.util.Collection;

import quanto.core.data.VertexType;

public interface Theory {
	VertexType getVertexType(String typeName);
	Collection<VertexType> getVertexTypes();
}
