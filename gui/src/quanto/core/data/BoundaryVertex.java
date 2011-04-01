package quanto.core.data;

public class BoundaryVertex extends Vertex {
	public BoundaryVertex(String name) {
		super(name);
	}
	
	@Override
	public String getVertexType() {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public void setVertexType(String vertexType) {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public void updateTo(Vertex v) {
		if (!v.isBoundaryVertex()) {
			throw new IllegalArgumentException("Cannot update a boundary vertex to a non-boundary vertex");
		}
		setData(v.getData());
	}
	
	@Override
	public boolean isBoundaryVertex() {
		return true;
	}
}
