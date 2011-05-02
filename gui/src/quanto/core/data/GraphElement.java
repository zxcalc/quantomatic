package quanto.core.data;

public class GraphElement implements CoreObject, Comparable<GraphElement> {
	protected GraphElementData data;
	protected String coreName;
	
	public GraphElement(String name) {
		this.coreName = name;
	}

	public String getCoreName() {
		return coreName;
	}

	public void updateCoreName(String name) {
		this.coreName = name;
	}
	
	public GraphElementData getData() {
		return data;
	}
	
	public void setData(GraphElementData data) {
		this.data = data;
	}

	public int compareTo(GraphElement o) {
		if (coreName == null) {
			if (o.coreName == null)
				return 0;
			else
				return -o.coreName.compareTo(coreName);
		}
		return coreName.compareTo(o.coreName);
	}

}
