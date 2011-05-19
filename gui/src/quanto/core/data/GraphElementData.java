package quanto.core.data;

public class GraphElementData {
	private String value;

	public GraphElementData(String value) {
		this.value = value;
	}
	
	public void setValue(String value) {
		this.value = value;
	}
	
	public String getStringValue() {
		return value == null ? "" : value;
	}

}
