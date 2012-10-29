package quanto.core.data;

public class GraphElementData {
	private String value;

	public GraphElementData(String value) {
		this.value = value;
	}
	
	public void setString(String value) {
		this.value = value;
	}
	
	public String getEditableString() {
		return value == null ? "" : value;
	}
	
	public String getDisplayString() {
		return getEditableString();
	}

	@Override
	public String toString() {
		return getEditableString();
	}
}
