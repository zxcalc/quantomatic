package quanto.core.data;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class GraphElement implements CoreObject, Comparable<GraphElement> {
	protected GraphElementData data;
	protected String coreName;
	private Map<String, String> userData = new HashMap<String, String>();
	
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
	
	public Map<String, String> getUserData() {
		return Collections.unmodifiableMap(userData);
	}
	
	public void setUserData(Map<String, String> map) {
		userData = new HashMap<String, String>(map);
	}
	
	public String getUserDataEntry(String k) {
		return userData.get(k);
	}
	
	public void setUserDataEntry(String k, String v) {
		userData.put(k, v);
	}

	@Override
	public String toString() {
		return coreName == null ? "<unnamed>" : coreName;
	}
}
