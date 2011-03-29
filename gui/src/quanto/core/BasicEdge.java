package quanto.core;

/**
 * An edge with no data attached.
 *
 * @author alemer
 */
public class BasicEdge implements CoreObject, Comparable<BasicEdge> {
	private String name;
	
	public BasicEdge(String name) {
		this.name = name;
	}

	public String getCoreName() {
		return name;
	}

	public void updateCoreName(String name) {
		this.name = name;
	}

	public int compareTo(BasicEdge o) {
		return getCoreName().compareTo(o.getCoreName());
	}
}
