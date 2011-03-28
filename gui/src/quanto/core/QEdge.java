package quanto.core;

public class QEdge implements CoreObject, Comparable<QEdge> {
	private String name;
	public Integer index;
	
	public QEdge(String name) {
		this.name = name;
		this.index = null;
	}

	public String getCoreName() {
		return name;
	}

	public void updateCoreName(String name) {
		this.name = name;
	}

	public int compareTo(QEdge o) {
		return getCoreName().compareTo(o.getCoreName());
	}
}
