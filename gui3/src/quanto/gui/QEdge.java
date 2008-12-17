package quanto.gui;

public class QEdge implements HasName {
	protected String name;
	
	public QEdge(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
