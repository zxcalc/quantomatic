package quanto.gui;

import edu.uci.ics.jung.contrib.HasName;

public class QEdge implements HasName {
	private String name;
	public Integer index;
	
	public QEdge(String name) {
		this.name = name;
		this.index = null;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
