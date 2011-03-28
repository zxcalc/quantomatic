package quanto.core;

import java.util.TreeSet;


@SuppressWarnings("serial")
public class QBangBox implements CoreObject {
	private String name;

	public QBangBox(String name) {
		this.name = name;
	}

	public QBangBox() {
		this(null);
	}

	public String getCoreName() {
		return name;
	}

	public void updateCoreName(String name) {
		this.name = name;
	}
}
