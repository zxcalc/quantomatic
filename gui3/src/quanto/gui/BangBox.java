package quanto.gui;

import java.util.TreeSet;

import edu.uci.ics.jung.contrib.HasName;

@SuppressWarnings("serial")
public class BangBox extends TreeSet<QVertex> implements HasName {
	private String name;
	
	public BangBox(String name) {
		super(new HasName.NameComparator());
		this.name = name;
	}
	
	public BangBox() {
		this(null);
	}
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

}
