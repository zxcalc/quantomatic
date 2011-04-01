package quanto.core.data;


/**
 * A bang box
 *
 * @author alemer
 */
public class BangBox implements CoreObject {
	private String name;

	public BangBox(String name) {
		this.name = name;
	}

	public BangBox() {
		this(null);
	}

	public String getCoreName() {
		return name;
	}

	public void updateCoreName(String name) {
		this.name = name;
	}
}
