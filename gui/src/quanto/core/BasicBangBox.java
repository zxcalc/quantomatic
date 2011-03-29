package quanto.core;


/**
 * A bang box with no data attached
 *
 * @author alemer
 */
@SuppressWarnings("serial")
public class BasicBangBox implements CoreObject {
	private String name;

	public BasicBangBox(String name) {
		this.name = name;
	}

	public BasicBangBox() {
		this(null);
	}

	public String getCoreName() {
		return name;
	}

	public void updateCoreName(String name) {
		this.name = name;
	}
}
