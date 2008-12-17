package quanto.gui;

/**
 * Interface for objects that have (hopefully unique) names.
 */
public interface HasName {
	public String getName();
	public void setName(String name);
	
	public static class Basic implements HasName {
		public String name;
		
		public Basic(String name) {
			this.name = name;
		}
		
		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}
	}
}
