package edu.uci.ics.jung.contrib;

import java.util.Collection;
import java.util.Comparator;


/**
 * Interface for objects that have (hopefully unique) names.
 */
public interface HasName {
	public String getName();
	public void setName(String name);
	
	/**
	 * Some names are immutable, so throw this exception if setName() is called.
	 * @author aleks
	 *
	 */
	public static class ReadOnlyNameException extends RuntimeException {
		private static final long serialVersionUID = -5618659061896863724L;
	}
	
	public static class StringName implements HasName {
		public String name;
		
		
		public StringName(String name) {
			this.name = name;
		}
		
		// TODO: make sure escaping works properly
		public String getName() {
			return "\"" +
				name.replace("\\", "\\\\").replace("\"", "\\\"") +
				"\"";
		}

		public void setName(String name) {
			throw new ReadOnlyNameException();
		}
	}
	
	public static class IntName implements HasName {
		public String name;
		
		public IntName(int name) {
			this.name = Integer.toString(name);
		}
		
		public String getName() {
			return name;
		}

		public void setName(String name) {
			throw new ReadOnlyNameException();
		}
	}
	
	/**
	 * An collection of named elements.
	 * @author aleks
	 *
	 * @param <T>
	 */
	public static class CollectionName implements HasName {
		private static final long serialVersionUID = -7602337023538613612L;
		private Collection<? extends HasName> col;
		
		public CollectionName(Collection<? extends HasName> col) {
			this.col = col;
		}
		
		public String getName() {
			StringBuffer sb = new StringBuffer();
			boolean first = true;
			for (HasName n : col) {
				if (first) first = false;
				else sb.append(" ");
				sb.append('"');
				sb.append(n.getName());
				sb.append('"');
			}
			return sb.toString();
		}

		public void setName(String name) {
			throw new ReadOnlyNameException();
		}
		
	}
	
	/**
	 * Comparator for instances of HasName
	 */
	public static class NameComparator implements Comparator<HasName> {
		public int compare(HasName o1, HasName o2) {
			return o1.getName().compareTo(o2.getName());
		}
	}

}
