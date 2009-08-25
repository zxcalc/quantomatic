package edu.uci.ics.jung.contrib;

import java.util.Collection;

/**
 * An interface marking objects whose names are already quoted.
 * @author aleks
 *
 */
public interface HasQuotedName extends HasName {
	/**
	 * Wrap an object of type HasName, and quote it.
	 * @author aleks
	 *
	 */
	public static class QuotedName implements HasQuotedName {
		HasName delegate;
		public QuotedName(HasName delegate) {
			this.delegate = delegate;
		}
//		public QuotedName(String name) {
//			this.delegate = new StringName(name);
//		}
		public String getName() {
			return "\"" +
				delegate.getName().replace("\\", "\\\\").replace("\"", "\\\"") +
				"\"";
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
	public static class QuotedCollectionName implements HasQuotedName {
		private static final long serialVersionUID = -7602337023538613612L;
		private Collection<? extends HasName> col;
		
		public QuotedCollectionName(Collection<? extends HasName> col) {
			this.col = col;
		}
		
		public String getName() {
			StringBuffer sb = new StringBuffer();
			boolean first = true;
			for (HasName n : col) {
				if (first) first = false;
				else sb.append(" ");
				sb.append(new QuotedName(n).getName());
			}
			return sb.toString();
		}

		public void setName(String name) {
			throw new ReadOnlyNameException();
		}
		
	}
}
