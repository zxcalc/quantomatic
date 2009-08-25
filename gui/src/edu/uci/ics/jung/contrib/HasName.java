package edu.uci.ics.jung.contrib;

import java.util.Comparator;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


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
		public String getName() {
			return name;
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
	 * Comparator for instances of HasName
	 */
	public static class NameComparator implements Comparator<HasName> {
		public int compare(HasName o1, HasName o2) {
			return o1.getName().compareTo(o2.getName());
		}
	}
	
	/**
	 * Simple unique namer for strings
	 */
	public static class StringNamer {
		private static Pattern p = Pattern.compile("^(.+)-[0-9]+$");
		
		public static String getFreshName(Set<String>names, String tryName) {
			if (!names.contains(tryName)) return tryName;
			String newTry; int idx = 1;
			
			Matcher m = p.matcher(tryName);
			if (m.matches()) {
				tryName = m.group(1);
			}
			
			// TODO: this method for finding a fresh name can be MUCH improved, but
			// should be fine for small numbers of duplicate names.
			while (true) {
				newTry = tryName + "-" + Integer.toString(idx);
				if (!names.contains(newTry)) return newTry;
				idx++;
			}
		}
	}

}
