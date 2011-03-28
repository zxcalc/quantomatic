package edu.uci.ics.jung.contrib;

import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Simple unique namer for strings
 */
public class StringNamer {

	private static Pattern p = Pattern.compile("^(.+)-[0-9]+$");

	public static String getFreshName(Set<String> names, String tryName) {
		if (!names.contains(tryName)) {
			return tryName;
		}
		String newTry;
		int idx = 1;
		Matcher m = p.matcher(tryName);
		if (m.matches()) {
			tryName = m.group(1);
		} // should be fine for small numbers of duplicate names.
		while (true) {
			newTry = tryName + "-" + Integer.toString(idx);
			if (!names.contains(newTry)) {
				return newTry;
			}
			idx++;
		}
	}
}
