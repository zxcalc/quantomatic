package org.apache.commons.collections15.contrib;

import java.util.Comparator;

/**
 * Dummy comparator, when we need an order, but don't care what it is.
 * @author aleks
 *
 * @param <T>
 */
public class HashCodeComparator<T extends Object> implements Comparator<T> {
	public int compare(T o1, T o2) {
		if (o1.hashCode() < o2.hashCode()) return -1;
		else if (o1.hashCode() > o2.hashCode()) return 1;
		else return 0;
	}
}
