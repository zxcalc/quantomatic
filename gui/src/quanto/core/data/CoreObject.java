/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core.data;

import java.util.Comparator;

/**
 *
 * @author alex
 */
public interface CoreObject {
	public String getCoreName();
	public void updateCoreName(String name);

	/**
	 * Comparator for instances of HasName
	 */
	public static class NameComparator implements Comparator<CoreObject> {
		public int compare(CoreObject o1, CoreObject o2) {
			if (o1 == null) {
				return (o2 == null) ? 0 : -1;
			}
			return o1.getCoreName().compareTo(o2.getCoreName());
		}
	}
}
