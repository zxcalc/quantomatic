/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.visualization;

/**
 *
 * @author alemer
 */
public class LayoutContext<L, E> {
	public L layout;
	public E element;

	public static <L,E> LayoutContext<L, E> getInstance(L l, E e) {
		LayoutContext<L, E> context = new LayoutContext<L, E>();
		context.layout = l;
		context.element = e;
		return context;
	}
}
