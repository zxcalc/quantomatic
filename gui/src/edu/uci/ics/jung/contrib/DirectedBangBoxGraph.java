/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib;

import edu.uci.ics.jung.graph.DirectedGraph;
import java.util.Collection;

/**
 * Interface for a graph with a collection of subgraphs, known as !-boxes.
 *
 * @author alex
 */
public interface DirectedBangBoxGraph<V,E,B> extends DirectedGraph<V, E> {
	/**
	 * Adds a !-box to this graph.
	 *
	 * Fails if @p bangbox is already an element of this graph, or
	 * @p bangbox or @p vertices is null.
	 *
	 * @param bangbox   the object representing the !-box
	 * @param vertices  the vertices that will be contained in the !-box
	 * @return @c true if the add is successful, and @c false otherwise
	 * @throws IllegalArgumentException  if bangbox or vertices is null
	 */
	public boolean addBangBox(B bangbox, Collection<? extends V> vertices);
	/**
	 * Whether this graph contains a particular !-box.
	 *
	 * Equivalent to getBangBoxes().contains(bangbox).
	 *
	 * @param bangbox  the object representing the !-box
	 * @return @c true if the graph contains @p bangbox, @c false otherwise
	 */
	public boolean containsBangBox(B bangbox);
	/**
	 * The number of !-boxes in this graph
	 *
	 * Equivalent to getBangBoxes().size().
	 *
	 * @return the number of !-boxes in this graph
	 */
	public int getBangBoxCount();
	/**
	 * Get !-boxes in the graph.
	 *
	 * In general, this obeys the Collection contract, and therefore makes
	 * no guarantees about the ordering of the !-boxes within the set.
	 *
	 * @return a view on the !-boxes in this graph
	 */
	public Collection<B> getBangBoxes();
	/**
	 * Get the contents of a !-box.
	 *
	 * In general, this obeys the Collection contract, and therefore makes
	 * no guarantees about the ordering of the vertices within the set.
	 *
	 * @param bangbox  the object representing the !-box
	 * @return a view on a subset of vertices of this graph
	 */
	public Collection<V> getBoxedVertices(B bangbox);
	/**
	 * Changes the contents of a !-box in this graph.
	 *
	 * Fails if @p bangbox is not an element of this graph, or
	 * @p bangbox or @p vertices is null.
	 *
	 * @param bangbox   the object representing the !-box
	 * @param vertices  the vertices that will be contained in the !-box
	 * @return the original contents of the bangbox
	 * @throws IllegalArgumentException  if bangbox or vertices is null, or
	 *                                   bangbox is not in the graph
	 */
	public Collection<V> setBoxedVertices(B bangbox, Collection<? extends V> vertices);
	/**
	 * Removes a !-box from this graph.
	 *
	 * Fails if @p bangbox is not an element of this graph, or
	 * @p bangbox is null.
	 *
	 * This does not affect the vertices contained in the !-box.
	 * @param bangbox   the object representing the !-box
	 * @return @c true if the remove is successful, and @c false otherwise
	 */
	public boolean removeBangBox(B bangbox);
}
