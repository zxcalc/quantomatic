/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.uci.ics.jung.contrib.graph;

import edu.uci.ics.jung.graph.DirectedGraph;

/**
 * Interface for a directed graph with a collection of subgraphs, known as
 * !-boxes.
 *
 * @author alex
 */
public interface DirectedBangBoxGraph<V,E,B>
	extends BangBoxGraph<V, E, B>,
	        DirectedGraph<V, E> {
}
