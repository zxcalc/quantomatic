package quanto.gui;

import quanto.core.data.Vertex;
import quanto.core.data.Edge;
import quanto.core.data.CoreGraph;
import java.awt.geom.Point2D;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.graph.util.BalancedEdgeIndexFunction;
import edu.uci.ics.jung.graph.util.EdgeIndexFunction;

public class TikzOutput {
	public static String generate(CoreGraph graph, Layout<Vertex, Edge> layout, boolean withArrowHeads) {
		StringBuilder tikz = new StringBuilder("\\begin{tikzpicture}[quanto]\n");
		synchronized (graph) {
			Point2D p;
			String col;
			for (Vertex v : graph.getVertices()) {
				p = layout.transform(v);
				col = v.getVertexType().toString().toLowerCase();
				tikz.append("\\node [").append(col).append(" vertex] ")
                                        .append("(").append(v.getCoreName()).append(") ")
					.append("at (")
					.append(Double.toString(Math.floor(p.getX()) / 40.0))
					.append(",")
					.append(Double.toString(Math.floor(p.getY()) / -40.0))
					.append(") {};\n");
			}
			
			EdgeIndexFunction<Vertex, Edge> eif =
				BalancedEdgeIndexFunction.<Vertex, Edge>getInstance();
			
			int idx;
			for (Edge e : graph.getEdges()) {
				idx = eif.getIndex(graph, e) + 1;
                                tikz.append("\\draw [");
				if (withArrowHeads) {
					tikz.append("-latex");
                                }
				if (idx!=0) {
					tikz.append(",bend left=").append(idx * 20);
				}
				tikz.append("] (")
                                        .append(graph.getSource(e).getCoreName())
                                        .append(") to ")
                                        .append("(")
                                        .append(graph.getDest(e).getCoreName())
                                        .append(");\n");
			}
			
			for (Vertex v : graph.getVertices()) {
				col = v.getVertexType().toString().toLowerCase();
				if (! v.getLabel().equals("0"))
					tikz.append("\\node [")
                                                .append(col)
                                                .append(" angle] at (")
                                                .append(v.getCoreName())
                                                .append(") {$")
						.append(v.getLabel())
                                                .append("$};\n");
			}
		}
		tikz.append("\\end{tikzpicture}\n");
		return tikz.toString();
	}
}
