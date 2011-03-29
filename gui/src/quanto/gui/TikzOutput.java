package quanto.gui;

import quanto.core.RGVertex;
import quanto.core.BasicEdge;
import quanto.core.RGGraph;
import java.awt.geom.Point2D;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.BalancedEdgeIndexFunction;
import edu.uci.ics.jung.graph.util.EdgeIndexFunction;

public class TikzOutput {
	public static String generate(RGGraph graph, Layout<RGVertex, BasicEdge> layout, boolean withArrowHeads) {
		StringBuilder tikz = new StringBuilder("\\begin{tikzpicture}[quanto]\n");
		synchronized (graph) {
			Point2D p;
			String col;
			for (RGVertex v : graph.getVertices()) {
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
			
			EdgeIndexFunction<RGVertex, BasicEdge> eif =
				BalancedEdgeIndexFunction.<RGVertex, BasicEdge>getInstance();
			
			int idx;
			for (BasicEdge e : graph.getEdges()) {
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
			
			for (RGVertex v : graph.getVertices()) {
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
