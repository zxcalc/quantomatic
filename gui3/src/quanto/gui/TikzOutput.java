package quanto.gui;

import java.awt.geom.Point2D;

import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.contrib.BalancedEdgeIndexFunction;
import edu.uci.ics.jung.graph.util.EdgeIndexFunction;

public class TikzOutput {
	public static String generate(QuantoGraph graph, Layout<QVertex, QEdge> layout) {
		StringBuffer tikz = new StringBuffer("\\begin{tikzpicture}\n");
		synchronized (graph) {
			Point2D p;
			String col;
			for (QVertex v : graph.getVertices()) {
				p = layout.transform(v);
				col = v.getVertexType().toString().toLowerCase();
				tikz.append("\\node [" + col + " vertex] ")
					.append("(" + v.getName() + ") ")
					.append("at (")
					.append(Double.toString(Math.floor(p.getX()) / 40.0))
					.append(",")
					.append(Double.toString(Math.floor(p.getY()) / -40.0))
					.append(") {};\n");
			}
			
			EdgeIndexFunction<QVertex, QEdge> eif =
				BalancedEdgeIndexFunction.<QVertex, QEdge>getInstance();
			
			int idx;
			for (QEdge e : graph.getEdges()) {
				idx = eif.getIndex(graph, e) + 1;
				if (QuantoApp.getInstance().getPreference(
						QuantoApp.DRAW_ARROW_HEADS))
					tikz.append("\\draw [-latex");
				else tikz.append("\\draw [");
				if (idx!=0) {
					tikz.append(",bend left=" + Integer.toString(idx * 20));
				}
				tikz.append("] (" + graph.getSource(e).getName() + ") to ")
					.append("(" + graph.getDest(e).getName() + ");\n");
			}
			
			for (QVertex v : graph.getVertices()) {
				col = v.getVertexType().toString().toLowerCase();
				if (! v.getAngle().equals("0"))
					tikz.append("\\node ["+ col + " angle] at (" + v.getName() + ") {$")
						.append(v.getAngle()).append("$};\n");
			}
		}
		tikz.append("\\end{tikzpicture}\n");
		return tikz.toString();
	}
}
