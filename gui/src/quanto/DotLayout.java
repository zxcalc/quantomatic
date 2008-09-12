package quanto;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.StringTokenizer;

public class DotLayout implements GraphLayout {
	
	public static class DotException extends RuntimeException {
		
		public DotException(String s) {
			super(s);
		}
	}

	public void layout(Graph graph) {
		String viz = graph.toDot();
		try {
			Process dot = Runtime.getRuntime().exec("dot -Tplain");
			BufferedReader dotIn = new BufferedReader(new InputStreamReader(dot
					.getInputStream()));

			OutputStreamWriter dotOut = new OutputStreamWriter(dot
					.getOutputStream());

			dotOut.write(viz);
			dotOut.close();

			String ln = dotIn.readLine();
			synchronized (graph) {
				while (!ln.equals("stop")) {
					StringTokenizer tk = new StringTokenizer(ln);
					String cmd = tk.nextToken();
					if (cmd.equals("node")) {
						String name = tk.nextToken();
						Vertex v = graph.vertices.get(name);
						if(v == null) {
							throw new DotException("Unknown vertex in dot layout: "+ name);
						}
						else {
							v.destX = (int) (Float.parseFloat(tk.nextToken()) * 50.0) + 43;
							v.destY = (int) (Float.parseFloat(tk.nextToken()) * 50.0) + 43;
						}
					}
					ln = dotIn.readLine();
				}
			}	
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
//			/* this is complete overkill for layout - we just need grab the x and y coords for each vertex
//			 * 
//			 */
//			//System.out.println("NOW READING DOT");
//			String ln = dotIn.readLine();
//			StringTokenizer tk;
//			String cmd, name;
//			int x, y;
//			Vertex n, n1, n2;
//			synchronized (graph) {
//				graph.edges.clear();
//				while (!ln.equals("stop")) {
//					//System.out.println(ln);
//					tk = new StringTokenizer(ln);
//					cmd = tk.nextToken();
//					if (cmd.equals("node")) {
//						name = tk.nextToken();
//						n = (Vertex) graph.vertices.get(name);
//						if (n == null) {
//							n = new Vertex(name, 0, 0);
//							graph.addVertex(n);
//						}
//						x = (int) (Float.parseFloat(tk.nextToken()) * 50.0) + 43;
//						y = (int) (Float.parseFloat(tk.nextToken()) * 50.0) + 43;
//
//						tk.nextToken();
//						tk.nextToken();
//						tk.nextToken();
//						tk.nextToken();
//						tk.nextToken();
//						n.setColor(tk.nextToken());
//						n.setDest(x, y);
//					} else if (cmd.equals("edge")) {
//						n1 = (Vertex) graph.vertices.get(tk.nextToken());
//						n2 = (Vertex) graph.vertices.get(tk.nextToken());
//
//						if (n1 == null || n2 == null) {
//							//System.out.println("Edge spec given before vertices defined.");
//						} else {
//							Edge e = graph.newEdge(n1, n2);
//							int controlCount = Integer.parseInt(tk.nextToken());
//
//							for (int i = 0; i < controlCount; ++i) {
//								x = (int) (Float.parseFloat(tk.nextToken()) * 50.0) + 43;
//								y = (int) (Float.parseFloat(tk.nextToken()) * 50.0) + 43;
//								e.addControlPoint(x, y);
//							}
//						}
//					}
//					ln = dotIn.readLine();
//
//				}
//	
//			//System.out.println("----NO MORE READING DOT----");
//		}
//	}
