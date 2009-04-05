package quanto;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.StringTokenizer;

public class DotLayout implements GraphLayout {
	
	public static class DotException extends RuntimeException {
		private static final long serialVersionUID = 8173148319748759814L;

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
