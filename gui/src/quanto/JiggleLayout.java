package quanto;
import java.util.Map;
import java.util.HashMap;


public class JiggleLayout implements GraphLayout {
	class JiggleWrapper extends jiggle.Graph {
		protected quanto.Graph graph;
		protected Map<jiggle.Vertex,quanto.Vertex> vertexMap;
		public JiggleWrapper(quanto.Graph g) {
			graph = g;
			vertexMap = new HashMap<jiggle.Vertex,quanto.Vertex>();
			
			// keep an inverse mapping from quanto.Vertex's to jiggle.Vertex's.
			Map<quanto.Vertex, jiggle.Vertex> inverseVertexMap =
				new HashMap<quanto.Vertex, jiggle.Vertex>();
			synchronized (g) {
				jiggle.Vertex jv;
				for (quanto.Vertex v : g.getVertices().values()) {
					jv = insertVertex();
					jv.setCoords(new double[]{(double)v.x, (double)v.y});
					inverseVertexMap.put(v, jv);
					vertexMap.put(jv, v);
				}
			}
			
			synchronized(g) {
				for (quanto.Edge e : g.getEdges().values()) {
					insertEdge(inverseVertexMap.get(e.source),inverseVertexMap.get(e.dest), true);
				}
			}
		}
		
		// call setDest() on all the vertices of the wrapped Graph.
		public void syncVertices() {
			quanto.Vertex qv;
			for (jiggle.Vertex v : vertices) {
				qv = vertexMap.get(v);
				if (qv != null)
					qv.setDest((float)v.getCoords()[0],
							   (float)v.getCoords()[1]);
			}
		}
	}
	
	public void layout(quanto.Graph g) {
		JiggleWrapper wrapper = new JiggleWrapper(g);
		double k = 30.0d;
		
		jiggle.ForceModel fm = new jiggle.ForceModel(wrapper);
		jiggle.VertexVertexRepulsionLaw repulsion =
			new jiggle.HybridVertexVertexRepulsionLaw(wrapper, k);
		jiggle.SpringLaw spring = new jiggle.LinearSpringLaw(wrapper, k);
		
		fm.addForceLaw(repulsion);
		fm.addForceLaw(spring);
		
		fm.addConstraint(new jiggle.SurfaceOfSphereConstraint(wrapper));
		
		/*
		jiggle.FirstOrderOptimizationProcedure opt =
			new jiggle.SteepestDescent (wrapper, fm, 0.0000001d);
		*/
		jiggle.FirstOrderOptimizationProcedure opt =
			new jiggle.ConjugateGradients(wrapper, fm, 0.0000001d, 5);
		
		
		for (int i=0;i<150;++i) opt.improveGraph();
		
		wrapper.syncVertices();
	}

}
