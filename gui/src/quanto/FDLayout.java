package quanto;

public class FDLayout implements GraphLayout {

	public void layout(Graph g) {
		float dx, dy, fx, fy, dist, accum_fx, accum_fy;
		for (int i=0;i<200;++i) {
			synchronized(g) {
				accum_fx = accum_fy = 0.0f;
				for (Vertex v1 : g.getVertices().values()) {
					fx = 0f; fy = 0f;
					for (Vertex v2 : g.getVertices().values()) {
						dx = v2.destX - v1.destX;
						dy = v2.destY - v1.destY;
						dist = dx*dx + dy*dy;
						if (dist == 0) {
							dist = 1;
						}
						//dist = (dist*dist);
						fx -= (dx*100.0f) / dist;
						fy -= (dy*100.0f) / dist;
					}
					
					float weight = (v1.edges.size()+1)*10.0f;
					for (Edge e : v1.edges) {
						if (e.source == v1) {
							dx = e.dest.destX - v1.destX;
							dy = e.dest.destY - v1.destY;
						} else {
							dx = e.source.destX - v1.destX;
							dy = e.source.destY - v1.destY;
						}
						
						fx += dx / weight;
						fy += dy / weight;
					}
					v1.destX += fx;
					v1.destY += fy;
					accum_fx += fx;
					accum_fy += fy;
				}
				
				if (g.getVertices().size()!=0) {
					accum_fx /= (float)g.getVertices().size();
					accum_fy /= (float)g.getVertices().size();
					for (Vertex v : g.getVertices().values()) {
						v.destX -= accum_fx;
						v.destY -= accum_fy;
					}
				}
			}
		}
	}
}
