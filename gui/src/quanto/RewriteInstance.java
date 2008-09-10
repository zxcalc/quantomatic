package quanto;

public class RewriteInstance {
	/* A RewriteInstance is generated from a prior graph - it is completely 
	 * tied to this graph!  But you can recover the original from the properties
	 * of the RewriteInstance by removing the graph rhs from fusedGraph. 
	 */
	/* the only memeber guaranteed to defined is total;  if total <= 0 then 
	 * the other members will be null;  if total < 0 then the instance has not been 
	 * initialised.
	 */
	public int total = -1, index = -1;
	public String ruleName;
	public Graph fusedGraph, lhs, rhs;
	/* An important invariant is that the lhs and rhs are disjoint strict subgraphs
	 * of fusedGraph -- i.e. all their edge and vertex names occur in fusedGraph 
	 * in the right way.  Further all the edges of fusedGraph are either in the 
	 * original graph from which this rewrite instance was generated OR in rhs; hence
	 * all the names of lhs occur in the original graph too - this is how we know
	 * where the match of lhs in the original graph is.
	 */
	
	public void highlightTargetVertices(Graph g) {
		for(Vertex v : lhs.getVertices().values()) {
			g.getVertices().get(v.id).extra_highlight = true;
		}
	}
	
	public void unhighlightTargetVertices(Graph g) {
		for(Vertex v : lhs.getVertices().values()) {
			g.getVertices().get(v.id).extra_highlight = false;
		}
	}

	public void highlightResultVertices(Graph g) {
		for(Vertex v : rhs.getVertices().values()) {
			g.getVertices().get(v.id).extra_highlight = true;
		}
	}
	
	public void unhighlightResultVertices(Graph g) {
		for(Vertex v : rhs.getVertices().values()) {
			g.getVertices().get(v.id).extra_highlight = false;
		}
	}
	
	public void layoutShiftedLhs(float x, float y) {
		lhs.layoutGraph();	
		BoundingBox bb = lhs.getBoundingBoxAtDest();
		lhs.shift(x - bb.getCenterX() - bb.getWidth()/2, y - bb.getCenterY());
		
	}
	public void layoutShiftedRhs(float x, float y) {
		rhs.layoutGraph();
		BoundingBox bb = rhs.getBoundingBoxAtDest();
		rhs.shift(x - bb.getCenterX() + bb.getWidth()/2, y - bb.getCenterY());
		
	}
}
