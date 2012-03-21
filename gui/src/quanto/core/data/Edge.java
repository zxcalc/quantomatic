package quanto.core.data;

/**
 * An edge
 *
 * @author alemer
 */
public class Edge extends GraphElement {
    private boolean directed;
	
	public Edge(String name, boolean directed) {
		super(name);
        this.directed = directed;
	}

    public void setDirected(boolean directed) {
        this.directed = directed;
    }

    public boolean isDirected() {
        return directed;
    }
}
