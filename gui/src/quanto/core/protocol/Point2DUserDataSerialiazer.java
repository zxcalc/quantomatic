package quanto.core.protocol;

import java.awt.geom.Point2D;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import quanto.core.CoreException;
import quanto.core.data.CoreGraph;

public class Point2DUserDataSerialiazer implements UserDataSerializer<Point2D> {

	private final static Logger logger = Logger.getLogger("quanto.protocol");
	public String dataTag = "quanto-gui:position";
	
	public Point2DUserDataSerialiazer() {}
	
	public String dataToString(Point2D p) {
		try {
			int X = (int) p.getX();
			int Y = (int) p.getY();
			return X + ":" + Y;
		} catch (NullPointerException e) {
			/*Should never happen as a vertex
			 * as a vertex always has a position..
			 */
			return null;
		}
	}
	
	public Point2D stringToData(String s) {
		StringTokenizer tk = new java.util.StringTokenizer(s, ":");
		int X = Integer.parseInt(tk.nextToken());
		int Y = Integer.parseInt(tk.nextToken());
		Point2D p = new Point2D.Double((double) X, (double) Y);
		
		return p;
	}

	public void setVertexUserData(ProtocolManager talker, CoreGraph g, String vertexName, Point2D data){
		String dataString = dataToString(data);
		if (dataString == null) return;
		try {
			talker.setVertexUserData(g.getCoreName(), vertexName, 
					this.dataTag, dataString);
		} catch (CoreException e) {
			logger.log(Level.FINE, "Could not set position on vertex " 
									+ vertexName, e);
		}
	}
	
	public Point2D getVertexUserData(ProtocolManager talker, CoreGraph g, String vertexName) {
		Point2D p;
		try {
			p = stringToData(talker.vertexUserData(g.getCoreName(), vertexName,
					this.dataTag));
		} catch (CoreException e) {
			logger.log(Level.FINE, "Could not get position on vertex " 
					+ vertexName, e);
			return null;
		}
		
		return p;
	}
	/* Irrelevant for this type */
	public Point2D getGraphUserData(ProtocolManager talker, CoreGraph g) {return null;}
	public void setGraphUserData(ProtocolManager talker, CoreGraph g, Point2D data) {}
}
