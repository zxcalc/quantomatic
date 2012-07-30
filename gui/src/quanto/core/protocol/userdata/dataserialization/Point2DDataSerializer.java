package quanto.core.protocol.userdata.dataserialization;

import java.awt.geom.Point2D;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class Point2DDataSerializer implements DataSerializer<Point2D> {

    private final static Logger logger = Logger.getLogger("quanto.protocol.userdata.dataserialization");

    @Override
    public Point2D fromString(String data) {

	    StringTokenizer tk = new java.util.StringTokenizer(data, ":");
		int X = Integer.parseInt(tk.nextToken());
		int Y = Integer.parseInt(tk.nextToken());
		Point2D p = new Point2D.Double((double) X, (double) Y);

		return p;
    }

    @Override
    public String toString(Point2D data) {
        try {
			int X = (int) data.getX();
			int Y = (int) data.getY();
			return X + ":" + Y;
		} catch (NullPointerException e) {
            logger.log(Level.WARNING, "Could not serialize user data of type Point2D");
			return null;
		}
    }
}
