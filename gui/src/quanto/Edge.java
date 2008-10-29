package quanto;
import java.util.ArrayList;
import java.awt.Point;
 
class Edge extends PLib {
	public static final float OFFSET_SIZE  = 14f;
	
	Vertex source, dest;
	String id;
	ArrayList<Point> controlPoints;
	
	private float midPointOffset;

	public Edge(String name, Vertex source, Vertex dest) {
		this.source = source;
		this.dest = dest;
		this.midPointOffset = 0f;
		source.addEdge(this);
		dest.addEdge(this);
		this.id = name;
		controlPoints = new ArrayList<Point>();
	}
	
	public void setOffset(int offset) {
		this.midPointOffset = offset*OFFSET_SIZE;
	}
	
	public void reverse() {
		Vertex temp = source;
		source = dest;
		dest = temp;
	}

	
	public void display() {
		display(false);
	}

	public void display(boolean inMotion) {
		IQuantoView p = QuantoApplet.p; // instance of PApplet which has all processing tools

		if (inMotion) {
			p.stroke(120,120,200);
		} else {
			p.stroke(0);
		}
		p.noFill();

//      float  dx, dy, len, offX, offY;
//		dx = dest.x - source.x;
//		dy = dest.y - source.y;
//		len = sqrt(dx * dx + dy * dy);
//		offX = 8.0f * (dx / len);
//		offY = 8.0f * (dy / len);
//p.line(source.x + offX, source.y + offY, dest.x - offX, dest.y - offY);
		
		Coord start, end, cp1, cp2;
		if(source == dest){			
			start = new Coord(source.x, source.y);
			end = new Coord(source.x + 0.4f*midPointOffset+OFFSET_SIZE, source.y);
			
			cp1 = start.plus(new Coord(0,0.4f*midPointOffset));
			cp2 = end.plus(new Coord(0,0.4f*midPointOffset));
			p.bezier(start.x,start.y, cp1.x,cp1.y, cp2.x,cp2.y,end.x,end.y);
			
			cp1 = end.plus(new Coord(0,-0.4f*midPointOffset));
			cp2 = start.plus(new Coord(0,-0.4f*midPointOffset));
			p.bezier(end.x,end.y, cp1.x,cp1.y, cp2.x,cp2.y,start.x,start.y);
		}
		else {
			start = new Coord(source.x, source.y);
			end = new Coord(dest.x, dest.y);
			Coord dir = end.minus(start);
			cp1 = start.plus(dir.getPerpUnit().rescale(this.midPointOffset));
			cp2 = end.plus(dir.getPerpUnit().rescale(this.midPointOffset));
			p.bezier(start.x,start.y, cp1.x,cp1.y, cp2.x,cp2.y,end.x,end.y);			
		}
	}



}