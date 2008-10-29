package quanto;
import java.lang.Math;
import java.util.ArrayList;

class Vertex extends PLib implements Comparable<Vertex>  {
	
	public static float radius = 15f;
	
	public float destX, destY, x, y;
	public float speed;
	public boolean selected;
	private int lastTick;
	public String id;
	public String col;
	public String angleexpr;
	public ArrayList<Edge> edges;
	public boolean snap;
	public boolean extra_highlight = false;
	public int flash_seq = 0;
	
	IQuantoView p; // instance of PApplet which has processing tools

	public Vertex(String id, int x, int y) {
		this.id = id;
		this.x = x;
		this.y = y;
		destX = x;
		destY = y;
		speed = 0.0005f;
		col = "red";
		lastTick = -1;
		QuantoApplet.p.play();
		edges = new ArrayList<Edge>();
		snap = false;
	}
	
	public void addEdge(Edge e) {
		edges.add(e);
	}

	
	
	public void setDest(float x, float y) {
		if (snap) {
			destX = (Graph.GRID_X * round(x/(float)Graph.GRID_X));
			destY = (Graph.GRID_Y * round(y/(float)Graph.GRID_Y));
		} else {
			destX = x;
			destY = y;
		}
		lastTick = -1;
		QuantoApplet.p.play();
	}
	
	public void shift(float dx, float dy) {
		setDest(destX + dx, destY + dy);
	}

	public void setColor(String col) {
		this.col = col;
	}

	public void setAngle(String expr) {
		this.angleexpr = expr;
	}

	public boolean tick() {
		if (lastTick == -1) lastTick = millis();
		
		int thisTick = millis();
		float rate = (float)(thisTick - lastTick) * speed;
		if (rate>1) rate = 1;
		float dx = destX - x;
		float dy = destY - y;

		if (floor(dx)==0 && floor(dy)==0) {
			lastTick = -1;
			x = destX;
			y = destY;
			return false;
		} else {
			x += dx * rate;
			y += dy * rate;
			return true;
		}
	}

	private void displayRed() {
		IQuantoView p = QuantoApplet.p; // instance of PApplet which has all processing tools

		p.stroke(255, 0, 0);
		p.fill(255, 100, 100);
		p.ellipse(x, y, radius, radius);
		if (angleexpr != null) {
			p.timesFont();
			p.fill(100, 0, 0);
			p.text(angleexpr, x + 21, y + 6, 30, 10);
		}
	}

	private void displayGreen() {
		IQuantoView p = QuantoApplet.p; // instance of PApplet which has all processing tools

		p.stroke(0, 255, 0);
		p.fill(100, 255, 100);
		p.ellipse(x, y, radius, radius);
		if (angleexpr != null) {
			p.timesFont();
			p.fill(0, 100, 0);
			p.text(angleexpr, x + 21, y + 6, 30, 10);
		}
	}

	private void displayBoundary() {
		IQuantoView p = QuantoApplet.p; // instance of PApplet which has all processing tools

		p.stroke(0, 0, 0);
		p.fill(0, 0, 0, 255);
		p.ellipse(x, y, 3, 3);
	}

	private void displayH() {
		IQuantoView p = QuantoApplet.p; // instance of PApplet which has all processing tools

		p.stroke(0, 0, 0);
		p.fill(255, 255, 0, 100);
		p.rectMode(CENTER);
		p.rect(x, y, 1.1f*radius, 1.1f*radius);
		p.timesFont();
		p.fill(0, 0, 0, 255);
		p.text("H", x - 5, y + 5);
	}
	
	public boolean inRect(float x1, float y1, float x2, float y2) {
		boolean inX = (x1<=x && x2>=x) || (x1>=x && x2<=x);
		boolean inY = (y1<=y && y2>=y) || (y1>=y && y2<=y);
		return inX && inY;
	}

	public void display() {
		IQuantoView p = QuantoApplet.p; // instance of PApplet which has all processing tools

		if(extra_highlight) {
			p.noStroke();
			p.fill(255, 100 + 10*((flash_seq++)%15), 0, 150);
			p.ellipse(x, y, 1.67f*radius, 1.67f*radius);
		}
		
		if (col.equals("red")) {
			displayRed();
		} else if (col.equals("green")) {
			displayGreen();
		} else if (col.equals("H")) {
			displayH();
		} else if (col.equals("boundary")) {
			displayBoundary();
		} else {
			p.stroke(0);
			p.fill(50, 50, 50, 150);
			p.ellipse(x, y, radius, radius);
		}
		if (selected) {
			p.stroke(0, 0, 255);
			p.noFill();
			p.ellipse(x, y, 1.33f*radius, 1.33f*radius);
		}
	}

	public void highlight() {
		IQuantoView p = QuantoApplet.p; // instance of PApplet which has all processing tools

		p.stroke(0, 255, 0);
		p.noFill();
		p.ellipse(x, y, 1.67f*radius, 1.67f*radius);
	}

	public void registerClick(int x, int y) {
		selected = at(x, y);
	}
	
	public boolean at(float x, float y) {
		if (Math.abs(x - this.x) < 8 && Math.abs(y - this.y) < 8)
			return true;
		else
			return false;
	}
	
	// a lexicographic order on y, x (with a minor fudge factor)
	public int compareTo(Vertex v) {
		if (v.x==x && v.y==y) return 0;
		else if (abs(v.y-y)<4) return (v.x<x) ? 1 : -1;
		else return (v.y<y) ? -1 : 1;
	}
}
