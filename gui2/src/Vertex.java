import java.lang.Math;

class Vertex extends PLib  {
	public float destX, destY, x, y;
	public float speed;
	public boolean selected;
	private int lastTick;
	public String id;
	public String col;
	public String angleexpr;
	QuantoApplet p; // instance of PApplet which has processing tools

	public Vertex(String id, int x, int y) {
		this.id = id;
		this.x = x;
		this.y = y;
		destX = x;
		destY = y;
		speed = 0.0005f;
		col = "red";
		lastTick = -1;
	}

	public void setDest(int x, int y) {
		destX = x;
		destY = y;
		lastTick = -1;
	}

	public void setColor(String col) {
		this.col = col;
	}

	public void setAngle(String expr) {
		this.angleexpr = expr;
	}

	public boolean tick() {
		if (lastTick == -1) {
			lastTick = millis();
			return true;
		}
		int thisTick = millis();
		float rate = (float)(thisTick - lastTick) * speed;
		if (rate>1) rate = 1;
		float dx = destX - x;
		float dy = destY - y;
		x += dx * rate;
		y += dy * rate;

		if (dx==0 && dy==0) {
			lastTick = -1;
			return false;
		} else return true;
	}

	private void displayRed() {
		QuantoApplet p = QuantoApplet.p; // instance of PApplet which has all processing tools

		p.stroke(255, 0, 0);
		p.fill(255, 100, 100);
		p.ellipse(x, y, 15, 15);
		if (angleexpr != null) {
			p.textFont(p.times);
			p.fill(100, 0, 0);
			p.text(angleexpr, x + 21, y + 6, 30, 10);
		}
	}

	private void displayGreen() {
		QuantoApplet p = QuantoApplet.p; // instance of PApplet which has all processing tools

		p.stroke(0, 255, 0);
		p.fill(100, 255, 100);
		p.ellipse(x, y, 15, 15);
		if (angleexpr != null) {
			p.textFont(p.times);
			p.fill(0, 100, 0, 255);
			p.text(angleexpr, x + 21, y + 6, 30, 10);
		}
	}

	private void displayBoundary() {
		QuantoApplet p = QuantoApplet.p; // instance of PApplet which has all processing tools

		p.stroke(0, 0, 0);
		p.fill(0, 0, 0, 255);
		p.ellipse(x, y, 3, 3);
	}

	private void displayH() {
		QuantoApplet p = QuantoApplet.p; // instance of PApplet which has all processing tools

		p.stroke(0, 0, 0);
		p.fill(255, 255, 0, 100);
		p.rectMode(CENTER);
		p.rect(x, y, 16, 16);
		p.textFont(p.helvetica);
		p.fill(0, 0, 0, 255);
		p.text("H", x - 5, y + 5);
	}

	public void display() {
		QuantoApplet p = QuantoApplet.p; // instance of PApplet which has all processing tools

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
			p.ellipse(x, y, 15, 15);
		}
		if (selected) {
			p.stroke(0, 0, 255);
			p.noFill();
			p.ellipse(x, y, 20, 20);
		}
	}

	public void highlight() {
		QuantoApplet p = QuantoApplet.p; // instance of PApplet which has all processing tools

		p.stroke(0, 255, 0);
		p.noFill();
		p.ellipse(x, y, 25, 25);
	}

	public void registerClick(int x, int y) {
		selected = at(x, y);
	}

	public boolean at(int x, int y) {
		if (Math.abs(x - this.x) < 8 && Math.abs(y - this.y) < 8)
			return true;
		else
			return false;
	}
}
