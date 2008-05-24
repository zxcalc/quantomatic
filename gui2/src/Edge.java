class Edge extends PLib {
	Vertex source, dest;
	String id;

	public Edge(String name, Vertex source, Vertex dest) {
		this.source = source;
		this.dest = dest;
		this.id = name;
	}

	public void display() {
		QuantoApplet p = QuantoApplet.p; // instance of PApplet which has all processing tools

		float dx = dest.x - source.x;
		float dy = dest.y - source.y;
		float len = sqrt(dx * dx + dy * dy);

		float offX = 8.0f * (dx / len);
		float offY = 8.0f * (dy / len);

		float theta = acos(dx / len);
		if (dy <= 0) {
			theta = (2 * PI) - theta;
		}
		float p1x = dest.x - offX + 5.0f * cos(theta + 1.2f * PI);
		float p1y = dest.y - offY + 5.0f * sin(theta + 1.2f * PI);
		float p2x = dest.x - offX + 5.0f * cos(theta - 1.2f * PI);
		float p2y = dest.y - offY + 5.0f * sin(theta - 1.2f * PI);

		p.stroke(0);
		p.fill(0);

		p.line(source.x + offX, source.y + offY, dest.x - offX, dest.y - offY);
		p.triangle(dest.x - offX, dest.y - offY, p1x, p1y, p2x, p2y);
	}
}