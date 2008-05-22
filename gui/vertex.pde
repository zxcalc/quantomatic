class Vertex {
    public float destX, destY, x, y;
    public float speed;
    public boolean selected;
    private float lastTick;
    public String id;
    public String col;
    public String angleexpr;
    public Vertex(String id, int x, int y) {
	this.id = id;
	this.x = x;
	this.y = y;
	destX = x;
	destY = y;
	speed = 0.5;
	col = "red";
	lastTick = (float)millis();
    }
  
    public void setDest(int x, int y) {
	destX = x;
	destY = y;
    }
  
    public void setColor(String col) {
	this.col = col;
    }

    public void setAngle(String expr) {
	this.angleexpr = expr;
    }

  
    public void tick() {
	float thisTick = (float)millis();
	float rate = 100.0/(thisTick-lastTick);
	float dx = speed*(destX-x);
	float dy = speed*(destY-y);
	x += (dx)/rate;
	y += (dy)/rate;
    
	lastTick = (float)millis();
    }

    private void displayRed() {
	stroke(255,0,0);
	fill(255,0,0,100);
	ellipse(x,y, 15, 15);
	if(angleexpr!=null) {
	    textFont(times);
	    fill(100,0,0,255);
	    text(angleexpr,x+21,y+6,30,10);
	}
    }

    private void displayGreen() {
	stroke(0,255,0);
	fill(0,255,0,100);
	ellipse(x,y, 15, 15);
	if(angleexpr!=null) {
	    textFont(times);
	    fill(0,100,0,255);
	    text(angleexpr,x+21,y+6,30,10);
	}
    }
    
    private void displayBoundary() {
	stroke(0,0,0);
	fill(0,0,0,255);
	ellipse(x,y, 3, 3);
    }
    
    private void displayH() {
	stroke(0,0,0);
	fill(255,255,0,100);
	rectMode(CENTER);
	rect(x,y, 16, 16);
	textFont(helvetica);
        fill(0,0,0,255);
	text("H",x-5,y+5);
	
    }
    
    public void display() {
	if (col.equals("red")) {
	    displayRed();
	} else if (col.equals("green")) {
	    displayGreen();
	} else if (col.equals("H")) {
	    displayH();
	} else if (col.equals("boundary")) {
	    displayBoundary();
	} else {
	    stroke(0);
	    fill(50, 50, 50, 150);
	    ellipse(x,y, 15, 15);
	}	
	if (selected) {
	    stroke(0, 0, 255);
	    noFill();
	    ellipse(x, y, 20, 20);
	}
    }
  
    public void highlight() {
	stroke(0, 255, 0);
	noFill();
	ellipse(x, y, 25, 25);
    }
  
    public void registerClick(int x, int y) {
	selected = at(x,y);
    }
  
    public boolean at(int x, int y) {
	if (abs(x-this.x)<8 && abs(y-this.y)<8) return true;
	else return false;
    }
}
