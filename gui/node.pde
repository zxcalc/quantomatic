class Node {
  public float destX, destY, x, y;
  public float speed;
  public boolean selected;
  private float lastTick;
  public String id;
  public String col;
  public Node(String id, int x, int y) {
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
  
  public void tick() {
    float thisTick = (float)millis();
    float rate = 100.0/(thisTick-lastTick);
    float dx = speed*(destX-x);
    float dy = speed*(destY-y);
    x += (dx)/rate;
    y += (dy)/rate;
    
    lastTick = (float)millis();
  }
  
  public void display() {
    if (col.equals("red")) {
      stroke(255,0,0);
      fill(255,0,0,100);
    } else if (col.equals("green")) {
      stroke(0,150,0);
      fill(0,150,0,100);
    } else if (col.equals("yellow")) {
      stroke(255,255,0,255);
      fill(255,255,0,150);
    } else if (col.equals("blue")) {
      stroke(0,0,255,255);
      fill(0,0,255,150);
    } else {
      stroke(0);
      fill(50, 50, 50, 150);
    }
    ellipse(x,y, 15, 15);
    
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
