class Edge {
  Vertex source, dest;
  public Edge(Vertex source, Vertex dest) {
    this.source = source;
    this.dest = dest;
  }
  
  public void display() {
    float dx = dest.x-source.x;
    float dy = dest.y-source.y;
    float len = sqrt(dx*dx+dy*dy);
    
    
    float offX = 8.0*(dx/len);
    float offY = 8.0*(dy/len);
    
    float theta = acos(dx / len);
    if (dy <= 0) {
      theta = (2*PI) - theta;
    }
    float p1x = dest.x-offX+5.0*cos(theta + 1.2*PI);
    float p1y = dest.y-offY+5.0*sin(theta + 1.2*PI);
    float p2x = dest.x-offX+5.0*cos(theta - 1.2*PI);
    float p2y = dest.y-offY+5.0*sin(theta - 1.2*PI);
    
    stroke(0);
    fill(0);
    
    line(source.x+offX, source.y+offY, dest.x-offX, dest.y-offY);
    triangle(dest.x-offX, dest.y-offY, p1x, p1y, p2x, p2y);
  }
}
