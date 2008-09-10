package quanto;

public class BoundingBox {
	/** pofloat a is the top left corner; pofloat b the bottom right
	 * 
	 */
	public float ax, ay, bx, by;
	
	public BoundingBox(float ax, float ay, float bx, float by) {
		if(ax < bx) {
			this.ax = ax;
			this.bx = bx;
		} else {
			this.ax = bx;
			this.bx = ax;
		}
		if(ay < by) {
			this.ay = ay;
			this.by = by;
		} else {
			this.ay = by;
			this.by = ay;
		}

	}
	
	public float getWidth() {
		return bx - ax;
	}
	public float getHeight() {
		return by - ay;
	}
	public float getCenterX() {
		return (ax+bx)/2;
	}
	public float getCenterY() {
		return (ay+by)/2;
	}
	public boolean containsPoint(float x, float y) {
		return (ax <= x) && (x <= bx) && (ay <= y) && (y <= by);
	}	
	public boolean collidesWith(BoundingBox that) {
		return (this.ax <= that.bx) && (this.ay <= that.by) && (that.ax <= this.bx) && (that.ay <= this.by);
	}
	/**  returns the X separation between two boxes.
	 * if negative then this is to the right of that.
	 * if the boxes overlap this function returns the distance required to *separate* them
	 * 
	 * @param that
	 * @return
	 */
	public float getXDistanceTo(BoundingBox that) {
		if(this.ax < that.ax) {
			return that.ax - this.ax - this.getWidth();
		}
		else {
			return that.ax - this.ax + that.getWidth(); 
		}
	}
	
	/**  returns the Y separation between two boxes.
	 * if negative then this is to the right of that.
	 * if the boxes overlap this function returns the distance required to *separate* them
	 * 
	 * @param that
	 * @return
	 */
	public float getYDistanceTo(BoundingBox that) {
		if(this.ay < that.ay) {
			return that.ay - this.ay - this.getWidth();
		}
		else {
			return that.ay - this.ay + that.getWidth(); 
		}
	}
}
