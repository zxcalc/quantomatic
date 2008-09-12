package quanto;

public class Coord implements Comparable<Coord>{

	public float x;
	public float y;
	
	public Coord(float x, float y) {
		this.x = x;
		this.y = y;
	}
	public Coord plus(Coord that) {
		return new Coord (this.x + that.x, this.y + that.y);
	}
	public Coord rescale(float a) {
		return new Coord(a*x, a*y);
	}
	public Coord minus(Coord that) {
		return new Coord(this.x -that.x, this.y - that.y);
	}
	public float getLength() {
		return (float)Math.sqrt(x*x + y*y);
	}
	public float getDistanceTo(Coord that) {
		return that.minus(this).getLength();
	}
	public Coord getPerpUnit() {
		return new Coord(-y/getLength(), x/getLength());
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + Float.floatToIntBits(x);
		result = prime * result + Float.floatToIntBits(y);
		return result;
	}
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final Coord other = (Coord) obj;
		if (Float.floatToIntBits(x) != Float.floatToIntBits(other.x))
			return false;
		if (Float.floatToIntBits(y) != Float.floatToIntBits(other.y))
			return false;
		return true;
	}
	
	public int compareTo(Coord o) {
		if (x < o.x)
			return -1;
		else if(x > o.x)
			return 1;
		else if(y < o.y)
			return -1;
		else if (y > o.x)
			return 1;
		else
			return 0;
	}
}
