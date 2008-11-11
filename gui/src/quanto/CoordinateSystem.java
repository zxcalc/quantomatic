package quanto;
import Jama.Matrix;

public class CoordinateSystem extends PLib {
	public static final CoordinateSystem IDENTITY =
		new CoordinateSystem();
	protected Matrix matrix;
	protected Matrix destMatrix;
	boolean interp;
	int lastTick;
	
	public CoordinateSystem() {
		matrix = new Matrix(new double[][]{
				{1,0,0},
				{0,1,0},
				{0,0,1}
		});
		destMatrix = null;
		lastTick = -1;
	}
	
	// make a copy
	public CoordinateSystem(CoordinateSystem coordinateSystem) {
		Matrix d = coordinateSystem.destMatrix;
		if (d != null) matrix = (Matrix)d.clone();
		else matrix = (Matrix)coordinateSystem.matrix.clone();
		
		destMatrix = null;
		lastTick = -1;
	}
	
	public void print() {
		this.matrix.print(4,2);
	}
	
	public void interpolateTo(CoordinateSystem dest) {
		lastTick = millis();
		destMatrix = dest.matrix;
		interp = true;
	}
	
	public boolean tick() {
		if (destMatrix != null) {
			Matrix dA = destMatrix.minus(matrix);
			if (dA.normF() <= 0.01f) {
				destMatrix = null;
				return false;
			} else {
				int thisTick = millis();
				float rate = (float)(thisTick - lastTick) * 0.0005f;
				matrix = matrix.plus(dA.times(rate));
				return true;
			}
		}
		return false;
	}
	

	public CoordinateSystem shift(float x, float y) {
		Matrix m = prepareMutation();
		m.setMatrix(0, 2, 0, 2, new Matrix(new double[][] {
				{1,0,x},
				{0,1,y},
				{0,0,1}
		}).times(m));
		return this;
	}
	
	public CoordinateSystem scale(float x, float y) {
		Matrix m = prepareMutation();
		m.setMatrix(0, 2, 0, 2, new Matrix(new double[][] {
				{x,0,0},
				{0,y,0},
				{0,0,1}
		}).times(m));
		return this;
	}
	
	public CoordinateSystem shear(float x, float y) {
		Matrix m = prepareMutation();
		m.setMatrix(0, 2, 0, 2, new Matrix(new double[][] {
				{1+x*y, -1*x,  0},
				{ -1*y,    1,  0},
				{    0,    0,  1}
		}).times(m));
		return this;
	}
	
	public CoordinateSystem rotate(float th) {
		Matrix m = prepareMutation();
		m.setMatrix(0, 2, 0, 2, new Matrix(new double[][] {
				{Math.cos(th), -1*Math.sin(th),  0},
				{Math.sin(th),    Math.cos(th),  0},
				{           0,               0,  1}
		}).times(this.matrix));
		return this;
	}
	
	private Matrix prepareMutation() {
		// IDENTITY should never be mutated
		assert (this != CoordinateSystem.IDENTITY);
		return (destMatrix == null) ? matrix : destMatrix;
	}
	
	// use this for drawing coordinates
	public Coord fromGlobal(float x, float y) {
		//print();
		Matrix c = this.matrix.times(
				new Matrix(new double[]{x,y,1},3));
		return new Coord((float)c.get(0,0), (float)c.get(1, 0));
	}
	
	// throw away the translation component for lengths
	public Coord lengthFromGlobal(float x, float y) {
		Matrix c = this.matrix.getMatrix(0, 1, 0, 1).times(
				new Matrix(new double[]{x,y},2));
		return new Coord((float)c.get(0,0), (float)c.get(1, 0));
	}
	
	// use this for input coordinates
	public Coord toGlobal(float x, float y) {
		Matrix c = this.matrix.inverse().times(
				new Matrix(new double[]{x,y,1},3));
		return new Coord((float)c.get(0,0), (float)c.get(1, 0));
	}
	
	// throw away the translation component for lengths
	public Coord lengthToGlobal(float x, float y) {
		Matrix c = this.matrix.getMatrix(0, 1, 0, 1).inverse().times(
				new Matrix(new double[]{x,y},2));
		return new Coord((float)c.get(0,0), (float)c.get(1, 0));
	}
}
