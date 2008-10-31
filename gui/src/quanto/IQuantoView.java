package quanto;

public interface IQuantoView {
	void play();
	void pause();
	
	// currently implemented by PApplet in processing.
	void timesFont();
	void helveticaFont();
	void noStroke();
	void stroke(float r, float g, float b);
	void stroke(float k);
	void noFill();
	void fill(float r, float g, float b);
	void fill(float r, float g, float b, float a);
	void rectMode(int mode);
	
	// To use these drawing tools globally, you must specify a
	// coordinate system. For the default system, use
	// CoordinateSystem.IDENTITY.
	void rect(CoordinateSystem cs, float x, float y, float w, float h);
	void ellipse(CoordinateSystem cs, float x, float y, float r1, float r2);
	void bezier(CoordinateSystem cs, float x1, float y1, float cx1, float cy1, float x2, float y2, float cx2, float cy2);
	void text(CoordinateSystem cs, String txt, float x, float y);
	void text(CoordinateSystem cs, String txt, float x, float y, float w, float h);
}