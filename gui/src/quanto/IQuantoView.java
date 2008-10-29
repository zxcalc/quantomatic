package quanto;

public interface IQuantoView {
	void play();
	void pause();
	void timesFont();
	void helveticaFont();
	void noStroke();
	void stroke(float r, float g, float b);
	void stroke(int k);
	void noFill();
	void fill(float r, float g, float b);
	void fill(float r, float g, float b, float a);
	void rect(float x, float y, float w, float h);
	void rectMode(int mode);
	void ellipse(float x, float y, float r1, float r2);
	void bezier(float x1, float y1, float cx1, float cy1, float x2, float y2, float cx2, float xy2);
	void text(String txt, float x, float y);
	void text(String txt, float x, float y, float w, float h);
}