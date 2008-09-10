package quanto;

import java.awt.Menu;
import java.awt.MenuBar;
import java.awt.MenuItem;
import java.awt.MenuShortcut;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.StringTokenizer;
import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPopupMenu;
import javax.swing.JMenuItem;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.ButtonGroup;
import javax.swing.UIManager;
import processing.core.*;
import processing.pdf.*;
import processing.video.MovieMaker;
 
public class QuantoApplet extends PApplet {

	private static final long serialVersionUID = 1L;
	public static final int WIDTH = 800;
	public static final int HEIGHT = 600;
 
	PFont helvetica;
	PFont times;
	
	public static final int DEFAULT_SHADE_ALPHA = 100;
	public static final int DEFAULT_TRANSITION_TIME = 20;
	public static final int DEFAULT_HIGHLIGHT_TIME = 60;
	
	// global application modes
	public final GuiMode NORMAL = new NormalMode();
	public final GuiMode REWRITE_SELECT = new RewriteSelectMode();
	public final GuiMode REWRITE_HIGHLIGHT_LHS = new WaitMode(DEFAULT_HIGHLIGHT_TIME, new FadeUpToMode(REWRITE_SELECT), true);
	public final GuiMode REWRITE_HIGHLIGHT_RHS = new RewriteHighlightRhsMode();
	public final GuiMode NO_REWRITES_FOUND = new FadeUpToMode(new DisplayMessageMode("No Matching Rewrites Found", new FadeDownToMode()));
	
	public GuiMode mode = NORMAL;
	
	// how long should the app stay in the mode; numbers are number of 
	// iterations of the draw loop 
	
	int selectedIndex = -1;
	char tool;
	Graph graph;
	RewriteInstance currentRewrite;

	boolean paused;
	boolean draw_grid = false;
	boolean doSplines=false;
	int rectX=-1, rectY=-1;
	boolean dragging = false; // are we moving a vertex by dragging with mouse
	int oldmouseX = 0, oldmouseY = 0; // where the mouse was at the last drag message
	boolean shift=false;
	boolean snapToGrid = false;
	int layoutMode = 0;

	String outDirName = "";
	JFileChooser fileChooser;
	JPopupMenu vertexPopUp;
	//JMenuItem menuItem;
	ButtonGroup colourButtons;
	JRadioButtonMenuItem vRedMenuItem, vGreenMenuItem, vHMenuItem, vBndMenuItem;
	
	String nextPDFFileName = "";
	int nextPDFFile = 1;
	boolean saveNextFrame = false;
	
	String nextQTFileName = "";
	int nextQTFile = 1;
	boolean recordingVideo = false;
	MovieMaker mm;  // to be initialised when recording starts
 
	QuantoCore qcore;
	static QuantoApplet p; // the top level applet 

	public void setup() {
		p = this;
		paused = false;
		size(WIDTH, HEIGHT);
		
		smooth();
		frameRate(30);
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		fileChooser = new JFileChooser();
		vertexPopUp = new JPopupMenu();
		vRedMenuItem = new JRadioButtonMenuItem("Red");
		vGreenMenuItem = new JRadioButtonMenuItem("Green");
		colourButtons = new ButtonGroup();
		colourButtons.add(vRedMenuItem);
		colourButtons.add(vGreenMenuItem);
		vertexPopUp.add(vRedMenuItem);
		vertexPopUp.add(vGreenMenuItem);

		helvetica = loadFont("HelveticaNeue-14.vlw");
		times = loadFont("Times-Italic-14.vlw");

		graph = new Graph(new DotLayout());
		tool = 's';
 
		qcore = new QuantoCore(graph);
	}
	
	
	// these methods are the "application logic"
	private void startRewriteSelection() {
		currentRewrite = qcore.getRewritesForSelection();
		if (currentRewrite.total <= 0) {
			mode = NO_REWRITES_FOUND;
		}
		else {
			// the constant is the width of the arrow
			currentRewrite.layoutShiftedLhs(WIDTH/2 - 50, HEIGHT/2);
			currentRewrite.layoutShiftedRhs(WIDTH/2 + 50, HEIGHT/2);
			currentRewrite.highlightTargetVertices(graph);
			mode = REWRITE_HIGHLIGHT_LHS;
		}
	}
	
	private void acceptRewrite() {
		currentRewrite.unhighlightTargetVertices(graph);
		qcore.acceptRewriteForSelection();
		currentRewrite.highlightResultVertices(graph);
		mode = new FadeDownToMode(REWRITE_HIGHLIGHT_RHS);
	}
	
	private void nextRewrite() {
		currentRewrite.unhighlightTargetVertices(graph);
		currentRewrite = qcore.nextRewriteForSelection();
		// the constant is the width of the arrow
		currentRewrite.layoutShiftedLhs(WIDTH/2 - 50, HEIGHT/2);
		currentRewrite.layoutShiftedRhs(WIDTH/2 + 50, HEIGHT/2);
		currentRewrite.highlightTargetVertices(graph);
		mode = REWRITE_HIGHLIGHT_LHS;	
	}
	
	private void prevRewrite() {
		currentRewrite.unhighlightTargetVertices(graph);
		currentRewrite = qcore.prevRewriteForSelection();
		// the constant is the width of the arrow
		currentRewrite.layoutShiftedLhs(WIDTH/2 - 50, HEIGHT/2);
		currentRewrite.layoutShiftedRhs(WIDTH/2 + 50, HEIGHT/2);
		currentRewrite.highlightTargetVertices(graph);
		mode = REWRITE_HIGHLIGHT_LHS;	
	}
	
	private void abortRewrite() {
		currentRewrite.unhighlightTargetVertices(graph);
		qcore.abortRewrite();
		mode = new FadeDownToMode();
	}
	
	public void quit(){
		if(qcore != null) {
			qcore.closeQuantoBackEnd();
		}
		exit();
	}
	
	
	
	public void pause() {
		if (!paused) {
			noLoop();
			paused = true;
		} 
	}
	
	public void play() {
		if (paused) {
			loop();
			paused = false;
		}
	}
	
	public void mouseReleased() {
		mode.mouseReleased();
	}
	
	public void mouseDragged() {
		mode.mouseDragged();
	}
	
	public void mousePressed() {
		mode.mousePressed();
	}
	
	public void keyPressed() {
		mode.keyPressed();
	}
	
	public void keyReleased() {
		mode.keyReleased();
	}
	
	public void draw() {
		
		background(255);
		drawGrid();
		smooth();
		// if recording video 
		if (saveNextFrame) {
			doSaveNextFrame();
		}
		
		// this boolean records if anything has moved since the last iteration of
		// the run loop i.e. should we do another cycle or can we stop.
		boolean moved = mode.draw();
		
		// stop recording the frame now
		if(saveNextFrame) {
			stopSaveNextFrame();
		}
		if(recordingVideo) doRecordVideo();

		//	if we are going to save this frame, stop recording before the 
		// interface fluff has been drawn
		drawGUI();
		if (!moved) pause();
		
	}
	
	public void shadeScreen() {
		shadeScreen(DEFAULT_SHADE_ALPHA);
	}
	public void shadeScreen(int alpha) {
		// draw box covering view screen
		noStroke();
		fill(255,255,255,alpha);
		rect(0,0,WIDTH,HEIGHT);
	}

	public boolean drawGraph(Graph g) {
		boolean moved = false;
		synchronized(g) {
			for (Vertex v : g.getVertices().values()) {
				moved = moved || v.tick();
				v.tick();
				v.display();
			}
		}	
		synchronized(g) {
			if (moved) {
				for (Edge e : g.getEdges().values()) e.display(true);
			} else {
				for (Edge e : g.getEdges().values()) e.display(false);
			}
			// debug
			BoundingBox bb = g.getBoundingBox();
			rectMode(CORNER);
			noFill();
			stroke(200,0,255);
			rect(bb.ax,bb.ay, bb.getWidth(), bb.getHeight());
		}
		
		return moved;
	}

	private void drawGUI() {
		textFont(helvetica);
		textAlign(LEFT);
		fill(255, 0, 0);
		switch (tool) {
		case 's':
			text("SELECT", 10, 20);
			break;
		case 'm':
			text("MOVE", 10, 20);
			break;
		case 'e':
			text("EDGE", 10, 20);
			break;
		}
		
		fill(0, 0, 255);
		switch (layoutMode) {
		case 0:
			text("DOT",80,20);
			break;
		case 1:
			text("FD",80,20);
			break;
		case 2:
			text("JIGGLE",80,20);
			break;
		}
	}

	private void drawGrid() {
		if(draw_grid){
			stroke(240);
			for (int i=0;i<WIDTH;i+=Graph.GRID_X) line(i,0,i,HEIGHT);
			for (int i=0;i<HEIGHT;i+=Graph.GRID_Y) line(0,i,WIDTH,i);
		
			stroke(220);
			for (int i=0;i<WIDTH;i+=Graph.GRID_X*2) line(i,0,i,HEIGHT);
			for (int i=0;i<HEIGHT;i+=Graph.GRID_Y*2) line(0,i,WIDTH,i);
		}
	}
	
	private void drawArrow(int x, int y, int width, int height) {
		noStroke();
		fill(255,220,0);
		rectMode(CORNER);
		rect(x, y+15, width-25, height-30);
		triangle(x+width-25, y, x+width-25, y+height, x+width,y+height/2);
	}
	
	/* functions for doing PDF and QuickTime output */
	
	boolean chooseOutputDir() { 
		fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		fileChooser.setDialogTitle("Choose the output directory");
		fileChooser.setMultiSelectionEnabled(false);
		int retVal = fileChooser.showOpenDialog(this);
		if(retVal == JFileChooser.APPROVE_OPTION){
			outDirName = fileChooser.getSelectedFile().getAbsolutePath();
			return true;
		}
		else return false;
		
	}
		
	void startSaveNextFrame() {
		if(outDirName != "" || chooseOutputDir() ) {
			nextPDFFileName = outDirName + "/quanto-frame-" + (nextPDFFile++) + ".pdf";
			saveNextFrame = true;
		}
	}
	
	void doSaveNextFrame() {
		p.beginRecord(PConstants.PDF, nextPDFFileName);
	}
	
	void stopSaveNextFrame() {
		p.endRecord();
		saveNextFrame = false;	
	}

	void startRecordVideo() {
		if(outDirName != "" || chooseOutputDir() ) {
			nextQTFileName = outDirName + "/quanto-vid-" + (nextQTFile++) + ".mov";
			recordingVideo = true;
			mm = new MovieMaker(
					this,
					width,
					height,
					nextQTFileName,
					30,
					MovieMaker.MOTION_JPEG_A,
					MovieMaker.MEDIUM
				);
		}
	}
	
	void doRecordVideo() {
		mm.addFrame();
	}

	void stopRecordVideo() {
		recordingVideo = false;
		mm.finish();
		mm = null;
	}
	
	/*-------------------------------------------------
	 * Below this line are the gui modes which govern the behaviour and appearance 
	 * of the program. 
	 */
	
	/** the gui has modal behaviour - use subclasses of this guy to represent it.
	 * 
	 * @author rwd
	 *
	 */
	private class GuiMode {
		public boolean draw() {
			fill(0,0,0);
			rect(WIDTH/2 -100,HEIGHT/2 - 100, 200,200);
			return false;
		}
		/** sub classes should call back to this method in their default 
		 * case.
		 */
		public void keyPressed() {
			switch (key) {
			case 'Q': quit(); break;
			// "capture" the screen to PDF
			case 'c': startSaveNextFrame(); break;
			// v is for video
			case 'v': 
				if (recordingVideo) stopRecordVideo();
				else startRecordVideo();
				break;
			case CODED: 
				if (keyCode == SHIFT) { shift = true; }
				// else do nothing
				break;
			default:
				// do nothing
			}		
		}
		public void mousePressed() {
			// ignore mouse
		}
		public void mouseReleased() {
			// ignore mouse
		}
		public void mouseDragged() {
			// ignore mouse
		}
		public void keyReleased() {
			switch (key) {
			case CODED:
				switch (keyCode) {
				case SHIFT: shift = false;
					break;
				default: 
					break;
				}
			default:
				break;
			}
		}
	}
	
	/** NormalMode does what you think: the behaviour of the gui in normal
	 * graph editing.
	 * @author rwd
	 *
	 */
	private class NormalMode extends GuiMode {

		public void mouseReleased() {
			if (mouseButton == LEFT) {
				if (tool == 's') {
					synchronized (graph) {
						// if we are not holding shift deselect all vertices
						if (!shift && !dragging) {
							for (Vertex v : graph.getVertices().values())
								v.selected = false;
						}
						if (!dragging && (mouseX != rectX || mouseY != rectY)) {
							for (Vertex v : graph.getVertices().values()) {
								if (shift) {
									v.selected = v.selected
									| v
									.inRect(rectX, rectY, mouseX,
											mouseY);
								} else {
									v.selected = v.inRect(rectX, rectY, mouseX,
											mouseY);
								}
							}
						} else if (dragging
								&& (oldmouseX == mouseX && oldmouseY == mouseY)) {
							Vertex current = null; // this is vertex the mouse is
							// over
							for (Vertex v : graph.getVertices().values()) {
								if (v.at(mouseX, mouseY))
									current = v;
							}
							// reselect the clicked one
							if (current != null)
								current.selected = true; // this can be null in certain cases
						}
					}
				}
				rectX = -1;
				rectY = -1;
				dragging = false;
				play();
			}
			else if (mouseButton == RIGHT) {
				println("Right up");
				vertexPopUp.setVisible(false);                    
			}
			else {
				println("other mouse button up");
			}

		}

		public void mouseDragged() {
			int dx = mouseX - oldmouseX;
			int dy = mouseY - oldmouseY;
			if(dragging){
				synchronized (graph) {
					for (Vertex v : graph.getVertices().values()) {
						if (v.selected) {
							v.x += dx;
							v.y += dy;
							v.setDest(v.x, v.y); // prevent points from snapping
							// back
						}
					}
				}
			}
			oldmouseX = mouseX;
			oldmouseY = mouseY;
		}

		public void mousePressed() {

			if(mouseButton == LEFT) {
				oldmouseX = mouseX;
				oldmouseY = mouseY;

				switch (tool) {
				case 's':
					Vertex current = null; // this is vertex the mouse is over
					for(Vertex v: graph.getVertices().values()){
						// The expected behavior when clicking is to pick a single vertex
						// if many overlap. To get all of them, one drags a box around them.
						if(v.at(mouseX, mouseY)) current = v;
					}
					if(current==null) { 
						rectX = mouseX;
						rectY = mouseY;
					} else {
						if (!current.selected && !shift) {
							for (Vertex v : graph.getVertices().values()) v.selected = false;
						}
						current.selected = true;
						dragging = true;
					}
					break;

				case 'e':
					for (Vertex v : graph.getVertices().values()) {
						if (v.at(mouseX, mouseY)) {
							for (Vertex w : graph.getVertices().values()) {
								if (w.selected){ qcore.addEdge(w,v); }
							}
						}
					}
					break;
				}
				play();
			}
			else if (mouseButton == RIGHT){
				println("RIGHT down");
				vertexPopUp.setLocation(mouseX, mouseY);
				vertexPopUp.setVisible(true);                    
			}
			else {
				println("How many buttons does this mouse have?");
			}
		}

		public void keyPressed() {
			Clipboard cb;
			StringSelection data;

			switch (key) {
			case 'l': graph.layoutGraph(); break;
			case 'r': qcore.addVertex(QuantoCore.VERTEX_RED); break;
			case 'g': qcore.addVertex(QuantoCore.VERTEX_GREEN); break;
			case 'h': qcore.addVertex(QuantoCore.VERTEX_HADAMARD); break;
			case 'b': qcore.addVertex(QuantoCore.VERTEX_BOUNDARY); break;
			case 'n': qcore.newGraph(); break;
			case 'u': qcore.previousGraphState(); break;
			case 'd': qcore.deleteAllSelected(); break;
			case 'R': startRewriteSelection(); break;
			case 'p': doSplines = !doSplines; break;
			case 'y':
				layoutMode = (layoutMode+1)%3;
				switch (layoutMode) {
				case 0:
					graph.setLayoutEngine(new DotLayout());
					break;
				case 1:
					graph.setLayoutEngine(new FDLayout());
					break;
				case 2:
					graph.setLayoutEngine(new JiggleLayout());
					break;
				}
				graph.layoutGraph();
				break;
			case TAB:
				synchronized(graph) {
					if (graph.getVertices().size()>0) {
						Vertex sel = null;
						boolean pickNext = false;
						for (Vertex v : graph.getVertices().values()) {
							if (v.selected) {
								pickNext = true;
							} else if (pickNext) {
								sel = v;
								pickNext = false;
							}
							v.selected = false;
						}
						if (sel==null) sel = graph.getVertices().values().iterator().next();
						sel.selected = true;
					}
				}
				break;
			case 'G':
				snapToGrid = !snapToGrid;
				if (snapToGrid) graph.enableSnap();
				else graph.disableSnap();
				break;
			case 'x':
				cb = Toolkit.getDefaultToolkit().getSystemClipboard();
				data = new StringSelection(graph.toLatex());
				cb.setContents(data, data);
				break;
			case 't':
				cb = Toolkit.getDefaultToolkit().getSystemClipboard();
				data = new StringSelection(graph.toDot());
				cb.setContents(data, data);
				break;
			case 's':
			case 'e': /* these are tools that require mouse input too */
				tool = key;
				break;
			default: super.keyPressed();
			break;
			}
			graph.layoutGraph();
			play();
		}

		public boolean draw() {
			boolean moved = drawGraph(graph);
			if (rectX!=-1) {
				moved = true;
				fill(100,100,255,30);
				stroke(100,100,255);
				rectMode(CORNER);
				rect(rectX, rectY, mouseX-rectX, mouseY-rectY);
			}
			return moved;
		}
	}
	
	/** this class implements the behaviour for display a rewrite, and handling the 
	 * 4 possible choices of user action: Accept, Next, Prev, or Abort.
	 * @author rwd
	 *
	 */
	private class RewriteSelectMode extends GuiMode {

		public void keyPressed() {
			switch (key) {
			case ENTER:
			case RETURN:
				acceptRewrite(); play(); break;
			case 'k': 
				abortRewrite(); play(); break;
			case CODED:
				switch(keyCode) {
				case LEFT:  prevRewrite(); play(); break;
				case RIGHT: nextRewrite(); play(); break;
				case SHIFT: shift = false; play(); break;
				default: 
					// huh?
					break;
				}
			default: super.keyPressed(); break;
			}
		}
		
		public boolean draw() {
			
			boolean moved = drawGraph(graph);
			shadeScreen();
			drawArrow(WIDTH/2 -50, HEIGHT/2 -25, 100,50);	
			moved = drawGraph(currentRewrite.lhs) || moved;
			moved = drawGraph(currentRewrite.rhs) || moved;
			
			String msg = "Rewrite " + currentRewrite.index + " of " + currentRewrite.total + ": " 
				+ currentRewrite.ruleName;
			rectMode(CENTER);
			noStroke();
			fill(255,220,0,150);
			rect(WIDTH/2, HEIGHT/2 + 150, WIDTH, 30);
			textFont(helvetica);
			fill(255, 0, 0);
			textAlign(CENTER);
			text(msg,WIDTH/2,HEIGHT/2+150);
			return moved;
		}
	}
	
	/** This class draws a semi-transparent mask on top of the normal view
	 * adjusting the opacity each tick;  when the clock runs out it changes
	 * the mode.  For simple fades just override constructors to give different start and end values.
	 * Override the updateAlpha method to do anything mode interesting.
	 * @author rwd
	 *
	 */
	private class Fade_ToMode extends GuiMode {
		
		public int time;
		public int startAlpha;
		public int endAlpha;
		protected int currentAlpha; 
		private double stepAlpha;
		private int ticks;;

		public GuiMode next;
		
		public Fade_ToMode(GuiMode next, int time, int startAlpha, int endAlpha) {
			this.next = next;
			this.time = time;
			this.ticks = 0;
			this.startAlpha = startAlpha;
			this.endAlpha = endAlpha;
			this.stepAlpha = (endAlpha - startAlpha) / time;
		}
		public Fade_ToMode(GuiMode next, int time, int startAlpha) {
			this(next, time, startAlpha, 255);
		}
		public Fade_ToMode(GuiMode next, int time) {
			this(next, time, 0);
		}
		public Fade_ToMode(GuiMode next) {
			this(next, DEFAULT_TRANSITION_TIME);
		}
		public Fade_ToMode(){
			this(NORMAL);
		}
		public boolean draw() {
			drawGraph(graph);
			updateAlpha();
			shadeScreen(currentAlpha);
			
			if(++ticks > time) {
				mode = next;
			}
			return true;
		}
		protected void updateAlpha() {
			currentAlpha = startAlpha + (new Double(Math.ceil((ticks*stepAlpha)))).intValue();
		}
	}

	
	private class FadeUpToMode extends Fade_ToMode {	

		public FadeUpToMode(GuiMode next, int time, int startAlpha) {
			super(next, time, startAlpha, DEFAULT_SHADE_ALPHA);
		}
		public FadeUpToMode(GuiMode next, int time) {
			this(next, time, 0);
		}
		public FadeUpToMode(GuiMode next) {
			this(next, DEFAULT_TRANSITION_TIME);
		}
		public FadeUpToMode(){
			this(NORMAL);
		}
	}
	private class FadeDownToMode extends Fade_ToMode {

		public FadeDownToMode(GuiMode next, int time, int startAlpha) {
			super(next, time, startAlpha, 0);
		}
		public FadeDownToMode(GuiMode next, int time) {
			this(next, time, DEFAULT_SHADE_ALPHA);
		}
		public FadeDownToMode(GuiMode next) {
			this(next, DEFAULT_TRANSITION_TIME);
		}
		public FadeDownToMode(){
			this(NORMAL);
		}
	}
	
	private class DisplayMessageMode extends GuiMode {
		
		public GuiMode next;
		public String msg;
		
		public DisplayMessageMode(String msg, GuiMode next) {
			this.msg = msg;
			this.next = next;
		}
		public DisplayMessageMode(String msg) {
			this(msg, NORMAL);
		}
		
		public boolean draw() {
			boolean moved = drawGraph(graph);
			
			shadeScreen();
			textFont(helvetica);
			fill(255, 0, 0);
			textAlign(CENTER);
			text(msg,WIDTH/2,HEIGHT/2);
					
			return moved;
		}
		
		public void keyPressed() {
			mode = next;
			play();
			super.keyPressed();
		}
	}
	
	/** just waits specifed number of ticks before changing to the next mode
	 * 
	 * @author rwd
	 *
	 */
	private class WaitMode extends GuiMode {
		private int ticks;
		public int wait;
		public GuiMode next;
		public boolean force_play;
		
		public WaitMode(int wait, GuiMode next, boolean force_play) {
			this.ticks = 0;
			this.wait = wait;
			this.next = next;
			this.force_play = force_play;
		}
		public WaitMode(int wait, GuiMode next) {
			this(wait, next, false);
		}
		public WaitMode(int wait) {
			this(wait, NORMAL);
		}
		public WaitMode() {
			this(DEFAULT_HIGHLIGHT_TIME);
		}
		
		/** to add some extra behaviour when finished waiting this can be overridden
		 * 
		 */
		protected void onExit() {
			// override this
		}
		
		public boolean draw() {
			boolean moved = drawGraph(graph);
			ticks++;
			if(ticks > wait) {
				onExit();
				mode = next;
				play();
			}
			return moved || force_play;
		}
		
	}
	
	private class RewriteHighlightRhsMode extends WaitMode {
		protected void onExit() {
			currentRewrite.unhighlightResultVertices(graph);
		}
	}
}
