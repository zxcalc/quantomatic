package quanto;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;

import javax.swing.JFileChooser;
import javax.swing.UIManager;

import processing.core.PApplet;
import processing.core.PConstants;
import processing.core.PFont;
import processing.video.MovieMaker;
 
public class QuantoApplet extends PApplet implements IQuantoView {

	private static final long serialVersionUID = 1L;
	public static final int WIDTH = 800;
	public static final int HEIGHT = 600;
	
	public static final int DEFAULT_SHADE_ALPHA = 100;
	public static final int DEFAULT_TRANSITION_TIME = 15;
	public static final int DEFAULT_HIGHLIGHT_TIME = 30;
	public static final char SELECT_TOOL = 's';
	public static final char SHIFT_SELECT_TOOL = 'S';
	public static final char EDGE_TOOL = 'e';
	public static final char GRAB_TOOL = 'g';
	
	
	// global application modes
	public final GuiMode NORMAL = new NormalMode();
	public final GuiMode REWRITE_SELECT = new RewriteSelectMode();
	public final GuiMode REWRITE_HIGHLIGHT_LHS = new WaitMode(DEFAULT_HIGHLIGHT_TIME, new FadeUpToMode(REWRITE_SELECT), true);
	public final GuiMode REWRITE_HIGHLIGHT_RHS = new RewriteHighlightRhsMode();
	public final GuiMode NO_REWRITES_FOUND = new FadeUpToMode(new DisplayMessageMode("No Matching Rewrites Found", new FadeDownToMode()));
	
	public GuiMode mode = NORMAL; 
	
	QuantoCore qcore;
	static IQuantoView p; // the top level applet 
	Graph graph;
	RewriteInstance currentRewrite;

	// these variables affect the drawing behaviour
	boolean paused;
	boolean draw_grid = false;
	boolean snapToGrid = false;
	int layoutMode = 0;
	Coord grabPos = new Coord(0,0);
	
	// variables affecting the controls
	int rectX=-1, rectY=-1;
	boolean draggingVertices = false; // are we moving a vertex by dragging with mouse
	int oldmouseX = 0, oldmouseY = 0; // where the mouse was at the last drag message
	boolean shift=false;
	boolean alt=false;
	boolean ctrl=false;
	int selectedIndex = -1;
	char tool;
	
	PFont helvetica;
	PFont times;

	String outDirName = "";
	JFileChooser fileChooser;
	
	String nextPDFFileName = "";
	int nextPDFFile = 1;
	boolean saveNextFrame = false;
	
	String nextQTFileName = "";
	int nextQTFile = 1;
	boolean recordingVideo = false;
	MovieMaker mm;  // to be initialised when recording starts

	public void helveticaFont() {
		this.textFont(times);
	}
	
	public void timesFont() {
		this.textFont(helvetica);
	}
	
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
			prepRewriteForDrawing();
			mode = REWRITE_HIGHLIGHT_LHS;
		}
	}
	
	private void acceptRewrite() {
		currentRewrite.unhighlightTargetVertices(graph);
		qcore.acceptRewriteForSelection();
		currentRewrite.highlightResultVertices(graph);
		currentRewrite.prepareResultVertices(graph);
		mode = new FadeDownToMode(REWRITE_HIGHLIGHT_RHS);
	}
	
	private void nextRewrite() {
		currentRewrite.unhighlightTargetVertices(graph);
		currentRewrite = qcore.nextRewriteForSelection();
		prepRewriteForDrawing();
		mode = REWRITE_HIGHLIGHT_LHS;	
	}
	
	private void prevRewrite() {
		currentRewrite.unhighlightTargetVertices(graph);
		currentRewrite = qcore.prevRewriteForSelection();
		prepRewriteForDrawing();
		mode = REWRITE_HIGHLIGHT_LHS;	
	}


	private void prepRewriteForDrawing() {
		// the constant is the width of the arrow
		currentRewrite.layoutShiftedLhs(WIDTH/2-60 , 2*HEIGHT/3, graph);
		currentRewrite.layoutShiftedRhs(WIDTH/2+60, 2*HEIGHT/3);
		currentRewrite.highlightTargetVertices(graph);
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
	

	/**
	 * Try to add an edge from all selected nodes to the node at (x,y)
	 **/
	public void tryAddEdgeAt(int x,int y) {
		for (Vertex v : graph.getVertices().values()) {
			if (v.at(x, y)) {
				for (Vertex w : graph.getVertices().values()) {
					if (w.selected){ qcore.addEdge(w,v); }
				}
			}
		}
	}
	
	 void updateModifierKey(boolean keyDown) {
		switch (keyCode) {
		case SHIFT: shift = keyDown; break;
		case ALT: alt = keyDown; break;
		case CONTROL: ctrl = keyDown; break;
		default:  // not a modifier
			break;
		}
		updateTool();
	}
	
	 private void updateTool() {
		 if(alt) {
			 tool = EDGE_TOOL;
		 }
		 else if (shift) {
			 tool = SHIFT_SELECT_TOOL;
		 }
		 else if (ctrl) {
			 tool = GRAB_TOOL;
		 }
		 else {
			 tool = SELECT_TOOL;
		 }
		 play();
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
		synchronized(g) {
			for (Vertex v : g.getVertices().values()) {
				moved = moved || v.tick();
				v.tick();
				v.display();
			}
		}
		return moved;
	}

	private void drawGUI() {
		textFont(helvetica);
		textAlign(LEFT);
		fill(255, 0, 0);
		switch (tool) {
		case SELECT_TOOL:
			text("SELECT", 10, 20);
			break;
		case SHIFT_SELECT_TOOL:
			text("SELECT+", 10, 20);
			break;
		case EDGE_TOOL:
			text("EDGE", 10, 20);
			break;
		case GRAB_TOOL:
			text("GRAB", 10, 20);
			fill(0,255,0);
			text(grabPos.x + " " + grabPos.y, 120,20);
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
	
	private void drawMessage(String msg) {
		rectMode(CENTER);
		noStroke();
		fill(255,220,0,150);
		rect(WIDTH/2, HEIGHT/2 + 150, WIDTH, 30);
		textFont(helvetica);
		fill(255, 0, 0);
		textAlign(CENTER);
		text(msg,WIDTH/2,HEIGHT/2+155);
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
		this.beginRecord(PConstants.PDF, nextPDFFileName);
	}
	
	void stopSaveNextFrame() {
		this.endRecord();
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
			case 'e': 
				
				break;
			// v is for video
			case 'v': 
				if (recordingVideo) stopRecordVideo();
				else startRecordVideo();
				break;
			case CODED: 
				updateModifierKey(true);
				break;
			default:
				// do nothing
			}		
		}
		public void keyReleased() {
			switch (key) {
			case CODED:
				updateModifierKey(false);
				break;
			default:
				break;
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
	}
	
	/** NormalMode does what you think: the behaviour of the gui in normal
	 * graph editing.
	 * @author rwd
	 *
	 */
	private class NormalMode extends GuiMode {

		public void mouseReleased() {
			if(mouseButton == LEFT) {
				if(draggingVertices) {
					// some weird case where you might get unwanted deselection.  I think.
					if(oldmouseX == mouseX && oldmouseY == mouseY) {
						Vertex v = graph.getVertexAtPoint(mouseX, mouseY);
						if(v != null) v.selected = true;
					}
				}
				else { // not dragging vertices
					switch (tool) {
					case SELECT_TOOL:
						graph.deselectAllVertices();
						// fall though to next case
					case SHIFT_SELECT_TOOL:
						if(mouseX != rectX || mouseY != rectY){ // dragging a rectangle
							for (Vertex v : graph.getVertices().values()) {
								if (v.inRect(rectX, rectY, mouseX, mouseY)) {
									v.selected = true;
								}
							}
						}
						else { // single click
							Vertex v = graph.getVertexAtPoint(mouseX, mouseY);
							if(v != null) v.selected = true;
						}
						break;
					case EDGE_TOOL: // do all work on mouse down
						break;
					default:
						break;
					}
				}
				// after mouse up reset mouse states
				rectX = -1;
				rectY = -1;
				draggingVertices = false;
			}
			else {
				// ignore non-left buttons
			}
			play();			
		}
		
		public void mouseReleased(boolean ignore) {
			if (mouseButton == LEFT) {
				synchronized (graph) {
					switch (tool) {
					case SELECT_TOOL:
						// deselect all vertices
						if (!draggingVertices) {
							for (Vertex v : graph.getVertices().values())
								v.selected = false;
						}
						if (!draggingVertices && (mouseX != rectX || mouseY != rectY)) {
							for (Vertex v : graph.getVertices().values()) {
								if (shift) {
									v.selected = v.selected | v.inRect(rectX, rectY, mouseX, mouseY);
								} else {
									v.selected = v.inRect(rectX, rectY, mouseX,	mouseY);
								}
							}
						} else if (draggingVertices	&& (oldmouseX == mouseX && oldmouseY == mouseY)) {
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
				draggingVertices = false;
				play();
			}
			// ignore non-left mousebuttons	
		}

		public void mouseDragged() {
			int dx = mouseX - oldmouseX;
			int dy = mouseY - oldmouseY;
			if(tool == GRAB_TOOL){
				grabPos = grabPos.plus(new Coord(dx,dy));
			}
			else {
				if(draggingVertices){
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
			}
			oldmouseX = mouseX;
			oldmouseY = mouseY;
			play();
		}

		public void mousePressed() {

			if(mouseButton == LEFT) {
				oldmouseX = mouseX;
				oldmouseY = mouseY;

				switch (tool) {
				case SELECT_TOOL:
				case SHIFT_SELECT_TOOL:
					Vertex current = graph.getVertexAtPoint(mouseX, mouseY);
					if(current==null) { // no vertex at mouse
						rectX = mouseX;
						rectY = mouseY;
					} else {
						if (!current.selected && (tool==SELECT_TOOL)) {
							graph.deselectAllVertices();
						}
						current.selected = true;
						draggingVertices = true;
					}
					break;

				case EDGE_TOOL:
					tryAddEdgeAt(mouseX,mouseY);
					break;
				}
				play();
			}
			// don't do anything with the right (or additional) MB 
		}

		public void keyPressed() {
			Clipboard cb;
			StringSelection data;

			switch (key) {
			case 'e': tryAddEdgeAt(mouseX,mouseY); break;
			case 'l': graph.layoutGraph(); break;
			case 'r': qcore.addVertex(QuantoCore.VERTEX_RED); break;
			case 'g': qcore.addVertex(QuantoCore.VERTEX_GREEN); break;
			case 'h': qcore.addVertex(QuantoCore.VERTEX_HADAMARD); break;
			case 'b': qcore.addVertex(QuantoCore.VERTEX_BOUNDARY); break;
			case 'n': qcore.newGraph(); grabPos = new Coord(0,0); break;
			case 'u': qcore.previousGraphState(); break;
			case 'd': qcore.deleteAllSelected(); break;
			case 'R': startRewriteSelection(); break;
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
			default: 
				super.keyPressed();
				break;
			}
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

		public static final int DEFAULT_HIGHLIGHT_TIME = 25;

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
			drawArrow(WIDTH/2 -50, 2*HEIGHT/3 -25, 100,50);	
			moved = drawGraph(currentRewrite.lhs) || moved;
			moved = drawGraph(currentRewrite.rhs) || moved;
			
			String msg = "Rewrite " + currentRewrite.index + " of " + currentRewrite.total + ": " 
				+ currentRewrite.ruleName;
			drawMessage(msg);
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
			drawMessage(msg);
			return moved;
		}
		
		public void keyPressed() {
			mode = next;
			play();
			super.keyPressed();
		}
		public void mousePressed() {
			keyPressed();
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

		public RewriteHighlightRhsMode(int wait, GuiMode next) {
			super(wait, next, true);
		}
		public RewriteHighlightRhsMode(int wait) {
			this(wait, NORMAL);
		}
		public RewriteHighlightRhsMode() {
			this(DEFAULT_HIGHLIGHT_TIME);
		}
		protected void onExit() {
			currentRewrite.unhighlightResultVertices(graph);
		}
	}
}
