package quanto;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.StringTokenizer;
import javax.swing.JFileChooser;
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

	int selectedIndex = -1;
	char tool;
	Graph graph;
	boolean paused;
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

	QuantoBack backend;
	XMLReader xml;
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

		backend = new QuantoBack();
		xml = new XMLReader();	
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
		if (mouseButton == LEFT) {
			if (tool == 's') {
				// if we are not holding shift deselect all vertices
				if (!shift && !dragging) {
					for (Vertex v : graph.vertexList)
						v.selected = false;
				}
				if (!dragging && (mouseX != rectX || mouseY != rectY)) {
					for (Vertex v : graph.vertexList) {
						if (shift) {
							v.selected = v.selected	| v.inRect(rectX, rectY, mouseX, mouseY);
						} else {
							v.selected = v.inRect(rectX, rectY, mouseX, mouseY);
						}
					}
				} else if (dragging
						&& (oldmouseX == mouseX && oldmouseY == mouseY)) {
					Vertex current = null; // this is vertex the mouse is over
					for (Vertex v : graph.vertexList) {
						if (v.at(mouseX, mouseY))
							current = v;
					}
					// reselect the clicked one
					if (current!=null) current.selected = true; // this can be null in certain cases
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
			for (Vertex v : graph.vertexList) {
				if (v.selected) {
					v.x += dx;
					v.y += dy;
					v.setDest(v.x,v.y); // prevent points from snapping back
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
				for(Vertex v: graph.vertexList){
					// The expected behavior when clicking is to pick a single vertex
					// if many overlap. To get all of them, one drags a box around them.
					if(v.at(mouseX, mouseY)) current = v;
				}
				if(current==null) { 
					rectX = mouseX;
					rectY = mouseY;
				} else {
					if (!current.selected && !shift) {
						for (Vertex v : graph.vertexList) v.selected = false;
					}
					current.selected = true;
					dragging = true;
				}
				break;
			case 'm':
				float xAccum=0, yAccum=0;
				int vCount=0;
				for (Vertex v : graph.vertexList) {
					if (v.selected) {
						vCount++;
						xAccum += v.x;
						yAccum += v.y;
					}
				}
				if (vCount > 0) {
					xAccum /= (float)vCount;
					yAccum /= (float)vCount;
					for (Vertex v : graph.vertexList) {
						if (v.selected) {
							v.clearEdgeControlPoints();
							v.setDest((int)(v.x+mouseX-xAccum), (int)(v.y+mouseY-yAccum));
						}
					}
				}
				break;
			case 'e':
				for (Vertex v : graph.vertexList) {
					if (v.at(mouseX, mouseY)) {
						for (Vertex w : graph.vertexList) {
							if (w.selected)
								modifyGraph("e " + w.id + " " + v.id + "\n");
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
		if (key == tool) {
			tool = 's';
			play();
			return;
		}
		
		Clipboard cb;
		StringSelection data;
		switch (key) {
		case 'l':
			graph.layoutGraph();
			break;
		case 'r':
		case 'g':
		case 'h':
		case 'b': /* add new nodes */
			modifyGraph(Character.toString(key));
			if (graph.newestVertex!=null) {
				Vertex w = graph.newestVertex;
				for (Vertex v : graph.vertexList) {
					if (v.selected) modifyGraph("e " + v.id + " " + w.id + "\n");
				}
			}
			break;
		case 'n':
		case 'u': /* other back-end commands, just pass them on */
			modifyGraph(key + "");
			break;
		case 'd':
			for (Vertex v : graph.vertexList) {
				if (v.selected) modifyGraph("d " + v.id);
			}
			break;
		case 'q':
			println("Shutting down quantoML");
			backend.send("Q\n");
			//println(backend.receive());
			//backend.send("quit () ; \n");
			println("Quitting....");
			exit();
			break;
		case 'p':
			doSplines = !doSplines;
			break;
		case 'c':  // "capture" the screen to PDF
			startSaveNextFrame();
			break;		
		case 'v': // v is for video
			if (recordingVideo) stopRecordVideo();
			else startRecordVideo();
			break;
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
		case CODED:
			if (keyCode == SHIFT) shift = true;
			break;
		case TAB:
			synchronized(graph.vertexList) {
				if (graph.vertexList.size()>0) {
					graph.sortVertices();
					Vertex sel = null;
					boolean pickNext = false;
					for (Vertex v : graph.vertexList) {
						if (v.selected) {
							pickNext = true;
						} else if (pickNext) {
							sel = v;
							pickNext = false;
						}
						v.selected = false;
					}
					if (sel==null) sel = graph.vertexList.get(0);
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
		case 'm':
		case 'e': /* these are tools that require mouse input too */
			tool = key;
			break;
		}
		
		play();
	}
	
	public void keyReleased() {
		shift = false;
	}

	void modifyGraph(String cmd) {
		backend.send(cmd + "\n");
		println(backend.receive());
		// here send D to back-end and dump the graph
		// then rebuild it via the XML parser.
		backend.send("D\n");
		Graph updated = xml.parseGraph(backend.receive());
		updated.reconcileVertices(graph);
		updated.setLayoutEngine(graph.getLayoutEngine());
		this.graph = updated;
		if (snapToGrid) graph.enableSnap();
		graph.layoutGraph();
		
		play();
	}

	public void draw() {
		
		background(255);
		
		stroke(240);
		for (int i=0;i<WIDTH;i+=Graph.GRID_X) line(i,0,i,HEIGHT);
		for (int i=0;i<HEIGHT;i+=Graph.GRID_Y) line(0,i,WIDTH,i);
		
		stroke(220);
		for (int i=0;i<WIDTH;i+=Graph.GRID_X*2) line(i,0,i,HEIGHT);
		for (int i=0;i<HEIGHT;i+=Graph.GRID_Y*2) line(0,i,WIDTH,i);
		
		textFont(helvetica);
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
		
		smooth();

		//	if we are going to save this frame start recording after the 
		// interface fluff has been drawn
		if (saveNextFrame) {
			doSaveNextFrame();
		}
		
		boolean moved = false;
		
		synchronized(graph.vertexList) {
			for (Vertex v : graph.vertexList) {
				moved = moved || v.tick();
				v.tick();
				v.display();
			}
		}
		
		synchronized(graph.edgeList) {
			if (moved) {
				for (Edge e : graph.edgeList) e.display(true);
			} else {
				for (Edge e : graph.edgeList) e.display(false);
			}
		}
		
		if (rectX!=-1) {
			moved = true;
			fill(100,100,255,30);
			stroke(100,100,255);
			rect(rectX, rectY, mouseX-rectX, mouseY-rectY);
		}

		// stop recording the frame now
		if(saveNextFrame) {
			stopSaveNextFrame();
		}
		
		if (!moved) pause();
		if(recordingVideo) doRecordVideo();
		
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
	

}
