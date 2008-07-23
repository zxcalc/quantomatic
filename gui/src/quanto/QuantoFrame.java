package quanto;
import java.awt.*;
import java.awt.event.*;
import processing.core.PConstants;
import javax.swing.*;

public class QuantoFrame extends JFrame implements PConstants {
	private static final long serialVersionUID = 1L;

	static QuantoApplet applet;
	
	public QuantoFrame() {
		super("Quanto");

        setLayout(new BorderLayout());
        setSize(QuantoApplet.WIDTH,QuantoApplet.HEIGHT);
        
        applet = new QuantoApplet();
        add(applet, BorderLayout.CENTER);

        // important to call this whenever embedding a PApplet.
        // It ensures that the animation thread is started and
        // that other internal variables are properly set.
        applet.init();
        
        makeAWTMenu();
	}
	
	public static void main(String[] args) {
		
		QuantoFrame f = new QuantoFrame();
		f.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				quit();
			}
		});
		f.setVisible(true);
	}
	
	public static void quit(){
		applet.quit();
	}
	
	
	public void makeAWTMenu() {
		/* add menus */
		MenuBar mb = new MenuBar();
		Menu menu;
		/* File Menu */
		menu = new Menu("File");
		mb.add(menu);
		
		MenuItem m;
		m = new MenuItem("New", new MenuShortcut(KeyEvent.VK_N));
		m.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				
			}
		});
		menu.add(m);
		menu.addSeparator();
		menu.add(new MenuItem("Capture to PDF",new MenuShortcut(KeyEvent.VK_N)));
		menu.add(new MenuItem("Start/Stop Video",new MenuShortcut('v')));
		menu.add(new MenuItem("Latex to clipboard",new MenuShortcut('x')));
		menu.add(new MenuItem("Dot to clipboard",new MenuShortcut('t')));
		menu.addSeparator();
		menu.add(new MenuItem("Quit",new MenuShortcut('q')));

		setMenuBar(mb);
		
		
	}
	
	public void makeSwingMenu() {
        /* add menus */
        JMenuBar mb = new JMenuBar();
        
        JMenuItem m;
        JMenu menu;
        
        /* File Menu */
        menu = new JMenu("File"); mb.add(menu);
   
        m = new JMenuItem("New"); m.setMnemonic('n'); menu.add(m);
        m.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				//applet.newGraph();
			}
		});
        menu.addSeparator();
        m = new JMenuItem("Capture to PDF"); m.setMnemonic('c'); menu.add(m);
        m = new JMenuItem("Start/Stop Video"); m.setMnemonic('v'); menu.add(m);
        m = new JMenuItem("Latex to clipboard"); m.setMnemonic('x'); menu.add(m);
        m = new JMenuItem("Dot to clipboard"); m.setMnemonic('t'); menu.add(m);
        menu.addSeparator();
        m = new JMenuItem("Quit"); m.setMnemonic('q'); menu.add(m);
        
		/* Edit Menu */
        menu = new JMenu("Edit"); mb.add(menu);
        m = new JMenuItem("Select Mode"); m.setMnemonic('s'); menu.add(m);
        m = new JMenuItem("Move Mode"); m.setMnemonic('m'); menu.add(m);
        menu.addSeparator();
        m = new JMenuItem("Add Red"); m.setMnemonic('r'); menu.add(m);
        m = new JMenuItem("Add Green"); m.setMnemonic('g'); menu.add(m);
        m = new JMenuItem("Add Hadamard"); m.setMnemonic('h'); menu.add(m);
        m = new JMenuItem("Add Boundary"); m.setMnemonic('b'); menu.add(m);
        m = new JMenuItem("Add Edge"); m.setMnemonic('e'); menu.add(m);
    	menu.addSeparator();
        m = new JMenuItem("Delete Vertex"); m.setMnemonic('d'); menu.add(m);
        menu.addSeparator();
        m = new JMenuItem("Undo"); m.setMnemonic('u'); menu.add(m);
        
        /* Layout Menu */
        menu = new JMenu("Layout"); mb.add(menu);
        m = new JMenuItem("Layout"); m.setMnemonic('l'); menu.add(m);
        m = new JMenuItem("Toggle splines for dot"); m.setMnemonic('p'); menu.add(m);
        m = new JMenuItem("Toggle snap to grid"); m.setMnemonic('G'); menu.add(m);
        m = new JMenuItem("Next layout engine"); m.setMnemonic('y'); menu.add(m);
        
        setJMenuBar(mb);
	}
}
