package quanto.gui;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import javax.swing.*;

public class QuantoFrame extends JFrame {
	private static final long serialVersionUID = 3656684775223085393L;
	public QuantoCore qcore;
	public QuantoConsole qconsole;
	boolean consoleVisible;
	
	protected abstract class QuantoFrameListener implements ActionListener {
		protected QuantoFrame frame;
		public QuantoFrameListener(QuantoFrame fr) {
			frame = fr;
		}
		public abstract void actionPerformed(ActionEvent e);
	}
	
	public QuantoFrame() {
		consoleVisible = true;
		JMenuBar mb = new JMenuBar();
		JMenu file = new JMenu("File");
		
		JMenuItem item = new JMenuItem("Show/Hide Console");
		item.addActionListener(new QuantoFrameListener(this) {
			public void actionPerformed(ActionEvent e) {
				frame.showHideConsole();
			}
		});
		
		file.add(item);
		mb.add(file);
		
		setJMenuBar(mb);
		
		setSize(800, 800);
		getContentPane().setLayout(new BorderLayout());
		QuantoGraph g = new QuantoGraph();
		
		QuantoVisualizer vv = new QuantoVisualizer(g);
        getContentPane().add(vv, BorderLayout.CENTER);
        
        qconsole = new QuantoConsole();
        qconsole.bindContext(vv);
        getContentPane().add(qconsole, BorderLayout.NORTH);
        
        /*addKeyListener(new KeyAdapter() {
			public void keyReleased(KeyEvent e) {
				if (e.getKeyChar() == '`')
					((QuantoFrame)e.getSource()).showHideConsole();
				System.out.println("got a key");
			}
        });
        
        getRootPane().registerKeyboardAction(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				System.out.println("BACKTICK");
				((QuantoFrame)e.getSource()).showHideConsole();
			}
        	
        }, KeyStroke.getKeyStroke('`'),
        	JComponent.WHEN_IN_FOCUSED_WINDOW);*/
        
        this.pack();
	}
	
	public void showHideConsole() {
		consoleVisible = !consoleVisible;
		qconsole.setVisible(consoleVisible);
		this.pack();
	}


	public static void main(String[] args) {
		QuantoFrame fr = new QuantoFrame();
		fr.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		fr.setVisible(true);
	}

}
