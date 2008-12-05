package quanto.gui;
import java.awt.*;

import javax.swing.*;

public class QuantoFrame extends JFrame {
	private static final long serialVersionUID = 3656684775223085393L;
	QuantoCore qcore;
	
	public QuantoFrame() {
		setSize(800, 800);
		getContentPane().setLayout(new BorderLayout());
		QuantoGraph g = new QuantoGraph();
		
		QuantoVisualizer vv = new QuantoVisualizer(g);
        getContentPane().add(vv, BorderLayout.CENTER);
        
        QuantoConsole qc = new QuantoConsole();
        qc.bindContext(vv);
        getContentPane().add(qc, BorderLayout.NORTH);
        
        this.pack();
	}


	public static void main(String[] args) {
		QuantoFrame fr = new QuantoFrame();
		fr.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		fr.setVisible(true);
	}

}
