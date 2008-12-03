package quanto.gui;
import java.awt.*;

import javax.swing.*;

import edu.uci.ics.jung.algorithms.layout.FRLayout;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.visualization.*;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;


public class QuantoFrame extends JFrame {
	private static final long serialVersionUID = 3656684775223085393L;
	QuantoCore qcore;
	
	public QuantoFrame() {
		setSize(800, 800);
		getContentPane().setLayout(new BorderLayout());
		QuantoGraph g = new QuantoGraph();
		
		g.addVertex(new QuantoVertex(QuantoVertex.Type.RED));
		
		
		/*PluggableRenderer pr = new PluggableRenderer();
		pr.setVertexPaintFunction(new VertexPaintFunction() {
			public Paint getDrawPaint(Vertex v) {return Color.black;}
			public Paint getFillPaint(Vertex v) {
				return ((QuantoVertex)v).getColor();
			}
		});*/
		
		Layout<QuantoVertex,String> layout =
			new FRLayout<QuantoVertex, String>(g);
		
		
        VisualizationViewer<QuantoVertex,String> vv =
        		new VisualizationViewer<QuantoVertex, String>(layout);
        vv.setGraphMouse(new DefaultModalGraphMouse<QuantoVertex, String>());
        getContentPane().add(vv, BorderLayout.CENTER);
        
        
        QuantoConsole qc = new QuantoConsole();
        qc.bindContext(new GraphContext(g,vv,layout));
        getContentPane().add(qc, BorderLayout.NORTH);
        
        this.pack();
	}


	public static void main(String[] args) {
		QuantoFrame fr = new QuantoFrame();
		fr.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		fr.setVisible(true);
	}

}
