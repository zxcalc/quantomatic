package quanto.gui;


import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.MouseEvent;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.LineBorder;
import javax.swing.event.MouseInputAdapter;

import edu.uci.ics.jung.graph.util.Pair;

public class RewriteViewer extends JFrame {
	private static final long serialVersionUID = 3627522980375030017L;
	private final InteractiveQuantoVisualizer vis;
	protected List<Pair<QuantoGraph>> rewrites;
	
	public RewriteViewer(InteractiveQuantoVisualizer vis) {
		this.vis = vis;
		rewrites = vis.getRewrites();
		
		JPanel panel = new JPanel();
		panel.setLayout(new GridLayout(rewrites.size(),1));
		int index = 0;
		for (Pair<QuantoGraph> rw : rewrites) {
			JPanel rwPanel = new JPanel();
			rwPanel.setLayout(new FlowLayout());
			QuantoVisualizer lhs = new QuantoVisualizer(rw.getFirst(), new Dimension(100,100));
			QuantoVisualizer rhs = new QuantoVisualizer(rw.getSecond(), new Dimension(100,100));
			JButton apply = new JButton("=>");
			rwPanel.add(lhs);
			rwPanel.add(apply);
			rwPanel.add(rhs);
			rwPanel.setBorder(new LineBorder(Color.black, 1));
			panel.add(rwPanel);
			
			final int thisIndex = index;
			
			apply.addMouseListener(new MouseInputAdapter() {
				public void mouseClicked(MouseEvent e) {
					RewriteViewer.this.vis.applyRewrite(thisIndex);
					RewriteViewer.this.dispose();
				}
			});
			index++;
		}
		
		JScrollPane scroll = new JScrollPane(panel);
		//scroll.setPreferredSize(new Dimension(600, 600));
		getContentPane().add(scroll);
		pack();
	}
}
