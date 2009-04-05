package quanto.gui;


import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.LineBorder;

public class RewriteViewer extends JFrame {
	private static final long serialVersionUID = 3627522980375030017L;
	private final InteractiveGraphView vis;
	protected List<Rewrite> rewrites;
	
	public RewriteViewer(InteractiveGraphView vis) {
		this.vis = vis;
		rewrites = vis.getRewrites();
		
		KeyListener esc = new KeyAdapter() {
			@Override
			public void keyReleased(KeyEvent e) {
				if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
					RewriteViewer.this.dispose();
				}
			}
		};
		
		JPanel panel = new JPanel();
		panel.setLayout(new GridLayout(rewrites.size(),1));
		int index = 0;
		JButton cancel = new JButton("Cancel");
		JComponent focusMe = cancel;
		for (Rewrite rw : rewrites) {
			JPanel rwPanel = new JPanel();
			rwPanel.setLayout(new FlowLayout());
			JLabel ruleName = new JLabel(rw.getName());
			rwPanel.add(ruleName);
			GraphView lhs = new GraphView(rw.getLhs(), new Dimension(100,100));
			GraphView rhs = new GraphView(rw.getRhs(), new Dimension(100,100));
			JButton apply = new JButton("=>");
			rwPanel.setBackground(lhs.getBackground());
			rwPanel.add(lhs);
			rwPanel.add(apply);
			rwPanel.add(rhs);
			if (index == 0) focusMe = apply;
			rwPanel.setBorder(new LineBorder(Color.black, 1));
			panel.add(rwPanel);
			lhs.zoomToFit(); rhs.zoomToFit();
			
			final int thisIndex = index;
			
			apply.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					RewriteViewer.this.vis.clearHighlight();
					RewriteViewer.this.vis.applyRewrite(thisIndex);
					RewriteViewer.this.dispose();
				}
			});
			
			MouseAdapter hl = new MouseAdapter() {
				public void mouseEntered(MouseEvent e) {
					QuantoGraph match = 
						RewriteViewer.this.rewrites
							.get(thisIndex).getLhs();
					RewriteViewer.this.vis.highlightSubgraph(match);
				}
				
				public void mouseExited(MouseEvent e) {
					RewriteViewer.this.vis.clearHighlight();
				}
			};
			
			rwPanel.addMouseListener(hl);
			lhs.addMouseListener(hl);
			rhs.addMouseListener(hl);
			apply.addMouseListener(hl);
			
			index++;
		}
		
		JScrollPane scroll = new JScrollPane(panel);
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(scroll, BorderLayout.CENTER);		
		cancel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				RewriteViewer.this.dispose();
			}
		});
		
		getContentPane().add(cancel, BorderLayout.SOUTH);
		
		pack();
		
		focusMe.grabFocus();
		focusMe.addKeyListener(esc);
		addKeyListener(esc);
	}
}
