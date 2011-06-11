package quanto.gui;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;


import quanto.core.Core;
import quanto.core.data.VertexType;

/*
 * Toolbox : Allows to add vertices/gates
 * lock/unlock vertices and bang/unbang/etc... vertices.
 */

public class Toolbox extends JPanel {
	
	private Core core;
	private QuantoFrame quantoFrame;
	
	public Toolbox(Core core, QuantoFrame quantoFrame) {
		/*
		 * The toolbox is divided in 3 distinct categories : add, 
		 * lock/unlock, bangbox stuff.
		 * They are all using a grid layout.
		 */

		this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		
		this.core = core;
		this.quantoFrame = quantoFrame;
		
		JPanel controlArea = new JPanel();
		controlArea.setLayout(new BoxLayout(controlArea, BoxLayout.Y_AXIS));
		
		controlArea.add(createAddVertexArea(this.core));
		controlArea.add(createLockArea(this.quantoFrame));
		controlArea.add(createBangBoxArea(this.quantoFrame));
		this.add(controlArea);
	}

	protected ImageIcon createImageIcon(String path,
		    String description) {
		java.net.URL imgURL = getClass().getResource(path);
		if (imgURL != null) {
			return new ImageIcon(imgURL, description);
		}
		else {
			System.err.println("Couldn't find file: " + path);
			return null;
		}
	}
	
	private ToolboxArea createAddVertexArea(Core core) {
		ToolboxArea addVertexArea;
		int numberOfRows;
		
		/* 
		 * We have n types + 1 for the boundary vertices. Two items per line.
		 */
		numberOfRows = (int) Math.ceil(((float) core.getActiveTheory().getVertexTypes().size() + 1)/ 2);
		addVertexArea = new ToolboxArea("Add", numberOfRows, 2);
		/*
		 * Then loop though all the types of vertices
		 */
		for (final VertexType vertexType : core.getActiveTheory().getVertexTypes()) {
			JButton button = new JButton(vertexType.getVisualizationData().getIcon());
			addVertexArea.add(button);
			button.setToolTipText("Add vertex of type " + vertexType.getTypeName());
			button.addActionListener(new ActionListener() {

				public void actionPerformed(ActionEvent e) {
					quantoFrame.getViewPort().executeCommand("add-" + vertexType.getTypeName() + "-vertex-command");
				}
			});
		}
		//TODO: Still need an icon for that.
		JButton button = new JButton("Bound.");
		addVertexArea.add(button);
		button.setToolTipText("Add boundary vertex");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				quantoFrame.getViewPort().executeCommand("add-boundary-vertex-command");
			}
		});
		
		return addVertexArea;
	}
	
	private ToolboxArea createLockArea(final QuantoFrame quantoFrame) {
		ToolboxArea lockArea = new ToolboxArea("Lock/Unlock", 1, 2);
		
		JButton button = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/Lock24.gif", "Lock Vertices"));
		lockArea.add(button);
		button.setToolTipText("Lock Vertices");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				quantoFrame.getViewPort().executeCommand("lock-vertices-command");
			}
		});
		
		button = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/Unlock24.gif", "Unlock Vertices"));
		lockArea.add(button);
		button.setToolTipText("Unlock Vertices");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				quantoFrame.getViewPort().executeCommand("unlock-vertices-command");
			}
		});
		
		return lockArea;
	}
	
	private ToolboxArea createBangBoxArea(final QuantoFrame quantoFrame) {
		ToolboxArea bangBoxArea = new ToolboxArea("Bang Boxes", 3, 2);
		JButton button = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/BangVertex32.png", "Bang Vertices"));
		bangBoxArea.add(button);
		button.setToolTipText("Bang Vertices");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				quantoFrame.getViewPort().executeCommand("bang-vertices-command");
			}
		});
		
		button = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/UnbangVertex32.png", "Unbang Vertices"));
		bangBoxArea.add(button);
		button.setToolTipText("Unbang Vertices");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				quantoFrame.getViewPort().executeCommand("unbang-vertices-command");
			}
		});
		
		//Unbang and Drop seem to do the exact same thing...
		button = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/UnbangVertex32.png", "Drop Vertices"));
		bangBoxArea.add(button);
		button.setToolTipText("Drop Bang Box");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				quantoFrame.getViewPort().executeCommand("drop-bang-box-command");
			}
		});
		button = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/KillBangBox32.png", "Kill Bang Box"));
		bangBoxArea.add(button);
		button.setToolTipText("Kill Bang Box");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				quantoFrame.getViewPort().executeCommand("kill-bang-box-command");
			}
		});
		
		button = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/DuplicateBangBox32.png", "Duplicate Bang Box"));
		bangBoxArea.add(button);
		button.setToolTipText("Duplicate Bang Box");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				quantoFrame.getViewPort().executeCommand("duplicate-bang-box-command");
			}
		});
		
		return bangBoxArea;
	}
	
	class ToolboxArea extends JPanel {
		
		public ToolboxArea(String name, int rows, int columns) {					
			super(new GridLayout(rows, columns));
			setBorder(BorderFactory.createTitledBorder(name));
		}
	}
}

