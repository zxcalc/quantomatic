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
import quanto.core.CoreChangeListener;
import quanto.core.TheoryChangeEvent;
import quanto.core.Theory;
import quanto.core.data.VertexType;

/*
 * Toolbox : Allows to add vertices/gates
 * bang/unbang/etc... vertices.
 */

public class Toolbox extends JPanel {
	
	private Core core;
    private ViewPort viewPort;
	
	public Toolbox(Core core, ViewPort viewPort) {
		/*
		 * The toolbox is divided in 2 distinct categories : add, 
		 * bangbox stuff.
		 * They are all using a grid layout.
		 */

		this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		
		this.core = core;
        this.viewPort = viewPort;
		
		JPanel controlArea = new JPanel();
		controlArea.setLayout(new BoxLayout(controlArea, BoxLayout.Y_AXIS));
		
		controlArea.add(new AddVertexArea());
		controlArea.add(createBangBoxArea());
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

	private ToolboxArea createBangBoxArea() {
		ToolboxArea bangBoxArea = new ToolboxArea("Bang Boxes", 3, 2);
		JButton button = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/BangVertex32.png", "Bang Vertices"));
		bangBoxArea.add(button);
		button.setToolTipText("Bang Vertices");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				viewPort.executeCommand("bang-vertices-command");
			}
		});
		
		button = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/UnbangVertex32.png", "Unbang Vertices"));
		bangBoxArea.add(button);
		button.setToolTipText("Unbang Vertices");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				viewPort.executeCommand("unbang-vertices-command");
			}
		});
		
		//Unbang and Drop seem to do the exact same thing...
		button = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/UnbangVertex32.png", "Drop Vertices"));
		bangBoxArea.add(button);
		button.setToolTipText("Drop Bang Box");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				viewPort.executeCommand("drop-bang-box-command");
			}
		});
		button = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/KillBangBox32.png", "Kill Bang Box"));
		bangBoxArea.add(button);
		button.setToolTipText("Kill Bang Box");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				viewPort.executeCommand("kill-bang-box-command");
			}
		});
		
		button = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/DuplicateBangBox32.png", "Duplicate Bang Box"));
		bangBoxArea.add(button);
		button.setToolTipText("Duplicate Bang Box");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				viewPort.executeCommand("duplicate-bang-box-command");
			}
		});
		
		return bangBoxArea;
	}
	
	class ToolboxArea extends JPanel {
		
		public ToolboxArea(String name, int rows, int columns) {					
			super(new GridLayout(rows, columns));
			setBorder(BorderFactory.createTitledBorder(name));
		}
        
        protected void setRows(int rows) {
            ((GridLayout)getLayout()).setRows(rows);
        }
	}
    
    private static int rowsForTheory(Theory theory) {
        return (int) Math.ceil(((float) theory.getVertexTypes().size() + 1)/ 2);
    }
    class AddVertexArea extends ToolboxArea {
        public AddVertexArea() {
            super("Add", rowsForTheory(core.getActiveTheory()), 2);
            loadButtons();
            core.addCoreChangeListener(new CoreChangeListener() {
                public void theoryChanged(TheoryChangeEvent evt) {
                    AddVertexArea.this.removeAll();
                    setRows(rowsForTheory(core.getActiveTheory()));
                    loadButtons();
                    AddVertexArea.this.validate();
                }

                public void theoryAboutToChange(TheoryChangeEvent evt) {
                }
            });
        }
        
        private void loadButtons() {
            /*
             * Then loop though all the types of vertices
             */
            for (final VertexType vertexType : core.getActiveTheory().getVertexTypes()) {
                JButton button = new JButton(vertexType.getVisualizationData().getIcon());
                this.add(button);
                String toolTipText = "Add vertex of type " + vertexType.getTypeName();
                if (vertexType.getMnemonic() != null)
                    toolTipText += " - '"+ vertexType.getMnemonic() +"'";
                button.setToolTipText(toolTipText);
                button.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent e) {
                        viewPort.executeCommand("add-" + vertexType.getTypeName() + "-vertex-command");
                    }
                });
            }
            //TODO: Still need an icon for that.
            JButton button = new JButton(createImageIcon("/toolbarButtonGraphics/quanto/EdgePointIcon20.png", "Add Boundary Vertex"));
            this.add(button);
            button.setToolTipText("Add Boundary Vertex");
            button.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    viewPort.executeCommand("add-boundary-vertex-command");
                }
            });
        }
    }
}

