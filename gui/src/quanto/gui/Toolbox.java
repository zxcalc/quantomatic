package quanto.gui;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.Rectangle2D;
import java.util.HashSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;


import quanto.core.Core;
import quanto.core.CoreChangeListener;
import quanto.core.CoreException;
import quanto.core.TheoryChangeEvent;
import quanto.core.Theory;
import quanto.core.data.BangBox;
import quanto.core.data.CoreGraph;
import quanto.core.data.Vertex;
import quanto.core.data.VertexType;

/*
 * Toolbox : Allows to add vertices/gates
 * bang/unbang/etc... vertices.
 */

public class Toolbox extends JPanel {
	
	private Core core;
    private ViewPort viewPort;
    private final static Logger logger =
              Logger.getLogger("quanto.gui");
    
    private void showModalError(String message, CoreException ex) {
           logger.log(Level.SEVERE, message, ex);
           DetailedErrorDialog.showCoreErrorDialog(this, message, ex);
    }
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
	
	private ImageIcon createImageIcon(String path,
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
		final ToolboxArea bangBoxArea = new ToolboxArea("Bang Boxes", 3, 2);
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
		
		final JButton popbutton = new JButton("+");
		bangBoxArea.add(popbutton);
		popbutton.setToolTipText("More...");
		popbutton.addActionListener(new ActionListener() {
		     public void actionPerformed(ActionEvent e) {
		          JPopupMenu popup = createPopupMenu();
		          if (popup != null)
		               popup.show(bangBoxArea, popbutton.getX() + popbutton.getWidth(), popbutton.getY());
		     }
		});
		
		return bangBoxArea;
	}
	
	private JMenu createSubPopupMenu(final BangBox bangBox, final InteractiveGraphView view) {
	     final CoreGraph graph = view.getGraph();
	     JMenu menu = new JMenu(bangBox.getCoreName());
	     JMenuItem menuItem = null;
	     JMenu subMenu = new JMenu("Bang Vertex...");
	     if (graph.getVertices().size() > 0) {
	          for(final Vertex v: graph.getVertices()) {
	               if (graph.getBoxedVertices(bangBox).contains(v))
	                    continue;
	               menuItem = new JMenuItem(v.getCoreName());
	               menuItem.addMouseListener(new MouseListener() {

	                    @Override
	                    public void mouseEntered(MouseEvent e) {
	                         //Using a trick because highlightSubgraph
	                         //does not highlight a subgraph but it's 
	                         //graph - subgraph. 
	                         CoreGraph subGraph = new CoreGraph();
	                         for(Vertex subV: graph.getVertices())
	                              subGraph.addVertex(subV);
	                         subGraph.removeVertex(v);
	                         view.highlightSubgraph(subGraph);
	                    }

	                    @Override
	                    public void mouseExited(MouseEvent e) {
	                         view.clearHighlight();
	                    }

	                    public void mouseClicked(MouseEvent arg0) {}

	                    @Override
	                    public void mousePressed(MouseEvent arg0) {
	                         view.clearHighlight();
	                    }

	                    public void mouseReleased(MouseEvent arg0) {}
	               });
	               menuItem.addActionListener(new ActionListener() {
	                    public void actionPerformed(ActionEvent e) {
	                         HashSet<Vertex> vertices = new HashSet<Vertex>();
	                         vertices.add(v);
	                         try {
	                              view.cacheVertexPositions();
	                              Rectangle2D rect=view.getVisualization().getGraphBounds();
	                              core.bangVertices(graph, bangBox.getCoreName(), vertices);
	                              view.updateGraph(rect);
	                         } catch (CoreException ex) {
	                              showModalError("Bang Vertex", ex);
	                         }
	                    }
	               });
	               subMenu.add(menuItem);
	          }
	          menu.add(subMenu);
	     }
	     if (graph.getBangBoxes().size() > 1) {
	          subMenu = new JMenu("Merge with...");
	          for(final BangBox b: graph.getBangBoxes()) {
	               if (b.getCoreName().equals(bangBox.getCoreName()))
	                    continue;
	               menuItem = new JMenuItem(b.getCoreName());
	               menuItem.addActionListener(new ActionListener() {
	                    public void actionPerformed(ActionEvent e) {
	                         try {
	                              HashSet<BangBox> bbs = new HashSet<BangBox>();
	                              bbs.add(b);bbs.add(bangBox);
	                              view.cacheVertexPositions();
	                              Rectangle2D rect=view.getVisualization().getGraphBounds();
	                              core.mergeBangBoxes(graph, bbs);
	                              view.updateGraph(rect);
                              } catch (CoreException ex) {
                                   showModalError("Merge !-Box", ex);
                              }
	                    }
	               });
	               subMenu.add(menuItem);
	          }
	          menu.add(subMenu);
	     }
	     String label = null;
	     if (graph.getBoxedVertices(bangBox).size() == 0)
	          label = "Drop this (empty) !-Box";
	     else
	          label = "Drop this !-Box";
	     menuItem = new JMenuItem(label);
	     menuItem.addActionListener(new ActionListener() {
               public void actionPerformed(ActionEvent e) {
                    try {
                         HashSet<BangBox> bbs = new HashSet<BangBox>();
                         bbs.add(bangBox);
                         view.cacheVertexPositions();
                         Rectangle2D rect=view.getVisualization().getGraphBounds();
                         core.dropBangBoxes(graph, bbs);
                         view.updateGraph(rect);
                    } catch (CoreException ex) {
                         showModalError("Drop!-Box", ex);
                    }
               }
          });
	     menu.add(menuItem);
	     return menu;
	}
	
	private JPopupMenu createPopupMenu() {
	     CoreGraph graph = null;
	     InteractiveGraphView view = null;
	     if (viewPort.getAttachedView() instanceof InteractiveGraphView)
	          view = (InteractiveGraphView) viewPort.getAttachedView();
	     else if (viewPort.getAttachedView() instanceof SplitGraphView)
	          if (((SplitGraphView) viewPort.getAttachedView()).isLeftFocused())
	               view = (InteractiveGraphView) ((SplitGraphView) viewPort.getAttachedView()).getLeftView();
	          else
	               view = (InteractiveGraphView) ((SplitGraphView) viewPort.getAttachedView()).getRightView();
	     else
	          return null;
	     graph = view.getGraph();
	     JPopupMenu menu = new JPopupMenu();
	     if (graph.getBangBoxes().size() == 0) {
	          JMenuItem menuItem = new JMenuItem("No !-Boxes");
	          menu.add(menuItem);
	          menuItem.setEnabled(false);
	          return menu;
	     }
	          
	     //Create a submenu for each !-Box
	     for (BangBox b: graph.getBangBoxes()) {
	          JMenu menuItem = createSubPopupMenu(b, view);
	          menu.add(menuItem);
	     }
	     
	     return menu;
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

