import java.awt.*;
import java.awt.event.*;
import processing.core.PConstants;


public class QuantoFrame extends Frame implements PConstants {
	private static final long serialVersionUID = 1L;

	public QuantoFrame() {
		super("Quanto");

        setLayout(new BorderLayout());
        setSize(QuantoApplet.WIDTH,QuantoApplet.HEIGHT);
        
        final QuantoApplet embed = new QuantoApplet();
        add(embed, BorderLayout.CENTER);

        // important to call this whenever embedding a PApplet.
        // It ensures that the animation thread is started and
        // that other internal variables are properly set.
        embed.init();
        
        MenuBar mb = new MenuBar();
        Menu tools = new Menu("Tools");
        Menu actions = new Menu("Actions");
        mb.add(tools);
        mb.add(actions);
        
        final String[] toolNames = {"Select (s)", "Move (m)", "Edge (e)"};
        final char[] toolKeys = {'s', 'm', 'e'};
        
        final String[] actionNames = {
        		"New graph (n)",
        		"Undo (u)",
        		"Layout (l)",
        		"add Red (r)",
        		"add Green (g)",
        		"add Hadamard (h)",
        		"add Boundary (b)",
        		"Delete vertex (d)",
        		"toggle sPlines (p)",
        		"Capture to pdf (c)",
        		"start/stop Video (v)",
        		"toggle snap to Grid (G)",
        		"quit (q)"
        	};
        final char[] actionKeys = {
        		'n', 'u', 'l', 'r', 'g', 'h', 'b',
        		'd', 'p', 'c', 'v', 'G', 'q'
        	};
        
        MenuItem mi;
        
        for (int i=0;i<toolNames.length;++i) {
        	// we need to ensure this char stays allocated...
        	final char c = toolKeys[i];
        	mi = new MenuItem(toolNames[i]);
        	mi.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					embed.key = c;
					embed.keyPressed();
				}
        		
        	});
        	tools.add(mi);
        }
        
        for (int i=0;i<actionNames.length;++i) {
        	// we need to ensure this char stays allocated...
        	final char c = actionKeys[i];
        	mi = new MenuItem(actionNames[i]);
        	mi.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					embed.key = c;
					embed.keyPressed();
				}
        		
        	});
        	actions.add(mi);
        }
        
        setMenuBar(mb);
	}
	
	public static void main(String[] args) {
		new QuantoFrame().setVisible(true);
	}

}
