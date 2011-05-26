package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Container;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import quanto.core.Core;

/*
 * Tabbed Pane containing the RulesBar and the Toolbox.
 * Instanciated in QuantoFrame
 */

public class LeftTabbedPane extends JPanel {
	public LeftTabbedPane(Core core, QuantoFrame quantoFrame) {
		
		this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		
		JTabbedPane tabbedPane = new JTabbedPane();
		RulesBar sidebar = new RulesBar(core.getRuleset());
		tabbedPane.addTab("Rules", null, sidebar,
        "Display Ruleset");
		
		Toolbox toolbox = new Toolbox(core, quantoFrame);
		tabbedPane.addTab("Toolbox", null, toolbox, "Display Toolbox");

		this.add(tabbedPane);
	}
}
