package quanto.gui;


import java.util.ArrayList;
import java.util.List;

import javax.swing.UIManager;
import javax.swing.WindowConstants;

/**
 * Singleton class 
 * @author aleks
 *
 */
public class QuantoApp {
	public static final boolean isMac =
		(System.getProperty("os.name").toLowerCase().indexOf("mac") != -1);
	private static QuantoApp theApp = null;
	
	private List<InteractiveView> views;
	
	private QuantoApp() {
		views = new ArrayList<InteractiveView>();
	}
	
	public static QuantoApp getInstance() {
		if (theApp == null) theApp = new QuantoApp();
		return theApp;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		for (String arg : args) {
			if (arg.equals("--app-mode")) {
				QuantoCore.appMode = true;
				edu.uci.ics.jung.contrib.DotLayout.dotProgram =
					"Quantomatic.app/Contents/MacOS/dot_static";
				System.out.println("Invoked as OS X application.");
			}
		}
		
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			System.err.println("ERROR SETTING LOOK AND FEEL:");
			e.printStackTrace();
		}
		if (QuantoApp.isMac) {
			System.setProperty("apple.laf.useScreenMenuBar", "true");
			System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Quanto");
		}
		
		QuantoFrame fr = new QuantoFrame();
		fr.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		fr.setVisible(true);
	}

	public List<InteractiveView> getViews() {
		return views;
	}

}
