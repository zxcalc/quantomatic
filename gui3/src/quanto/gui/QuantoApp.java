package quanto.gui;


import java.util.Collections;
import java.util.TreeMap;
import java.util.Map;

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
	private QuantoConsole console;
	private QuantoCore core;
	
	
	private final Map<String,InteractiveView> views;
	private volatile String focusedView = null;
	
	private QuantoApp() {
		views = Collections.synchronizedMap(new TreeMap<String,InteractiveView>());
		console = new QuantoConsole();
		core = console.getCore();
	}
	
	public void addView(String name, InteractiveView v) {
		if (views.get(name) != null)
			throw new QuantoCore.FatalError("Attempted to add duplicate view.");
		System.out.printf("adding %s\n", name);
		views.put(name, v);
	}
	
	public Map<String,InteractiveView> getViews() {
		return views;
	}

	public void removeView(String name) {
		views.remove(name);
	}
	
	public void focusView(String name) {
		if (focusedView != null) views.get(focusedView).loseFocus();
		focusedView = name;
		views.get(focusedView).gainFocus();
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

	public QuantoConsole getConsole() {
		return console;
	}

	public QuantoCore getCore() {
		return core;
	}

}
