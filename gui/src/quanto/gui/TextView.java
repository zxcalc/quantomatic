package quanto.gui;

import java.util.Collection;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

public class TextView extends InteractiveView {

	private static final long serialVersionUID = -9201774497137020314L;
	private static int instanceCount = 0;
	private JTextArea textArea;

	public TextView(String title, String text) {
		setTitle(title);
		textArea = new JTextArea();
		textArea.setText(text);

		setMainComponent(new JScrollPane(textArea));

		instanceCount++;
	}

	public static void registerKnownCommands(Collection<String> commands) {
	}
}
