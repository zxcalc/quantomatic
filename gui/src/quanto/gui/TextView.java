package quanto.gui;

import java.awt.BorderLayout;
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

		setLayout(new BorderLayout());
		add(new JScrollPane(textArea), BorderLayout.CENTER);

		instanceCount++;
	}

	public void attached(ViewPort vp) {
	}

	public void detached(ViewPort vp) {
	}

	public void cleanUp() {
	}

	public boolean isSaved() {
		return true;
	}

	public static void registerKnownCommands(Collection<String> commands) {
	}

	public void commandTriggered(String command) {
	}

	public void refresh() {
	}
}
