package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.SortedSet;
import java.util.Stack;

import javax.swing.*;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.JTextComponent;

public class ConsoleView extends InteractiveView {

	private static final long serialVersionUID = -5833674157230451213L;
	private final QuantoCore core;
	private JTextField input;
	private JTextArea output;
	private Stack<String> history;
	private int hpointer;
	private JTextComponent lastFocusOwner = null;

	private class CommandEntryListener
		extends KeyAdapter
		implements ActionListener
	{
		public void actionPerformed( final ActionEvent e )
		{
			submitCommand(input);
			input.requestFocusInWindow();
		}

		@Override
		public void keyReleased( final KeyEvent e )
		{
			if ( e.isConsumed() )
			{
				return;
			}

			JTextField tf = (JTextField) e.getSource();
			switch (e.getKeyCode()) {
				case KeyEvent.VK_UP:
					if (hpointer > 0) {
						tf.setText(history.get(--hpointer));
					}
					e.consume();
					break;
				case KeyEvent.VK_DOWN:
					if (hpointer < history.size() - 1) {
						tf.setText(history.get(++hpointer));
					}
					e.consume();
					break;
				case KeyEvent.VK_TAB:
					showCompletions(tf);
					e.consume();
					break;
				case KeyEvent.VK_ENTER:
					submitCommand(tf);
					e.consume();
					break;
			}
		}

		private void showCompletions(JTextField tf) {
			SortedSet<String> compl = core.getCompleter().getCompletions(tf.getText());
			if (compl.size() == 1) {
				tf.setText(compl.first());
			}
			else if (compl.size() > 1) {
				tf.setText(Completer.greatestCommonPrefix(compl));
				println();
				for (String c : compl) {
					println(c);
				}
				print(core.getPrompt());
			}
		}

		private void submitCommand(JTextField tf)
		{
			String text = tf.getText();
			write(text);
			tf.setText("");
			if (history.isEmpty() || !history.peek().equals(text)) {
				history.push(text);
			}
			hpointer = history.size();
		}
	}

	private FocusListener focusListener = new FocusListener() {
		public void focusGained(FocusEvent e) {
			lastFocusOwner = (JTextComponent)e.getComponent();
			updateSelectionCommands();
		}

		public void focusLost(FocusEvent e) {
			if (!e.isTemporary()) {
				lastFocusOwner = null;
				updateSelectionCommands();
			}
		}
	};
	private CaretListener caretListener = new CaretListener() {
		public void caretUpdate(CaretEvent e) {
			if (isAttached()) {
				updateSelectionCommands();
			}
		}
	};
	private void print(String text) {
		output.append(text);
		output.setCaretPosition(output.getDocument().getLength() - 1);
	}
	private void println(String text) {
		print(text + "\n");
	}
	private void println() {
		print("\n");
	}

	public ConsoleView(final QuantoApp app, QuantoCore core) {
		super("console");

		this.setLayout(new BorderLayout());
		output = new JTextArea();
		output.setEditable(false);
		output.addFocusListener(focusListener);
		output.addCaretListener(caretListener);

		this.core = core;
		print(core.getPrompt());

		history = new Stack<String>();
		input = new JTextField();
		input.setFocusTraversalKeysEnabled(false);
		input.addFocusListener(focusListener);
		input.addCaretListener(caretListener);
		CommandEntryListener listener = new CommandEntryListener();
		input.addKeyListener(listener);

		JButton execButton = new JButton("Exec");
		execButton.addActionListener(listener);

		JPanel commandPane = new JPanel(new BorderLayout());
		commandPane.add(input, BorderLayout.CENTER);
		commandPane.add(execButton, BorderLayout.LINE_END);

		JScrollPane scroll = new JScrollPane(output);
		scroll.setPreferredSize(new Dimension(800, 600));
		this.add(scroll, BorderLayout.CENTER);
		this.add(commandPane, BorderLayout.PAGE_END);
	}

	public void write(String text) {
		synchronized (core) {
			try {
				println(text);
				core.send(text);
				String rcv = core.receiveOrFail();
				print(rcv);
			}
			catch (QuantoCore.CoreException e) {
				print("ERROR: ".concat(e.getMessage()));
			}

			try {
				// print the prompt
				print(core.receive());
			}
			catch (QuantoCore.CoreException e) {
				print("ERROR: ".concat(e.getMessage()));
			}
		}
	}

	private void updateSelectionCommands() {
		if (isAttached()) {
			ViewPort vp = getViewPort();
			if (lastFocusOwner == null) {
				vp.setCommandEnabled(ViewPort.CUT_ACTION, false);
				vp.setCommandEnabled(ViewPort.COPY_ACTION, false);
				vp.setCommandEnabled(ViewPort.PASTE_ACTION, false);
				vp.setCommandEnabled(ViewPort.SELECT_ALL_ACTION, false);
				vp.setCommandEnabled(ViewPort.DESELECT_ALL_ACTION, false);
			}
			else {
				boolean hasSelection = (lastFocusOwner.getSelectionEnd() - lastFocusOwner.getSelectionStart()) != 0;
				vp.setCommandEnabled(ViewPort.CUT_ACTION,
					lastFocusOwner.isEditable() &&
					hasSelection
					);
				vp.setCommandEnabled(ViewPort.COPY_ACTION,
					hasSelection
					);
				vp.setCommandEnabled(ViewPort.PASTE_ACTION,
					lastFocusOwner.isEditable()
					);
				vp.setCommandEnabled(ViewPort.SELECT_ALL_ACTION,
					true);
				vp.setCommandEnabled(ViewPort.DESELECT_ALL_ACTION,
					true);
			}
		}
	}

	@Override
	public void grabFocus() {
		input.grabFocus();
	}

	public QuantoCore getCore() {
		return core;
	}

	public void attached(ViewPort vp) {
		// refuse to allow us to be closed
		vp.preventViewClosure();
		input.requestFocusInWindow();
	}

	public void detached(ViewPort vp) {
		vp.setCommandEnabled(ViewPort.CUT_ACTION, false);
		vp.setCommandEnabled(ViewPort.COPY_ACTION, false);
		vp.setCommandEnabled(ViewPort.PASTE_ACTION, false);
		vp.setCommandEnabled(ViewPort.SELECT_ALL_ACTION, false);
		vp.setCommandEnabled(ViewPort.DESELECT_ALL_ACTION, false);
	}

	public void cleanUp() {
	}

	public boolean isSaved() {
		return true;
	}

	public static void registerKnownCommands() {
	}

	public void commandTriggered(String command) {
		if (ViewPort.CUT_ACTION.equals(command)) {
			if (lastFocusOwner != null)
				lastFocusOwner.cut();
		}
		else if (ViewPort.COPY_ACTION.equals(command)) {
			if (lastFocusOwner != null)
				lastFocusOwner.copy();
		}
		else if (ViewPort.PASTE_ACTION.equals(command)) {
			if (lastFocusOwner != null)
				lastFocusOwner.paste();
		}
		else if (ViewPort.SELECT_ALL_ACTION.equals(command)) {
			if (lastFocusOwner != null)
				lastFocusOwner.selectAll();
		}
		else if (ViewPort.DESELECT_ALL_ACTION.equals(command)) {
			if (lastFocusOwner != null)
				lastFocusOwner.select(0, 0);
		}
	}

	public void refresh() {
	}
}
