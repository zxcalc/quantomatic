package quanto.gui;

import quanto.core.ConsoleInterface;
import quanto.core.Completer;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.Collection;
import java.util.SortedSet;
import java.util.Stack;

import javax.swing.*;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.JTextComponent;
import quanto.core.CoreException;
import quanto.core.ParseException;
import quanto.core.protocol.ProtocolManager;

public class ConsoleView extends InteractiveView {

	private static final long serialVersionUID = -5833674157230451213L;
	private final ConsoleInterface coreConsole;
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

		private void showCompletions(JTextField tf)
                {
                        if (coreConsole == null)
                                return;

			SortedSet<String> compl = coreConsole.getCompleter().getCompletions(tf.getText());
			if (compl.size() == 1) {
				tf.setText(compl.first());
			}
			else if (compl.size() > 1) {
				tf.setText(Completer.greatestCommonPrefix(compl));
				println();
				for (String c : compl) {
					println(c);
				}
                                prompt();
			}
		}

		private void submitCommand(JTextField tf)
		{
                        execCommand(tf.getText());
                        tf.setText("");
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
	private void prompt() {
		print("quanto:> ");
	}
        private void output(String text) {
		println(text.trim());
                prompt();
        }
        private void execCommand(String text) {
                if (coreConsole == null)
                        return;

                try
                {
                        println(text);
                        coreConsole.inputCommandAsync(text);
                        if (history.isEmpty() || !history.peek().equals(text)) {
                                history.push(text);
                        }
                        hpointer = history.size();
                }
                catch (CoreException ex)
                {
                        output("Error: " + ex.getMessage());
                }
                catch (ParseException ex)
                {
                        output("Failed to parse command: " + ex.getMessage());
                }
        }

	public ConsoleView(final QuantoApp app, ProtocolManager core) {
		super("console");

		this.setLayout(new BorderLayout());
		output = new JTextArea();
		output.setEditable(false);
		output.addFocusListener(focusListener);
		output.addCaretListener(caretListener);

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

                this.coreConsole = new ConsoleInterface(core);
                coreConsole.setResponseListener(new ConsoleInterface.ResponseListener() {
                        public void responseReceived(String response) {
                                output(response);
                        }
                });

                prompt();
	}

	private void updateSelectionCommands() {
		if (isAttached()) {
			ViewPort vp = getViewPort();
			if (lastFocusOwner == null) {
				vp.setCommandEnabled(CommandManager.Command.Cut, false);
				vp.setCommandEnabled(CommandManager.Command.Copy, false);
				vp.setCommandEnabled(CommandManager.Command.Paste, false);
				vp.setCommandEnabled(CommandManager.Command.SelectAll, false);
				vp.setCommandEnabled(CommandManager.Command.DeselectAll, false);
			}
			else {
				boolean hasSelection = (lastFocusOwner.getSelectionEnd() - lastFocusOwner.getSelectionStart()) != 0;
				vp.setCommandEnabled(CommandManager.Command.Cut,
					lastFocusOwner.isEditable() &&
					hasSelection
					);
				vp.setCommandEnabled(CommandManager.Command.Copy,
					hasSelection
					);
				vp.setCommandEnabled(CommandManager.Command.Paste,
					lastFocusOwner.isEditable()
					);
				vp.setCommandEnabled(CommandManager.Command.SelectAll,
					true);
				vp.setCommandEnabled(CommandManager.Command.DeselectAll,
					true);
			}
		}
	}

	@Override
	public void grabFocus() {
		input.grabFocus();
	}

	public void attached(ViewPort vp) {
		// refuse to allow us to be closed
		vp.preventViewClosure();
		input.requestFocusInWindow();
	}

	public void detached(ViewPort vp) {
        vp.setCommandEnabled(CommandManager.Command.Cut, false);
        vp.setCommandEnabled(CommandManager.Command.Copy, false);
        vp.setCommandEnabled(CommandManager.Command.Paste, false);
        vp.setCommandEnabled(CommandManager.Command.SelectAll, false);
        vp.setCommandEnabled(CommandManager.Command.DeselectAll, false);
	}

	public void cleanUp() {
	}

	public boolean isSaved() {
		return true;
	}

	public void commandTriggered(String command) {
		if (CommandManager.Command.Cut.matches(command)) {
			if (lastFocusOwner != null)
				lastFocusOwner.cut();
		}
		else if (CommandManager.Command.Copy.matches(command)) {
			if (lastFocusOwner != null)
				lastFocusOwner.copy();
		}
		else if (CommandManager.Command.Paste.matches(command)) {
			if (lastFocusOwner != null)
				lastFocusOwner.paste();
		}
		else if (CommandManager.Command.SelectAll.matches(command)) {
			if (lastFocusOwner != null)
				lastFocusOwner.selectAll();
		}
		else if (CommandManager.Command.DeselectAll.matches(command)) {
			if (lastFocusOwner != null)
				lastFocusOwner.select(0, 0);
		}
	}

	public void refresh() {
	}
}
