package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.*;
import java.util.regex.Pattern;

import javax.swing.*;


public class QuantoConsole extends JPanel {
	private static final long serialVersionUID = -5833674157230451213L;
	public PrintStream out;
	public QuantoCore qcore;
	JTextField input;
	JTextArea output;
	final Pattern graph_xml = Pattern.compile("^GRAPH\\_XML (.+)");
	
	class QuantoConsoleOutputStream extends OutputStream {
		JTextArea textArea;
		public QuantoConsoleOutputStream(JTextArea textArea) {
			this.textArea = textArea;
		}
		@Override
		public void write(int b) throws IOException {
			textArea.append(String.valueOf((char)b));
			textArea.setCaretPosition(textArea.getDocument().getLength()-1);
		}
		
	}
	
	public QuantoConsole() {
        this.setLayout(new BorderLayout());
        input = new JTextField();
        output = new JTextArea();
        output.setFocusable(false);
        out = new PrintStream(new QuantoConsoleOutputStream(output));
		qcore = new QuantoCore(out);
		
		// print the prompt
		out.print(qcore.receive());
		
		input.addKeyListener(new KeyAdapter () {
			public void keyReleased(KeyEvent e) {
				if (e.getKeyCode() == KeyEvent.VK_ENTER) {
					JTextField tf = (JTextField)e.getSource();
					String text = tf.getText();
					write(text);
					tf.setText("");
				}
			}
        });

		JScrollPane scroll = new JScrollPane(output);
		scroll.setPreferredSize(new Dimension(800, 200));
        this.add(scroll,BorderLayout.CENTER);
        this.add(input,BorderLayout.SOUTH);
	}
	
	public void write(String text) {
		synchronized (qcore) {
			try {
				out.println(text);
				qcore.send(text);
				String rcv = qcore.receiveOrFail();
				out.print(rcv);
			} catch (QuantoCore.ConsoleError e) {
				out.print("ERROR: ".concat(e.getMessage()));
			}
			
			// print the prompt
			out.print(qcore.receive());
		}
	}
	
	public void grabFocus() {
		input.grabFocus();
	}

}