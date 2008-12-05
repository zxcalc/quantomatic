package quanto.gui;

import java.awt.BorderLayout;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.*;
import javax.swing.*;

import edu.uci.ics.jung.algorithms.layout.FRLayout;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;


class JTextAreaOutputStream extends OutputStream {
	JTextArea textArea;
	public JTextAreaOutputStream(JTextArea textArea) {
		this.textArea = textArea;
	}
	@Override
	public void write(int b) throws IOException {
		textArea.append(String.valueOf((char)b));
		textArea.setCaretPosition(textArea.getDocument().getLength()-1);
	}
	
}

public class QuantoConsole extends JPanel {
	private static final long serialVersionUID = -5833674157230451213L;
	public PrintStream out;
	public QuantoCore qcore;
	private QuantoVisualizer boundContext;
	JTextField input;
	JTextArea output;
	public QuantoConsole() {
        this.setLayout(new BorderLayout());
        input = new JTextField();
        
        
        output = new JTextArea(10,0);
        output.setFocusable(false);
        out = new PrintStream(new JTextAreaOutputStream(output));
		qcore = new QuantoCore(out);
		
		
		input.addKeyListener(new KeyListener () {
        	public void keyPressed(KeyEvent e) {}
        	public void keyTyped(KeyEvent e) {}
        	
			public void keyReleased(KeyEvent e) {
				if (e.getKeyCode() == KeyEvent.VK_ENTER) {
					JTextField tf = (JTextField)e.getSource();
					String text = tf.getText();
					write(text);
					tf.setText("");
				}
			}        	
        });
		
		
        this.add(new JScrollPane(output),BorderLayout.CENTER);
        this.add(input,BorderLayout.SOUTH);
	}
	
	public synchronized void write(String text) {
		out.println("> ".concat(text));
		qcore.send(text.concat("\n"));
		qcore.send("D\n");
		if (boundContext == null) {
			out.println("ERROR: No graph bound to this console.");
		} else {
			
			boundContext.getGraph().fromXml(qcore.receive());
			//boundContext.animateToNewLayout();
			boundContext.getModel().getRelaxer().resume();
			out.println("OK");
		}
	}

	public QuantoVisualizer getBoundContext() {
		return boundContext;
	}

	public void bindContext(QuantoVisualizer boundContext) {
		this.boundContext = boundContext;
	}

}