package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.*;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

@SuppressWarnings("serial")
public class Labeler extends JPanel implements MouseListener, KeyListener, FocusListener {
	JLabel label;
	JTextField textField;
	JComponent active;
	public Labeler(String value) {
		setLayout(null);
		label = new JLabel(value);
		textField = new JTextField();
		setLayout(new BorderLayout());
		addMouseListener(this);
		textField.addKeyListener(this);
		textField.addFocusListener(this);
		active = label;
		refresh();
	}
	
	@Override
	public void mouseClicked(MouseEvent e) {
		if (e.getClickCount()==2) {
			textField.setText(label.getText());
			remove(label);
			active = textField;
			refresh();
		}
	}
	
	@Override
	public void keyReleased(KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_ENTER) {
			label.setText(textField.getText());
			remove(textField);
			active = label;
			refresh();
		}
	}
	
	@Override
	public void focusLost(FocusEvent e) {
		remove(textField);
		active = label;
		refresh();
	}
	
	private void refresh() {
		Dimension size = active.getPreferredSize();
		active.setSize(size);
		setSize((int)size.getWidth()+10, (int)size.getHeight()+10);
		add(active);
		active.grabFocus();
		repaint();
	}
	
	public String getText() {
		return label.getText();
	}
	
	public void setText(String text) {
		label.setText(text);
	}
	
	// stubs
	public void mouseEntered(MouseEvent e) {}
	public void mouseExited(MouseEvent e) {}
	public void mousePressed(MouseEvent e) {}
	public void mouseReleased(MouseEvent e) {}
	public void keyPressed(KeyEvent e) {}
	public void keyTyped(KeyEvent e) {}
	public void focusGained(FocusEvent e) {}
}
