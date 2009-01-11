package quanto.gui;

import java.awt.BorderLayout;
import java.awt.event.*;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

@SuppressWarnings("serial")
public class Labeler extends JPanel implements MouseListener, KeyListener, FocusListener {
	JLabel label;
	JTextField textField;
	JComponent active;
	String value;
	ChangeEvent evt;
	
	public Labeler(String value) {
		setLayout(new BorderLayout());
		evt = new ChangeEvent(this);
		label = new JLabel();
		textField = new JTextField();
		setText(value);
		
		addMouseListener(this);
		textField.addKeyListener(this);
		textField.addFocusListener(this);
		active = label;
		add(active, BorderLayout.CENTER);
		refresh();
	}
	
	//@Override
	public void mouseClicked(MouseEvent e) {
		if (e.getClickCount()==2) {
			textField.setText(getText());
			remove(label);
			active = textField;
			add(active, BorderLayout.CENTER);
			active.grabFocus();
			refresh();
		}
	}
	
	//@Override
	public void keyReleased(KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_ENTER) {
			setText(textField.getText());
			remove(textField);
			active = label;
			add(active, BorderLayout.CENTER);
		}
		refresh();
	}
	
	//@Override
	public void focusLost(FocusEvent e) {
		remove(textField);
		active = label;
		add(active, BorderLayout.CENTER);
		refresh();
	}
	
	private void refresh() {
		revalidate();
		repaint();
	}
	
	public String getText() {
		return value;
	}
	
	public void setText(String text) {
		value = text;
		label.setText(value);
		refresh();
	}
	
	public void addChangeListener(ChangeListener l) {
		listenerList.add(ChangeListener.class, l);
	}
	
	public void fireStateChanged() {
		ChangeListener[] listeners =
			listenerList.<ChangeListener>getListeners(ChangeListener.class); 
		for (ChangeListener l : listeners) l.stateChanged(evt);
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
