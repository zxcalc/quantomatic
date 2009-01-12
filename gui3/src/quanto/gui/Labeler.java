package quanto.gui;

import java.awt.AlphaComposite;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.*;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.LineBorder;
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
		setBackground(Color.yellow);
		setBorder(new LineBorder(Color.yellow,1));
		textField = new JTextField();
		setText(value);
		
		addMouseListener(this);
		textField.addKeyListener(this);
		textField.addFocusListener(this);
		active = label;
		add(active, BorderLayout.CENTER);
		refresh();
	}
	
	public void paint(Graphics g) { 
        Graphics2D g2 = (Graphics2D) g.create(); 
        g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.75f)); 
        super.paint(g2); 
        g2.dispose(); 
    }
	
	//@Override
	public void mouseClicked(MouseEvent e) {
		if (e.getClickCount()==2) {
			textField.setText(getText());
			remove(label);
			add(textField, BorderLayout.CENTER);
			textField.grabFocus();
			textField.selectAll();
			active = textField;
			refresh();
		}
	}
	
	//@Override
	public void keyReleased(KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_ENTER) {
			String old = getText();
			setText(textField.getText());
			remove(textField);
			active = label;
			add(active, BorderLayout.CENTER);
			if (!old.equals(getText())) fireStateChanged();
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
