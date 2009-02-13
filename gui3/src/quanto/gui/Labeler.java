package quanto.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
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
	Point idealLocation;
	
	public Labeler(String value) {
		setLayout(new BorderLayout());
		evt = new ChangeEvent(this);
		label = new JLabel();
		label.setOpaque(false);
		setColor(Color.yellow);
		textField = new JTextField();
		setText(value);
		
		addMouseListener(this);
		textField.addKeyListener(this);
		textField.addFocusListener(this);
		active = label;
		add(active, BorderLayout.CENTER);
		refresh();
	}
	
	@Override
	public void setLocation(Point p) {
		super.setLocation(p);
	}
	
	public Point getIdealLocation() {
		return idealLocation;
	}
	
	public void setIdealLocation(Point p) {
		idealLocation = p;
	}
	
	public void setColor(Color c) {
		setBackground(c);
		setBorder(new LineBorder(c,1));
	}
	
	public void paint(Graphics g) { 
        //Graphics2D g2 = (Graphics2D) g.create(); 
        //g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.6f));
        super.paint(g); 
        //g2.dispose();
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
			updateLabel();
		}
		refresh();
	}
	
	//@Override
	public void focusLost(FocusEvent e) {
		updateLabel();
		refresh();
	}
	
	public boolean isBeingEdited() {
		return active == textField;
	}
	
	private void updateLabel() {
		String old = getText();
		setText(textField.getText());
		remove(textField);
		active = label;
		add(active, BorderLayout.CENTER);
		if (!old.equals(getText())) fireStateChanged();
	}
	
	private void refresh() {
		revalidate();
		// along with keeping the bounds, this forces a redraw
		setBounds(new Rectangle(getPreferredSize()));
		repaint();
	}
	
	public String getText() {
		return value;
	}
	
	public void setText(String text) {
		value = text;
		label.setText(TexConstants.translate(value));
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
