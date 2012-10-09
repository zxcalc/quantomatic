package quanto.gui.graphhelpers;

import java.awt.BorderLayout;
import java.awt.Color;
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


import quanto.core.data.GraphElementData;
import quanto.core.data.VertexType;
import quanto.core.data.TexConstants;

@SuppressWarnings("serial")
public class Labeler extends JPanel implements MouseListener, KeyListener, FocusListener {

	JLabel label;
	JTextField textField;
	JComponent active;
	ChangeEvent evt;
	Point idealLocation;
	GraphElementData data;

	public Labeler(GraphElementData data) {
		setLayout(new BorderLayout());
		this.data = data;
		evt = new ChangeEvent(this);
		label = new JLabel();
		label.setOpaque(false);
		setColor(Color.yellow);
		textField = new JTextField();
		label.setText(data.getDisplayString());

		addMouseListener(this);
		textField.addKeyListener(this);
		textField.addFocusListener(this);
		active = label;
		add(active, BorderLayout.CENTER);
		refresh();
	}

	public Labeler(String value) {
		this(new GraphElementData(value));
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

	public final void setColor(Color c) {
		setBackground(c);
		setBorder(new LineBorder(c, 1));
	}

	//@Override
	public void mouseClicked(MouseEvent e) {
		if (e.getClickCount() == 2) {
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
		if (!old.equals(getText())) {
			fireStateChanged();
		}
	}

	private void refresh() {
		revalidate();
		// along with keeping the bounds, this forces a redraw
		setBounds(new Rectangle(getPreferredSize()));
		repaint();
	}
	
	public void update() {
		label.setText(data.getDisplayString());
		textField.setText(data.getEditableString());
		refresh();
	}

	public String getText() {
		return data.getEditableString();
	}

	public final void setText(String text) {
		data.setString(text);
		label.setText(data.getDisplayString());
		refresh();
	}

	public void addChangeListener(ChangeListener l) {
		listenerList.add(ChangeListener.class, l);
	}

	public void fireStateChanged() {
		ChangeListener[] listeners =
				listenerList.<ChangeListener>getListeners(ChangeListener.class);
		for (ChangeListener l : listeners) {
			l.stateChanged(evt);
		}
	}

	// stubs
	public void mouseEntered(MouseEvent e) {
	}

	public void mouseExited(MouseEvent e) {
	}

	public void mousePressed(MouseEvent e) {
	}

	public void mouseReleased(MouseEvent e) {
	}

	public void keyPressed(KeyEvent e) {
	}

	public void keyTyped(KeyEvent e) {
	}

	public void focusGained(FocusEvent e) {
	}
}
