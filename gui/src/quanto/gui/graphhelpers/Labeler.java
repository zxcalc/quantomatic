package quanto.gui.graphhelpers;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.*;
import java.util.EventListener;
import java.util.EventObject;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import quanto.core.data.GraphElementData;

@SuppressWarnings("serial")
public class Labeler extends JPanel implements MouseListener, KeyListener, FocusListener {
    
	public class LabelChangeEvent extends EventObject {
		private String oldText;
		private String newText;

		public LabelChangeEvent(Object source, String oldText, String newText) {
			super(source);
			this.oldText = oldText;
			this.newText = newText;
		}

		public String getOldText() {
			return oldText;
		}

		public String getNewText() {
			return newText;
		}
	}

	public interface LabelChangeListener extends EventListener {
		boolean aboutToChangeLabel(LabelChangeEvent evt);
	}

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
		String oldText = getText();
		String newText = textField.getText();
		if (oldText.equals(newText))
			return;
		if (fireAboutToChangeLabel(oldText, newText)) {
			setText(newText);
		} else {
			textField.setText(oldText);
		}
		remove(textField);
		active = label;
		add(active, BorderLayout.CENTER);
	}

	private void refresh() {
		revalidate();
		// along with keeping the bounds, this forces a redraw
		setBounds(new Rectangle(getPreferredSize()));
		repaint();
	}
	
	public void update() {
		label.setText(data.getDisplayString());
		refresh();
	}

	public String getText() {
		return data.getEditableString();
	}

	public final void setText(String text) {
		data.setString(text);
		update();
	}

	public void addLabelChangeListener(LabelChangeListener l) {
		listenerList.add(LabelChangeListener.class, l);
	}

	public boolean fireAboutToChangeLabel(String oldValue, String newValue) {
		LabelChangeListener[] listeners =
				listenerList.<LabelChangeListener>getListeners(LabelChangeListener.class);
		LabelChangeEvent evt = new LabelChangeEvent(this, oldValue, newValue);
		for (LabelChangeListener l : listeners) {
			if (!l.aboutToChangeLabel(evt))
				return false;
		}
		return true;
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
