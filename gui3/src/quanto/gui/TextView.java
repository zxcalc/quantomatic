package quanto.gui;

import java.awt.BorderLayout;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JMenu;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

public class TextView extends JPanel implements InteractiveView {
	private static final long serialVersionUID = -9201774497137020314L;
	private static int instanceCount=0; 
	private String title;
	private JTextArea textArea;
	
	public TextView(String text) {
		this("output ("+Integer.toString(instanceCount)+")", text);
	}
	
	public TextView(String title, String text) {
		this.title = title;
		textArea = new JTextArea();
		textArea.setText(text);
		
		setLayout(new BorderLayout());
		add(new JScrollPane(textArea),BorderLayout.CENTER);
		
		instanceCount++;
	}

	public List<JMenu> getMenus() {
		return new ArrayList<JMenu>();
	}
	
	public String getTitle() {
		return title;
	}

	public void setViewHolder(Holder h) {
	}

}
