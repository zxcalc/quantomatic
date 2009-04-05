package quanto.gui;

import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

public class TextView extends JPanel implements InteractiveView {
	private static final long serialVersionUID = -9201774497137020314L;
	private static int instanceCount=0; 
	//private String title;
	private JTextArea textArea;
	
	
	public TextView(String text) {
		//this.title = title;
		textArea = new JTextArea();
		textArea.setText(text);
		
		setLayout(new BorderLayout());
		add(new JScrollPane(textArea),BorderLayout.CENTER);
		
		instanceCount++;
	}
	
	public boolean viewHasParent() {
		return this.getParent() != null;
	}
	
//	public String getTitle() {
//		return title;
//	}

	public void viewFocus(ViewPort vp) {
		
	}
	
	public void viewUnfocus(ViewPort vp) {
		
	}

	public void viewKill(ViewPort vp) {
		// TODO Auto-generated method stub
		
	}

}
