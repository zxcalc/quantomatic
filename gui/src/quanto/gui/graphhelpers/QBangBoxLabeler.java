package quanto.gui.graphhelpers;

import edu.uci.ics.jung.contrib.visualization.renderers.BangBoxLabelRenderer;
import java.awt.Component;
import java.awt.Font;
import javax.swing.JComponent;
import javax.swing.JLabel;

/**
 *
 * @author alemer
 */
public class QBangBoxLabeler implements BangBoxLabelRenderer {

	private JLabel dummyLabel = new JLabel();

	public QBangBoxLabeler() {
	}

	public <T> Component getBangBoxLabelRendererComponent(JComponent vv,
			Object value, Font font, boolean isSelected, T edge) {
		return dummyLabel;
	}
}
