import java.awt.BorderLayout;
import java.awt.Frame;


public class QuantoFrame extends Frame {
	private static final long serialVersionUID = 1L;

	public QuantoFrame() {
		super("Quanto");

        setLayout(new BorderLayout());
        setSize(QuantoApplet.WIDTH,QuantoApplet.HEIGHT);
        QuantoApplet embed = new QuantoApplet();
        add(embed, BorderLayout.CENTER);

        // important to call this whenever embedding a PApplet.
        // It ensures that the animation thread is started and
        // that other internal variables are properly set.
        embed.init();
	}
	
	public static void main(String[] args) {
		new QuantoFrame().setVisible(true);
	}

}
