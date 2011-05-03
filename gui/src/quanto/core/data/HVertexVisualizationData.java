package quanto.core.data;

import com.kitfox.svg.app.beans.SVGIcon;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Shape;
import java.awt.geom.Dimension2D;
import java.awt.geom.Rectangle2D;
import java.net.URI;
import java.net.URL;
import javax.swing.Icon;

public class HVertexVisualizationData implements VertexVisualizationData {

	SVGIcon cachedIcon = new SVGIcon();

	public HVertexVisualizationData() {
		URL hsvg = getClass().getResource("resources/hadamard.svg");
		URI svgdocURI = cachedIcon.getSvgUniverse().loadSVG(hsvg);
		cachedIcon.setSvgURI(svgdocURI);
		cachedIcon.setScaleToFit(true);
		cachedIcon.setAntiAlias(false);
	}

	public Shape getShape(Rectangle2D bounds) {
		return bounds;
	}

	public Color getFillColour() {
		return Color.yellow;
	}

	public Color getLabelColour() {
		return null;
	}

	public Icon getIcon(Dimension2D size) {
		Dimension d = new Dimension((int)size.getWidth(), (int)size.getHeight());
		if (!cachedIcon.getPreferredSize().equals(d))
			cachedIcon.setPreferredSize(d);
		return cachedIcon;
	}
}
