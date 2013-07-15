package org.lindenb.awt;
import java.awt.BasicStroke;

/**
 * SVG definition for line cap
 */
public enum Cap {
BUTT() { public int stroke() { return BasicStroke.CAP_BUTT;}},
ROUND { public int stroke() { return BasicStroke.CAP_ROUND;}},
SQUARE { public int stroke() { return BasicStroke.CAP_SQUARE;}};
/** return the value as a java.awt.BasicStroke.CAP_* */
public abstract int stroke();
/** return the value as a SVG string style */
public String svg()
    {
    return name().toLowerCase();
    }

public static Cap parseSVG(String style)
    {
    for(Cap j: values())
        {
        if(j.svg().equals(style)) return j;
        }
    throw new IllegalArgumentException("Bad cap "+style);
    }
}
