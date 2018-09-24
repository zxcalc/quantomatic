package org.lindenb.awt;
import java.awt.BasicStroke;
/**
 * SVG definition for line join
 */
public enum Join {
BEVEL(){ public int stroke() { return BasicStroke.JOIN_BEVEL;}},
ROUND(){ public int stroke() { return BasicStroke.JOIN_ROUND;}},
MITER(){ public int stroke() { return BasicStroke.JOIN_MITER;}};
/** return the value as a java.awt.BasicStroke.JOIN_* */
public abstract int stroke();
/** return the value as a SVG string style */
public String svg()
    {
    return name().toLowerCase();
    }

public static Join parseSVG(String style)
    {
    for(Join j: values())
        {
        if(j.svg().equals(style)) return j;
        }
    throw new IllegalArgumentException("Bad join "+style);
    }

}
