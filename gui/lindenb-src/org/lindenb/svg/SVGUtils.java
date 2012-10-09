package org.lindenb.svg;

import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.PathIterator;
import java.awt.geom.Rectangle2D;

import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Scanner;
import java.util.logging.Logger;
import org.lindenb.awt.Dimension2D;
import org.lindenb.lang.InvalidXMLException;
import org.lindenb.svg.path.SVGPathParser;
import org.lindenb.sw.vocabulary.SVG;
import org.lindenb.util.StringUtils;
import org.lindenb.xml.XMLUtilities;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;


/**
 * Utilities for Scalable Vector Graphics
 * @author lindenb
 *
 */
public class SVGUtils extends SVG
{
	protected static final Logger LOG=Logger.getLogger("org.lindenb"); 
	
	public static double castUnit(String s)
		{
		s=s.trim();
		if(s.endsWith("px") || s.endsWith("pt") || s.endsWith("cm"))
			{
			s=s.substring(0,s.length()-2).trim();
			}
		if(s.endsWith("in"))
			{
			s=s.substring(0,s.length()-2).trim();
			return 75.0*Double.parseDouble(s);
			}
		return Double.parseDouble(s);	
		}
	
	/** return the dimension of a SVG document */
	public static Dimension2D getSize(Element svgRoot)throws InvalidXMLException
		{
		if(!XMLUtilities.isA(svgRoot, NS, "svg")) throw new InvalidXMLException(svgRoot,"not a svg:svg element");
		try
			{
			Dimension2D.Double srcSize=new Dimension2D.Double(0,0);
			Attr width= svgRoot.getAttributeNode("width");
			Attr height= svgRoot.getAttributeNode("height");
			
			if(width==null) throw new InvalidXMLException(svgRoot,"@width missing");
			srcSize.width= castUnit(width.getValue());
			
			if(height==null) throw new InvalidXMLException(svgRoot,"@height missing");
			srcSize.height= castUnit(height.getValue());
			return srcSize;
			}
		catch(NumberFormatException err)
			{
			throw new InvalidXMLException(err);
			}
		}
	
	static public AffineTransform svgToaffineTransform(String transform)
		{
		if(StringUtils.isBlank(transform)) return null;
		String s=transform.trim();
		
		if(s.startsWith("matrix("))
			{
			int i=s.indexOf(")");
			if(i==-1) throw new IllegalArgumentException(s);
			if(!StringUtils.isBlank(s.substring(i+1))) throw new IllegalArgumentException(s);
			String tokens[]=s.substring(7, i).split("[,]");
			if(tokens.length!=6) throw new IllegalArgumentException(s);
			return new AffineTransform(new double[]{
				Double.parseDouble(tokens[0]),
				Double.parseDouble(tokens[1]),
				Double.parseDouble(tokens[2]),
				Double.parseDouble(tokens[3]),
				Double.parseDouble(tokens[4]),
				Double.parseDouble(tokens[5])
				});
			}
		AffineTransform tr= new AffineTransform();
		while(s.length()!=0)
			{
		
			
			if(s.startsWith("scale("))
				{
				int i=s.indexOf(")");
				if(i==-1) throw new IllegalArgumentException(s);
				
				String s2= s.substring(6,i).trim();
				s= s.substring(i+1).trim();
				i= s2.indexOf(',');
				if(i==-1)
					{
					double scale= Double.parseDouble(s2.trim());
					
					AffineTransform tr2= AffineTransform.getScaleInstance(
							scale,scale
						);
					tr2.concatenate(tr);
					tr=tr2;
					}
				else
					{
					double scalex= Double.parseDouble(s2.substring(0,i).trim());
					double scaley= Double.parseDouble(s2.substring(i+1).trim());
					
					AffineTransform tr2= AffineTransform.getScaleInstance(
							scalex,scaley
						);
					tr2.concatenate(tr);
					tr=tr2;
					}
				}
			else if(s.startsWith("translate("))
				{
				int i=s.indexOf(")");
				if(i==-1) throw new IllegalArgumentException(s);
				String s2= s.substring(10,i).trim();
				s= s.substring(i+1).trim();
				i= s2.indexOf(',');
				if(i==-1)
					{
					double translate= Double.parseDouble(s2.trim());
					
					AffineTransform tr2= AffineTransform.getTranslateInstance(
							translate,0
						);
					tr2.concatenate(tr);
					tr=tr2;
					}
				else
					{
					double translatex= Double.parseDouble(s2.substring(0,i).trim());
					double translatey= Double.parseDouble(s2.substring(i+1).trim());
					
					AffineTransform tr2= AffineTransform.getTranslateInstance(
							translatex,translatey
						);
					tr2.concatenate(tr);
					tr=tr2;
					}
				}
			else if(s.startsWith("rotate("))
				{
				int i=s.indexOf(")");
				if(i==-1) throw new IllegalArgumentException(s);
				String s2= s.substring(7,i).trim();
				s= s.substring(i+1).trim();
				i= s2.indexOf(',');
				if(i==-1)
					{
					double angle= Double.parseDouble(s2.trim());
					
					AffineTransform tr2= AffineTransform.getRotateInstance((angle/180.0)*Math.PI);
					tr2.concatenate(tr);
					tr=tr2;
					}
				else
					{
					double angle= Double.parseDouble(s2.substring(0,i).trim());
					s2=s2.substring(i+1);
					i= s2.indexOf(',');
					if(i==-1) throw new IllegalArgumentException("bad rotation "+s);
					
					double cx= Double.parseDouble(s2.substring(0,i).trim());
					double cy= Double.parseDouble(s2.substring(i+1).trim());
					
					AffineTransform tr2= AffineTransform.getRotateInstance(
							angle,cx,cy
						);
					tr2.concatenate(tr);
					tr=tr2;
					}
				}
			else if(s.startsWith("skewX("))
				{
				int i=s.indexOf(")");
				if(i==-1) throw new IllegalArgumentException(s);
				String s2= s.substring(6,i).trim();
				s= s.substring(i+1).trim();
				
				double shx= Double.parseDouble(s2.trim());
				
				AffineTransform tr2= AffineTransform.getShearInstance(shx, 1f);
				tr2.concatenate(tr);
				tr=tr2;
				}
			else if(s.startsWith("skewY("))
				{
				int i=s.indexOf(")");
				if(i==-1) throw new IllegalArgumentException(s);
				String s2= s.substring(6,i).trim();
				s= s.substring(i+1).trim();
				
				double shy= Double.parseDouble(s2.trim());
				
				AffineTransform tr2= AffineTransform.getShearInstance(1f,shy);
				tr2.concatenate(tr);
				tr=tr2;
				}
			
			}
		return tr;
		}
	
	/**
	 * transform a shape into a SVG path as String
	 * @param shape the shape
	 * @return the SVG points for &lt;path&gt;
	 */
	static public String shapeToPath(Shape shape)
	{
		StringWriter out= new StringWriter();
		shapeToPath(out,shape);
		return out.toString();
	}

	
	
	
	/**
	 * transform a shape into a SVG path
	 * @param shape
	 * @return
	 */
	static public void shapeToPath(Writer out,Shape shape)
	{
		PrintWriter path= new PrintWriter(out);

		double tab[] = new double[6];
		PathIterator pathiterator = shape.getPathIterator(null);

		while(!pathiterator.isDone())
		{
			int currSegmentType= pathiterator.currentSegment(tab);
			switch(currSegmentType) {
			case PathIterator.SEG_MOVETO: {
				path.print( "M " + (tab[0]) + " " + (tab[1]) + " ");
				break;
			}
			case PathIterator.SEG_LINETO: {
				path.print( "L " + (tab[0]) + " " + (tab[1]) + " ");
				break;
			}
			case PathIterator.SEG_CLOSE: {
				path.print( "Z ");
				break;
			}
			case PathIterator.SEG_QUADTO: {
				path.print( "Q " + (tab[0]) + " " + (tab[1]));
				path.print( " "  + (tab[2]) + " " + (tab[3]));
				path.print( " ");
				break;
			}
			case PathIterator.SEG_CUBICTO: {
				path.print( "C " + (tab[0]) + " " + (tab[1]));
				path.print( " "  + (tab[2]) + " " + (tab[3]));
				path.print( " "  + (tab[4]) + " " + (tab[5]));
				path.print( " ");
				break;
			}
			default:
			{
				LOG.info("Cannot handled "+currSegmentType);
				break;
			}
			}
			pathiterator.next();
		}
		path.flush();
	}

	
	public static GeneralPath polygonToShape(String lineString )
		{
		GeneralPath p = polylineToShape(lineString);
		p.closePath();
		return p;
		}
	
	public static GeneralPath polylineToShape(String lineString )
		{
		GeneralPath p = new GeneralPath(GeneralPath.WIND_EVEN_ODD);
		Scanner scanner= new Scanner(new StringReader(lineString));
		scanner.useDelimiter("[ \n,\t]+");
		
		boolean found=false;
		Double prev=null;
		while(scanner.hasNext())
			{
			String s=scanner.next();
			if(s.length()==0) continue;
			double v= Double.parseDouble(s);
			if(prev==null)
				{
				prev=v;
				}
			else
				{
				if(!found)
					{
					p.moveTo(prev, v);
					found=true;
					}
				else
					{
					p.lineTo(prev, v);
					}
				prev=null;
				}
			}
		if(prev!=null) throw new IllegalArgumentException("bad polyline "+lineString);
		return p;
		}
	
	/**
	 * @param pathString the path string
	 * @return
	 */
	public static Shape pathToShape(String pathString )
		{
		return SVGPathParser.parse(pathString);
		}

}




