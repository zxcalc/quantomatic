package org.lindenb.svg;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Composite;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;

import org.lindenb.awt.Cap;
import org.lindenb.awt.ColorUtils;
import org.lindenb.awt.Dimension2D;
import org.lindenb.awt.Join;
import org.lindenb.lang.InvalidXMLException;
import org.lindenb.sw.vocabulary.SVG;
import org.lindenb.util.StringUtils;
import org.lindenb.xml.XMLUtilities;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 * A Basic Renderer for Scalable Vector Graphics SVG
 * @author lindenb
 *
 */
public class SVGRenderer
	extends SVGUtils
	{
	
	public SVGRenderer()
		{	
		}

	
	/**
	 * Record the Graphic context for a given node 
	 * @author lindenb
	 *
	 */
	private static class Shuttle
		{
		/** current graphics2D */
		Graphics2D g;
		/** the svg:svg root */
		Element svgRoot;
		/** current node */
		Element node;
		/** current clip */
		Shape clip;
		/** current shape */
		Shape shape;
		float fontSize=12;
		String fontFamily=null;
		Paint fill=null;
		Paint stroke=null;
		float strokeWidth;
		Cap linecap=Cap.BUTT;
		Join linejoin=Join.MITER;
		AffineTransform transform=new AffineTransform();
		float opacity=1f;
		
		public Shuttle(Graphics2D g,Element root)
			{
			this.g=g;
			
			Font font= g.getFont();
			this.fontSize= font.getSize();
			this.fontFamily= font.getFamily();
			this.svgRoot=root;
			this.node=root;
			this.transform=new AffineTransform(g.getTransform());
			this.clip= g.getClip();
			}
		
		Shuttle(Shuttle cp,Element e)
			{
			this.node=e;
			this.g = cp.g;
			this.svgRoot = cp.svgRoot;
			this.shape=cp.shape;
			this.clip=cp.clip;
			this.fontSize=cp.fontSize;
			this.fontFamily=cp.fontFamily;
			this.stroke=cp.stroke;
			this.fill=cp.fill;
			this.strokeWidth=cp.strokeWidth;
			this.transform=new AffineTransform(cp.transform);
			this.opacity=cp.opacity;
			if(e.hasAttributes())
				{
				NamedNodeMap atts=e.getAttributes();
				for(int i=0;i< atts.getLength();++i)
					{
					
					Attr att= Attr.class.cast(atts.item(i));
					if(att.getNamespaceURI()!=null) continue;
					String s=att.getName();
					String value= att.getValue();
					if(s.equals("style"))
						{
						for(String styles:value.split("[;]+"))
							{
							int j=styles.indexOf(':');
							if(j!=-1)
								{
								applyStyle(styles.substring(0,j).trim(),styles.substring(j+1).trim());
								}
							}
						
						}
					else
						{
						applyStyle(s,att.getValue());
						}
					}
				}
			}
		private void applyStyle(String key,String value)
			{
			if(key.equals("fill"))
				{
				if(value!=null && value.equals("none")) return;
				this.fill= ColorUtils.parseColor(value);
				}
			else if(key.equals("stroke"))
				{
				if(value!=null && value.equals("none")) return;
				this.stroke= ColorUtils.parseColor(value);
				}
			else if(key.equals("stroke-width"))
				{
				this.strokeWidth= Float.parseFloat(value);
				}
			else if(key.equals("stroke-linecap"))
				{
				this.linecap= Cap.parseSVG(value);
				}
			else if(key.equals("stroke-linejoin"))
				{
				this.linejoin=Join.parseSVG(value);
				}
			else if(key.equals("transform"))
				{
				AffineTransform tr=svgToaffineTransform(value);
				this.transform.concatenate(tr);
				}
			else if(key.equals("font-size"))
				{
				this.fontSize= (float)castUnit(value);
				}
			else if(key.equals("font-family"))
				{
				this.fontFamily= value;
				}
			else if(key.equals("opacity"))
				{
				this.opacity *= Float.parseFloat(value);
				}
			
			
			}
		
		}
	
	public void paint(
			Graphics2D g,
			Node dom
			) throws InvalidXMLException
		{
		paint(g,dom,null);
		}
	
	
	public void paint(
			Graphics2D g,
			Node dom,
			Rectangle2D viewRect
			) throws InvalidXMLException
		{
		
		if(g==null) throw new NullPointerException("g is null");
		if(dom==null) throw new NullPointerException("dom is null");
		Element root=null;
		if(dom.getNodeType()==Node.DOCUMENT_NODE)
			{
			root= Document.class.cast(dom).getDocumentElement();
			}
		else if(dom.getNodeType()==Node.ELEMENT_NODE)
			{
			root =  Element.class.cast(dom);
			}
		
		if(root==null) throw new InvalidXMLException(dom,"no root");
		if(!XMLUtilities.isA(root, SVG.NS, "svg")) throw new InvalidXMLException(root,"not a SVG root");
		
		Dimension2D srcSize= getSize(root);
		Rectangle2D viewBox=null;
		Attr viewBoxAttr = root.getAttributeNode("viewBox");
		if(viewBoxAttr!=null)
			{
			String tokens[]= viewBoxAttr.getValue().trim().split("[ \t\n]+");
			if(tokens.length!=4) throw new InvalidXMLException(viewBoxAttr,"invalid ");
			viewBox = new  Rectangle2D.Double(
				Double.parseDouble(tokens[0]),
				Double.parseDouble(tokens[1]),
				Double.parseDouble(tokens[2]),
				Double.parseDouble(tokens[3])
				);
			srcSize= new Dimension2D.Double(viewBox.getWidth(),viewBox.getHeight());
			}
		
			
		AffineTransform originalTr=null;
		if(viewRect!=null)
			{
			if(srcSize.getWidth()>0 && srcSize.getHeight()>0)
				{
				originalTr= g.getTransform();
				
				double ratio= Math.min(
						viewRect.getWidth()/srcSize.getWidth(),
						viewRect.getHeight()/srcSize.getHeight()
					);
				
				g.translate(
						(viewRect.getWidth() -srcSize.getWidth()*ratio)/2.0,
						(viewRect.getHeight()-srcSize.getHeight()*ratio)/2.0
						);
				g.scale(ratio,ratio);
				}
			}
		Shape oldclip= g.getClip();
		Shuttle shuttle=new Shuttle(g,root);
		if(viewBox!=null)
			{
			AffineTransform tr= AffineTransform.getTranslateInstance(
				-viewBox.getX(),
				-viewBox.getY()
				);
			shuttle.transform.concatenate(tr);
			Area area= new Area(new Rectangle2D.Double(
				0,0,
				viewBox.getWidth(),
				viewBox.getHeight()
				));
			if (shuttle.clip != null)
				area.intersect(new Area(shuttle.clip));
			shuttle.clip= area;
			}
		
		paint(shuttle);
		
		
		if(originalTr!=null)
			{
			g.setTransform(originalTr);
			}
		g.setClip(oldclip);
		}
	
	private void paint(Shuttle shuttle) throws InvalidXMLException
		{
		Element e= shuttle.node;
		String shapeName= e.getLocalName();
		
		if(!SVG.NS.equals(e.getNamespaceURI()))
			{
			
			for(Node c=e.getFirstChild();c!=null;c=c.getNextSibling())
				{
				if(c.getNodeType()!=Node.ELEMENT_NODE) continue;
				Shuttle cp= new Shuttle(shuttle,Element.class.cast(c));
				paint(cp);
				}
			}
		else if(shapeName==null)
			{
			LOG.warning("shapeName is null");
			}
		else if(shapeName.equals("g"))
			{
			
			for(Node c=e.getFirstChild();c!=null;c=c.getNextSibling())
				{
				if(c.getNodeType()!=Node.ELEMENT_NODE) continue;
				Shuttle cp= new Shuttle(shuttle,Element.class.cast(c));
				paint(cp);
				}
			}
		else if(shapeName.equals("path"))
			{
			Attr d= e.getAttributeNode("d");
			if(d!=null)
				{
				shuttle.shape = SVGUtils.pathToShape(d.getValue());
				drawShape(shuttle);
				}
			}
		else if(shapeName.equals("polyline"))
			{
			Attr points= e.getAttributeNode("points");
			if(points!=null)
				{
				shuttle.shape = SVGUtils.polylineToShape(points.getValue());
				drawShape(shuttle);
				}
			}
		else if(shapeName.equals("polygon"))
			{
			Attr points= e.getAttributeNode("points");
			if(points!=null)
				{
				shuttle.shape = SVGUtils.polygonToShape(points.getValue());
				drawShape(shuttle);
				}
			}
		else if(shapeName.equals("rect"))
			{
			
			Attr x= e.getAttributeNode("x");
			Attr y= e.getAttributeNode("y");
			Attr w= e.getAttributeNode("width");
			Attr h= e.getAttributeNode("height");
			if(x!=null && y!=null && w!=null && h!=null)
				{
				shuttle.shape =new Rectangle2D.Double(
					Double.parseDouble(x.getValue()),
					Double.parseDouble(y.getValue()),	
					Double.parseDouble(w.getValue()),	
					Double.parseDouble(h.getValue())
					);
				drawShape(shuttle);
				}
			}
		else if(shapeName.equals("line"))
			{
			Attr x1= e.getAttributeNode("x1");
			Attr y1= e.getAttributeNode("y1");
			Attr x2= e.getAttributeNode("x2");
			Attr y2= e.getAttributeNode("y2");
			if(x1!=null && y1!=null && x2!=null && y2!=null)
				{
				shuttle.shape =new Line2D.Double(
					Double.parseDouble(x1.getValue()),
					Double.parseDouble(y1.getValue()),	
					Double.parseDouble(x2.getValue()),	
					Double.parseDouble(y2.getValue())
					);
				drawShape(shuttle);
				}
			}
		else if(shapeName.equals("circle"))
			{
			Attr cx= e.getAttributeNode("cx");
			Attr cy= e.getAttributeNode("cy");
			Attr r= e.getAttributeNode("r");
			if(cx!=null && cy!=null && r!=null)
				{
				double radius=Double.parseDouble(r.getValue());
				shuttle.shape =new Ellipse2D.Double(
					Double.parseDouble(cx.getValue())-radius,
					Double.parseDouble(cy.getValue())-radius,	
					radius*2,	
					radius*2
					);
				drawShape(shuttle);
				}
			}
		else if(shapeName.equals("ellipse"))
			{
			Attr cx= e.getAttributeNode("cx");
			Attr cy= e.getAttributeNode("cy");
			Attr rx= e.getAttributeNode("rx");
			Attr ry= e.getAttributeNode("ry");
			if(cx!=null && cy!=null && rx!=null && ry!=null)
				{
				double radiusx=Double.parseDouble(rx.getValue());
				double radiusy=Double.parseDouble(ry.getValue());
				shuttle.shape =new Ellipse2D.Double(
					Double.parseDouble(cx.getValue())-radiusx,
					Double.parseDouble(cy.getValue())-radiusy,	
					radiusx*2,	
					radiusy*2
					);
				drawShape(shuttle);
				}
			}
		else if(StringUtils.isIn(shapeName,"title","defs","desc","metadata"))
			{
			//ignore
			}
		else if(shapeName.equals("text"))
			{
			Attr x= e.getAttributeNode("x");
			Attr y= e.getAttributeNode("y");
			if(x!=null && y!=null)
				{
				
				Font f= new Font(shuttle.fontFamily,Font.PLAIN,(int)shuttle.fontSize);
				
				FontRenderContext frc = shuttle.g.getFontRenderContext();
		        TextLayout tl = new TextLayout(e.getTextContent(), f, frc);
		        shuttle.shape= tl.getOutline(null);
		        shuttle.shape = AffineTransform.getTranslateInstance(
		        		Double.parseDouble(x.getValue()),
		        		Double.parseDouble(y.getValue())).createTransformedShape(shuttle.shape)
		        		;

				drawShape(shuttle);
				}
			}
		else if(shapeName.equals("svg"))
			{
			
			for(Node c=e.getFirstChild();c!=null;c=c.getNextSibling())
				{
				if(c.getNodeType()!=Node.ELEMENT_NODE) continue;
				Shuttle cp = new Shuttle(shuttle,Element.class.cast(c));
				paint(cp);
				}
			}
		else
			{
			LOG.warning("cannot display <"+e.getLocalName()+">");
			}
		}
	
	
	private void drawShape(Shuttle shuttle)
		{
		Graphics2D g= shuttle.g;
		Shape oldclip=shuttle.g.getClip();
		g.setClip(shuttle.clip);
		Composite oldcomposite=g.getComposite();
		if(shuttle.opacity!=1f)
			{
			g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,shuttle.opacity));
			}
		
		AffineTransform oldtr=g.getTransform();
		g.setTransform(shuttle.transform);
		
		Stroke oldStroke= g.getStroke();
		Stroke newStroke= new BasicStroke(
				shuttle.strokeWidth,
				shuttle.linecap.stroke(),
				shuttle.linejoin.stroke()
				);
		g.setStroke(newStroke);
		
		if(shuttle.fill!=null)
			{	
			g.setPaint(shuttle.fill);
			g.fill(shuttle.shape);
			}
		if(shuttle.stroke!=null)
			{
			g.setPaint(shuttle.stroke);
			g.draw(shuttle.shape);
			}
		g.setClip(oldclip);
		g.setStroke(oldStroke);
		g.setTransform(oldtr);
		g.setComposite(oldcomposite);
		}
	
	
	

}
