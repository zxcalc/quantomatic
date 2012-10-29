package org.lindenb.lang;

import org.lindenb.xml.XMLUtilities;
import org.w3c.dom.Node;

/**
 * Exception throwed when the schema DOM is invalid
 * @author pierre
 *
 */
public class InvalidXMLException extends Exception
	{
	private static final long serialVersionUID = 1L;

	public InvalidXMLException()
		{
		}
	public InvalidXMLException(Node node)
		{
		this(node,"Illegal node");
		}
	
	public InvalidXMLException(Node node,String msg)
		{
		super(
			(node==null?"Error":XMLUtilities.node2path(node))
			+(msg==null?"":" : "+msg)
			);
		}
	
	public InvalidXMLException(String msg)
		{
		super(msg);
		}

	public InvalidXMLException(Throwable err)
		{
		super( err);
		}

	public InvalidXMLException(String msg, Throwable  err) {
		super(msg, err);
		}	
	
	}
