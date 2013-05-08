package org.lindenb.xml;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


import org.lindenb.util.StringUtils;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/** Static methods for XML */
public class XMLUtilities
	{
	public static final String VERSION= "1.0";
	/** xml header encoding UTF-8 */
	public static final String DECLARATION_UTF8= "<?xml version=\""+VERSION +"\" encoding=\"UTF-8\"?>";
	private XMLUtilities() {}
	
	
/** escape the XML of a given string */
public static String escape(CharSequence s)
        {
        if(s==null) throw new NullPointerException("XML.escape(null)");
        int needed=-1;
        for(int i=0;i< s.length();++i)
	        {
	        switch(s.charAt(i))
	            {
				case '\'': 
				case '\"': 
				case '&': 
				case '<':
				case '>':  needed=i; break;
	
	            default: break;
	            }
	        if(needed!=-1) break;
	        }
		if(needed==-1) return s.toString();
		StringBuilder buffer=new StringBuilder(s.subSequence(0,needed));
		for(int i=needed;i< s.length();++i)
		       {
		       switch(s.charAt(i))
		               {
		               case '\'': buffer.append("&apos;"); break;
		               case '\"': buffer.append("&quot;"); break;
		               case '&': buffer.append("&amp;"); break;
		               case '<': buffer.append("&lt;"); break;
		               case '>': buffer.append("&gt;"); break;
		               default:  buffer.append(s.charAt(i));break;
		               }
		       }
		return buffer.toString();
        }

/** write a CharSequence to a java.io.Writer */
public static Writer escape(Writer out,CharSequence s) throws IOException
	{
	 for(int i=0;i< s.length();++i)
	     {
		 switch(s.charAt(i))
	         {
	         case '\'': out.write("&apos;"); break;
	         case '\"': out.write("&quot;"); break;
	         case '&': out.write("&amp;"); break;
	         case '<': out.write("&lt;"); break;
	         case '>': out.write("&gt;"); break;
	         default:  out.write(s.charAt(i));break;
	         }
	     }
	return out;
	}

/** Lousy function removing the tags in a string, it does NOT unescapes the entities */
public static String removeTags(CharSequence seq)
	{
	boolean inXML=false;
	StringBuilder b= new StringBuilder(seq.length());
	for(int i=0;i< seq.length();++i)
		{
		if(inXML)
			{
			if(seq.charAt(i)=='>')
				{
				inXML=false;
				}	
			}
		else
			{
			if(seq.charAt(i)=='<')
				{
				inXML=true;
				}
			else
				{
				b.append(seq.charAt(i));
				}
			}
		}
	return b.toString();
	}


public static String unescape(CharSequence seq)
	{
	return removeTags(seq)
		.replaceAll("&gt;", ">")
		.replaceAll("&lt;", "<")
		.replaceAll("&apos;", "\'")
		.replaceAll("&quot;", "\"")
		.replaceAll("&amp;", "&");
	}

/**
 * return wether the given node matches ns and localName
 * @param node
 * @param ns namespace use null as a wildcard for all namespaceuri
 * @param localName use null as a wildcard for all localNames
 * @return is node matching
 */
public static boolean isA(Node node,String ns,String localName)
	{
	if(node.getNodeType()!=Node.ELEMENT_NODE) return false;
	if(ns!=null && !ns.equals(node.getNamespaceURI()))return false;
	if(localName!=null && !localName.equals(node.getLocalName())) return false;
	return true;
	}


/**
 * count number of element under root matching ns and localName
 * @param root
 * @param ns namespace use null as a wildcard for all namespaceuri
 * @param localName use null as a wildcard for all localNames
 * @return number Found
 */
public static int count(Node root,String ns,String localName)
	{
	int n=0;
	for(Node c=root.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(!isA(c,ns,localName)) continue;
		++n;
		}
	return n;
	}

/**
 * count number of element under root
 * @param root
 * @return number of element Found
 */
public static int count(Node root)
	{
	if(root==null) throw new NullPointerException("count(null)");
	int n=0;
	for(Node c=root.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(c.getNodeType()!=Element.ELEMENT_NODE) continue;
		++n;
		}
	return n;
	}

/**
 * count any kind of node under root
 * @param root
 * @return number of element Found
 */
public static int countAllNodes(Node root)
	{
	int n=0;
	for(Node c=root.getFirstChild();c!=null;c=c.getNextSibling())
		{
		++n;
		}
	return n;
	}

/**
 * return the first Element under root matching ns and localName
 * @param root
 * @param ns namespace use null as a wildcard for all namespaceuri
 * @param localName use null as a wildcard for all localNames
 * @return element Found or null
 */
public static Element firstChild(Node root,String ns,String localName)
	{
	for(Node c=root.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(!isA(c,ns,localName)) continue;
		return Element.class.cast(c);
		}
	return null;
	}

/**
 * return the first Element under root matching the given tagName
 * @param root
 * @param taglName element name
 * @return element Found or null
 */
public static Element firstChild(Node root,String tagName)
	{
	for(Node c=root.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(!(c.getNodeType()==Node.ELEMENT_NODE && c.getNodeName().equals(tagName))) continue;
		return Element.class.cast(c);
		}
	return null;
	}



/**
 * return the first Element under root 
 */
public static Element firstChild(Node root)
	{
	for(Node c=root.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(c.getNodeType()!=Node.ELEMENT_NODE) continue;
		return Element.class.cast(c);
		}
	return null;
	}


/**
 * return one or no Element under root 
 */
public static Element oneOrZero(Node root)
	{
	Element found=null;
	for(Node c=root.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(c.getNodeType()!=Node.ELEMENT_NODE) continue;
		if(found!=null) throw new IllegalArgumentException(
				"found two elements under "+node2path(root));
		found= Element.class.cast(c);
		}
	return found;
	}

/**
 * return one or no Element under root 
 */
public static Element oneOrZero(Node root,String tagName)
	{
	Element found=null;
	for(Node c=root.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(c.getNodeType()!=Node.ELEMENT_NODE) continue;
		if(!tagName.equals(c.getNodeName())) continue;
		if(found!=null) throw new IllegalArgumentException(
				"found two elements under "+node2path(root));
		found= Element.class.cast(c);
		}
	return found;
	}

/**
 * return one or no Element under root
 * @param ns namespace can be used as a wildcard if null
 * @param localName can be used as a wildcard if null
 */
public static Element oneOrZero(Node root,String ns,String local)
	{
	Element found=null;
	for(Node c=root.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(c.getNodeType()!=Node.ELEMENT_NODE) continue;
		if(ns!=null && !ns.equals(c.getNamespaceURI())) continue;
		if(local!=null && !local.equals(c.getLocalName())) continue;
		if(found!=null) throw new IllegalArgumentException(
				"found two elements under "+node2path(root));
		found= Element.class.cast(c);
		}
	return found;
	}

/**
 * return one and only one Element under root 
 */
public static Element one(Node root)
	{
	Element found=null;
	for(Node c=root.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(c.getNodeType()!=Node.ELEMENT_NODE) continue;
		if(found!=null) throw new IllegalArgumentException(
				"found two elements under "+node2path(root));
		found= Element.class.cast(c);
		}
	if(found==null) throw new IllegalArgumentException(
			"found no element under "+node2path(root));
	return found;
	}

/**
 * return one and only one Element under root 
 */
public static Element one(Node root,String tagName)
	{
	Element found=null;
	for(Node c=root.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(c.getNodeType()!=Node.ELEMENT_NODE) continue;
		if(!tagName.equals(c.getNodeName())) continue;
		if(found!=null) throw new IllegalArgumentException(
				"found two elements under "+node2path(root));
		found= Element.class.cast(c);
		}
	if(found==null) throw new IllegalArgumentException(
			"found no element under "+node2path(root));
	return found;
	}

/**
 * return one and only one Element under root
 * @param ns namespace can be used as a wildcard if null
 * @param localName can be used as a wildcard if null
 */
public static Element one(Node root,String ns,String local)
	{
	Element found=null;
	for(Node c=root.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(c.getNodeType()!=Node.ELEMENT_NODE) continue;
		if(ns!=null && !ns.equals(c.getNamespaceURI())) continue;
		if(local!=null && !local.equals(c.getLocalName())) continue;
		if(found!=null) throw new IllegalArgumentException(
				"found two elements under "+node2path(root));
		found= Element.class.cast(c);
		}
	if(found==null) throw new IllegalArgumentException(
			"found no element under "+node2path(root));
	return found;
	}



/** return Collection over child elements */
public static List<Element> elements(Node parent)
	{
	List<Element> v= new ArrayList<Element>();
	for(Node c=parent.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(c.getNodeType()==Node.ELEMENT_NODE)
			{
			v.add( Element.class.cast(c));
			}
		}
	return v;
	}

/**
 * return a collection of all Element under parent matching ns and localName
 * @param parent
 * @param ns namespace use null as a wildcard for all namespaceuri
 * @param localName use null as a wildcard for all localNames
 * @return element Found or null
 */
public static List<Element> elements(Node parent,String namespace,String localName)
	{
	List<Element> v= new ArrayList<Element>();
	for(Node c=parent.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(isA(c,namespace,localName))
			{
			v.add( Element.class.cast(c));
			}
		}
	return v;
	}

/**
 * return a collection of all Element under parent matching the given tag name
 * @param parent
 * @param tagName the element tagName
 * @return element Found or null
 */
public static List<Element> elements(Node parent,String tagName)
	{
	List<Element> v= new ArrayList<Element>();
	for(Node c=parent.getFirstChild();c!=null;c=c.getNextSibling())
		{
		if(c.getNodeType()==Node.ELEMENT_NODE &&
			tagName.equals(c.getNodeName()))
			{
			v.add( Element.class.cast(c));
			}
		}
	return v;
	}


/**
 * return a XPATH-like description of the node
 */
public static String node2path(Node n)
	{
	StringBuilder b=new StringBuilder();
	while(n!=null)
		{
		switch(n.getNodeType())
			{
			case Node.ATTRIBUTE_NODE:
				{
				b.insert(0,"/@"+n.getNodeName());
				n= Attr.class.cast(n).getOwnerElement();
				continue;
				}
			case Node.TEXT_NODE:
				{
				b.insert(0,"text()");
				break;
				}
			case Node.COMMENT_NODE:
				{
				b.insert(0,"comment()");
				break;
				}
			case Node.ELEMENT_NODE:
				{
				int L=0;
				Node curr= n;
				while(curr!=null)
					{
					if(curr.getNodeType()==Node.ELEMENT_NODE &&
					   curr.getNodeName().equals(n.getNodeName()))
						{
						L++;
						}
					curr=curr.getPreviousSibling();
					}
				
				b.insert(0,"/"+n.getNodeName()+"["+(L)+"]");
				break;
				}
			}
		n=n.getParentNode();
		}
	return b.toString();
	}

/** return the level of a node */
public static int getLevel(Node n)
	{
	if(n==null) return -1;
	int L=0;
	while(n.getParentNode()!=null)
		{
		++L;
		n= n.getParentNode();
		}
	return L;
	}

/** remove all children under n
 * @returns the number of node removed */
public static int removeChildren(Node n)
	{
	int L=0;
	while(n.hasChildNodes())
		{
		++L;
		n.removeChild( n.getFirstChild());
		}
	return L;
	}


/** returns wether is document is Data Oriented
 * @throws IllegalArgumentException if this document is not DataOriented*/
public static void validateAsDataOrientedDocument(Node node)
	throws IllegalArgumentException
	{
	if(node==null) throw new NullPointerException("node is null");
	if(node.getNodeType()==Node.DOCUMENT_NODE)
		{
		Element root= Document.class.cast(node).getDocumentElement();
		if(root==null) return;
		validateAsDataOrientedDocument(root);
		}
	else if(node.getNodeType()==Node.ELEMENT_NODE)
		{
		boolean containsTag=false;
		boolean blank=true;
		for(Node n1=node.getFirstChild();n1!=null;n1=n1.getNextSibling())
			{
			switch(n1.getNodeType())
				{
				case Node.ELEMENT_NODE:
					{
					containsTag=true;
					validateAsDataOrientedDocument(Element.class.cast(n1));
					break;
					}
				case Node.TEXT_NODE:
				case Node.CDATA_SECTION_NODE:
					{
					if(blank)
						{
						blank=StringUtils.isBlank(n1.getTextContent());
						}
					break;
					}
				}
			}
		if(!blank && containsTag)
			{
			throw new IllegalArgumentException("not a Data Oriented Document: see "+node2path(node));
			}
		}
	
	}

private static abstract class AbstractIter
implements Iterator<Element>
	{
	protected boolean _first=true;
	protected boolean _hasNextCalled=false;
	protected Element _next=null;
	protected boolean _eofMet=false;
	protected abstract boolean accept(Element e);
	protected abstract Node firstChild();
	
	public boolean hasNext()
		{
		if(_hasNextCalled)
			{
			return _next!=null;
			}
		_hasNextCalled=true;
		if(_eofMet)
			{
			return false;
			}
		Node c;
		if(_first)
			{
			c= firstChild();
			_first=false;
			}
		else
			{
			c=_next.getNextSibling();
			}
		while(c!=null)
			{
			if(c.getNodeType()==Node.ELEMENT_NODE &&
			   accept(Element.class.cast(c)))
				{
				break;
				}
			c=c.getNextSibling();
			}
		if(c==null)
			{
			_eofMet=true;
			_next=null;
			return false;
			}
		else
			{
			_next=Element.class.cast(c);
			return true;
			}
		}

	public Element next()
		{
		if(!_hasNextCalled) hasNext();
		if(_next==null) throw new IllegalStateException();
		_hasNextCalled=false;
		return _next;
		}

	public void remove()
		{
		throw new UnsupportedOperationException();
		}
	}

private static class ForEach1
	implements Iterable<Element>
	{
	private Node root;
	private String namespaceuri;
	private String localName;
	private String tagName;

	class Iter extends AbstractIter
		{
		@Override
		protected Node firstChild()
			{
			return ForEach1.this.root.getFirstChild();
			}
		
		@Override
		protected boolean accept(Element c)
			{
			if(ForEach1.this.namespaceuri!=null || ForEach1.this.localName!=null)
				{
				if(ForEach1.this.namespaceuri!=null && !ForEach1.this.namespaceuri.equals(c.getNamespaceURI())) return false;
				if(ForEach1.this.localName!=null && !ForEach1.this.localName.equals(c.getLocalName())) return false;
				return true;
				}
			else if(ForEach1.this.tagName!=null)
				{
				return 	ForEach1.this.tagName.equals(c.getNodeName());
				}
			else
				{
				return true;
				}
			
			}
		}
	
	ForEach1(Node root,String namespaceuri,String localName,String tagName)
		{
		this.root=root;
		this.namespaceuri=namespaceuri;
		this.localName=localName;
		this.tagName=tagName;
		}
	
	public Iterator<Element> iterator()
		{
		return new Iter();
		}
	}


/**
 * 
 * @param parent
 * @param ns namespace can be used as a wildcard if null
 * @param localName can be used as a wildcard if null
 * @return
 */
public static Iterable<Element> forEach(Node parent,String namespaceuri,String localName)
	{
	return new ForEach1(parent,namespaceuri,localName,null);
	}
public static Iterable<Element> forEach(Node parent,String name)
	{
	return new ForEach1(parent,null,null,name);
	}
public static Iterable<Element> forEach(Node parent)
	{
	return new ForEach1(parent,null,null,null);
	}
}	
