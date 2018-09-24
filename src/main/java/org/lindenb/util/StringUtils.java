package org.lindenb.util;

import java.util.Arrays;
import java.util.Collection;

/**
 * Utilities for Strings or CharSequence
 * @author lindenb
 *
 */
public class StringUtils
	{
	protected StringUtils()
		{
		}
	
	@Override
	protected final Object clone() throws CloneNotSupportedException {
		throw new CloneNotSupportedException();
		}
	
	/** returns the substring of the first argument string that precedes the first occurrence of the second argument string in the first argument string
	 * or the empty string if the first argument string does not contain the second argument string.  */
	public static String substringBefore(String s,String delim)
		{
		int i=s.indexOf(delim);
		return(i==-1?null:s.substring(0,i));
		}
	
	/** returns the substring of the first argument string that precedes the first occurrence of the second argument char in the first argument string
	 * or null if the first argument string does not contain the second argument string.  */
	public static String substringBefore(String s,char delim)
		{
		int i=s.indexOf(delim);
		return(i==-1?null:s.substring(0,i));
		}
	
	
	/** returns the substring of the first argument string that precedes the first occurrence of the second argument char in the first argument string
	 * or the empty string if the first argument string does not contain the second argument string.  */
	public static String substringAfter(String s,String delim)
		{
		int i=s.indexOf(delim);
		return(i==-1?null:s.substring(i+delim.length()));
		}
	
	/** returns the substring of the first argument string that precedes the first occurrence of the second argument char in the first argument string
	 * or the empty string if the first argument string does not contain the second argument string.  */
	public static String substringAfter(String s,char delim)
		{
		int i=s.indexOf(delim);
		return(i==-1?null:s.substring(i+1));
		}
	
	/** returns wether the sequence is empty of null */
	public static boolean isEmpty(CharSequence s)
		{
		return s==null || s.length()==0;
		}
	
	/** return wether the sequence is null, empty of contains only white characters */
	public static boolean isBlank(CharSequence s)
		{
		if(isEmpty(s)) return true;
		for(int i=0;i< s.length();++i)
			{
			if(!Character.isWhitespace(s.charAt(i))) return false;
			}
		return true;
		}
	/** return wether the sequence is null, empty of contains only white characters */
	public static boolean isBlank(char array[],int start,int length)
		{
		if(array==null || length==0) return true;
		for(int i=0;i< length ;++i)
			{
			if(!Character.isWhitespace(array[start+i])) return false;
			}
		return true;
		}
	
	/** remove simple or double quote from a String */
	public static String unquote(String s)
		{
		if(s==null) return null;
		if(s.length()>1 && ((s.startsWith("\'") && s.endsWith("\'")) || (s.startsWith("\"") && s.endsWith("\""))))
			{
			return s.substring(1, s.length()-1);
			}
		return s;
		}
	
	/** anwsers wether the first string is in the choice */
	public static boolean isIn(String search,String...choice)
		{
		for(String s:choice) if(s.equals(search)) return true;
		return false;
		}
	
	/** anwsers wether the first string is in the choice */
	public static boolean isInIgnoreCase(String search,String...choice)
		{
		for(String s:choice) if(s.equalsIgnoreCase(search)) return true;
		return false;
		}
	
	/** anwsers wether the first string starts with any of the other strings */
	public static boolean startsWith(String search,String...starts)
		{
		for(String s:starts) if(search.startsWith(s)) return true;
		return false;
		}
	/** anwsers wether the first string ends with any of the other strings */
	public static boolean endsWith(String search,String...ends)
		{
		for(String s:ends) if(search.endsWith(s)) return true;
		return false;
		}
	
	public static String ljust(String s,int width,char fillchar)
		{
		if(s==null || s.length()>=width) return s;

		StringBuilder b=new StringBuilder(width);
		b.append(s);
		while(b.length()<width)
			{
			b.append(fillchar);
			}
		return b.toString();
		}
	public static String ljust(String s,int width)
		{
		return ljust(s, width,' ');
		}
	
	public static String rjust(String s,int width,char fillchar)
		{
		if(s==null || s.length()>=width) return s;
		StringBuilder b=new StringBuilder(width);
		while(b.length()<(width-s.length()))
			{
			b.append(fillchar);
			}
		b.append(s);
		return b.toString();
		}
	
	public static String rjust(String s,int width)
		{
		return rjust(s, width,' ');
		}
	
	public static String swapcase(String s)
		{
		StringBuilder b=new StringBuilder(s.length());
		for(int i=0;i< s.length();++i)
			{
			char c=s.charAt(i);
			if(Character.isLetter(c))
				{
				if(Character.isUpperCase(c))
					{
					c=Character.toLowerCase(c);
					}
				else
					{
					c=Character.toUpperCase(c);
					}
				}
			b.append(c);
			}
		return b.toString();
		}
	
	public static String join(Collection<?> c,String sep)
		{
		StringBuilder b=new StringBuilder();
		boolean first=true;
		for(Object o:c)
			{
			if(!first) b.append(sep);
			first=false;
			b.append(String.valueOf(o));
			}
		return b.toString();
		}
	public static String join(Collection<?> c)
		{
		return join(c," ");
		}
	public static String join(Object c[],String sep)
		{
		return  join(Arrays.asList(c),sep);
		}
	public static String join(Object c[])
		{
		return  join(Arrays.asList(c));
		}
	}
