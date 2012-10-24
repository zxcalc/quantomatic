/**
 * 
 */
package org.lindenb.util;

/**
 * @author pierre
 *
 */
public class Compilation {
private Compilation()
	{
	
	}
static public String getName() { return "?";}
static public String getDate() { return "__DATE__";}
static public String getUser() { return "__USER__";}
static public String getPath() { return "__PWD__";}
static public String getLabel() { return "Compiled by "+getUser()+" on "+getDate()+" in "+getPath();}
}
