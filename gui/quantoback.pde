/*
  Java wrapper for the ML back end.
  Ross Duncan
 */

import java.io.*;
import java.util.Map;
import java.util.HashMap;
import java.util.StringTokenizer;

public class QuantoBack {

    final static String ml_command = 
	"/usr/local/bin/isabelle -e Controller.init(); quanto";

    Process backEnd;
    BufferedReader from_backEnd;
    BufferedWriter to_backEnd;

    public QuantoBack() {
	try {
	    backEnd = Runtime.getRuntime().exec(ml_command);
	    from_backEnd =  new BufferedReader(
			    new InputStreamReader(
			    backEnd.getInputStream()
			    ));
	    to_backEnd = new BufferedWriter(
			 new OutputStreamWriter(
		         backEnd.getOutputStream()
			 ));	
	    println("Initialising QuantoML...");	   
	    send("s\n");  // ask for back end status
	    println(receive());  //  print it out
	}
	catch (IOException e) {
	    e.printStackTrace();
	}	
    }

    public void send(String command) {
	try {
	    to_backEnd.write(command);
	    to_backEnd.newLine();
	    to_backEnd.flush();
	}
	catch (IOException e)  {
	    e.printStackTrace();
	}
    }

    public String receive() {	
	StringBuffer message = new StringBuffer();
	try {
	    String ln = from_backEnd.readLine();
	    while (!ln.equals("stop")) {
		println(ln);
		message.append(ln);
		ln = from_backEnd.readLine();	    
	    }
	} catch (IOException e) {
	    e.printStackTrace();
	}
	return message.toString();
	
    }
}