package quanto.gui;

import java.io.*;

public class QuantoCore {
 
	public static final int VERTEX_RED = 1;
	public static final int VERTEX_GREEN = 2;
	public static final int VERTEX_HADAMARD = 3;
	public static final int VERTEX_BOUNDARY = 4;
	
	Process backEnd;
	BufferedReader from_backEnd;
	BufferedReader from_backEndError;
	BufferedWriter to_backEnd;
	PrintStream output;

	public QuantoCore(PrintStream output) {
		this.output = output;
		try {
			ProcessBuilder pb = new ProcessBuilder("quanto-core");	
			output.println("Initialising QuantoML...");
			backEnd = pb.start();
			
			System.out.println("Connecting pipes...");
			from_backEnd = new BufferedReader(new InputStreamReader(backEnd
					.getInputStream()));
			from_backEndError = new BufferedReader(new InputStreamReader(backEnd
					.getErrorStream()));
			to_backEnd = new BufferedWriter(new OutputStreamWriter(backEnd
					.getOutputStream()));
			
			output.println("Sending hello...");
			send("H\n"); // ask for back end status
			
			output.println("Getting hello...");
			// Make sure we eat up any output before the status.
			String rcv = receive();
			while (rcv != null && !rcv.contains("Hello from QUANTOMATIC")) {
				System.out.println("QuantoCore Sent: " + rcv);
				rcv = receive();
			}
			
			output.println("Status:");
			output.println(rcv); //  print it out
		} catch (IOException e) {
			e.printStackTrace();
			if(backEnd == null) { output.println("ERROR: Cannot execute: quanto-core, check it is in the path."); }
			else {output.println("Exit value from backend: " + backEnd.exitValue()); }
		}
	}

	public void send(String command) {
		if(to_backEnd != null){
			try {
				to_backEnd.write(command);
				to_backEnd.newLine();
				to_backEnd.flush();
			} catch (IOException e) {
				output.println("Exit value from backend: "
						+ backEnd.exitValue());
				e.printStackTrace();
			}
		}
	}

	public String receive() {
		StringBuffer message = new StringBuffer();
		try {
			String ln = from_backEnd.readLine();
			while (!ln.equals("stop")) {
				message.append(ln);
				message.append('\n');
				ln = from_backEnd.readLine();
			} 
		} catch (IOException e) {
			output.println("Exit value from backend: " + backEnd.exitValue());
			e.printStackTrace();
		}
		catch (java.lang.NullPointerException e) {
			output.println("Exit value from backend: " + backEnd.exitValue());
			e.printStackTrace();
			return null;
		}
		return message.toString();
	}
	
	public void closeQuantoBackEnd(){
		output.println("Shutting down quantoML");
		send("Q\n");
	}
	
	/*void updateGraph() {
		send("D\n");
		//output.println("RECEIVED:");
		String r = receive();
		//r = "<graph><vertex><name>a</name><boundary>false</boundary><colour>red</colour></vertex></graph>";
		//XMLElement x = new XMLElement(r);
		//output.println(r);
		Graph updated = xml.parseGraph(r);
		updated.coordinateSystem = null;
		graph.updateTo(updated);
		graph.layoutGraph();
	}
	
	void modifyCmd(String s){	
		send(s + "\n");
		updateGraph();
	}
	
	public RewriteInstance getRewritesForSelection() {
		
		String s = " ";
		for (Vertex v : graph.getVertices().values()) {
			if(v.selected) {
				s = s + v.id + " ";
			}
		}
		send("RWshow" + s + "\n");
		return xml.parseRewrite(receive());
	}
	
	public RewriteInstance nextRewriteForSelection() {
		send("RWnext\n");
		return xml.parseRewrite(receive());
	}
	
	public RewriteInstance prevRewriteForSelection() {
		send("RWprev\n");
		return xml.parseRewrite(receive());
	}
	
	public void abortRewrite() {
		send("RWabort\n");
	}

	public void acceptRewriteForSelection() {
		modifyCmd("RWyes");
	}
	
	public void newGraph() {
		modifyCmd("n");
	}

	public void previousGraphState() {
		modifyCmd("u");
	}

	public void deleteAllSelected() {
		for (Vertex v : graph.getVertices().values()) {
			if (v.selected){ send("d " + v.id + "\n"); }
		}
		updateGraph();
	}

	public void addEdge(Vertex w, Vertex v) {
		modifyCmd("e " + w.id + " " + v.id);
	}

	public void addVertex(int v) {
		switch(v){
			case VERTEX_RED: modifyCmd("r"); break;
			case VERTEX_GREEN: modifyCmd("g"); break;
			case VERTEX_HADAMARD: modifyCmd("h"); break;
			case VERTEX_BOUNDARY: modifyCmd("b"); break;
		}
	}*/
}
