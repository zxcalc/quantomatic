package quanto.gui;

import java.io.*;

/**
 * Regulate communications with the back-end. Primarily accessed via wrappers
 * to the "command" method, which throw QuantoCore.ConsoleError.
 * 
 * In this version, the core contains no GUI code.
 * @author aleks kissinger
 *
 */
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
	
	public static class ConsoleError extends Exception {
		private static final long serialVersionUID = 1053659906558198953L;
		public ConsoleError(String msg) {
			super(msg);
		}
	}

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
			
			// sync the console
			send("HELO;");
			while (!receive().contains("HELO"));
			
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
			// end of text is marked by " "+BACKSPACE (ASCII 8)
			
			int c = from_backEnd.read();
			while (c != 8) {
				message.append((char)c);
				c = from_backEnd.read();
			}
			
			// delete the trailing space
			message.deleteCharAt(message.length()-1);
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
	
	public String receiveOrFail() throws ConsoleError {
		String rcv = receive();
		
		if (rcv.startsWith("!!!")) {
			throw new ConsoleError(rcv.substring(4));
		}
		return rcv;
	}
	
	public void closeQuantoBackEnd(){
		output.println("Shutting down quantoML");
		send("quit");
	}
	
	
	/*
	 * Some helpers for the methods below
	 */
	
	/**
	 * Send a command with the given arguments. All of the args should be of types
	 * with a well-behaved toString() method.
	 */
	protected String command(String name, HasName ... args) throws ConsoleError {
		StringBuffer cmd = new StringBuffer(name);
		for (HasName arg : args) {
			if (arg.getName() == null)
				throw new ConsoleError(
						"Attempted to pass unnamed object to core.");
			cmd.append(' ');
			cmd.append(arg.getName());
		}
		cmd.append(';');
		
		String ret;
		synchronized (this) {
			send(cmd.toString());
			ret = receiveOrFail();
			receive(); // eat the prompt
		}
		
		return ret;
	}
	
	/**
	 * Remove all line breaks.
	 */
	protected String chomp(String str) {
		return str.replace("\n", "");
	}
	
	/*
	 * Below here are all the functions implemented by the quanto core
	 */
	
	
	public String graph_xml(QuantoGraph graph) throws ConsoleError {
		return command("graph_xml", graph);
	}
	
	public QuantoGraph new_graph() throws ConsoleError {
		return new QuantoGraph(chomp(command("new_graph")));
	}
	
	public void add_vertex(QuantoGraph graph, QVertex.Type type)
	throws ConsoleError {
		command("add_vertex", graph, 
				new HasName.Basic(type.toString().toLowerCase()));
	}
	
	public void add_edge(QuantoGraph graph, QVertex s, QVertex t)
	throws ConsoleError {
		command("add_edge", graph, s, t);
	}
}
