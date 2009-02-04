package quanto.gui;

import java.io.*;
import java.util.Set;

import edu.uci.ics.jung.contrib.HasName;

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
	
	private Process backEnd;
	private BufferedReader from_backEnd;
	private BufferedWriter to_backEnd;
	private PrintStream output;
	private Completer completer;
	private boolean consoleEcho;
	
	public static class ConsoleError extends Exception {
		private static final long serialVersionUID = 1053659906558198953L;
		public ConsoleError(String msg) {
			super(msg);
		}
	}
	
	public static class FatalError extends RuntimeException {
		private static final long serialVersionUID = -3757849807264018024L;
		public FatalError(String msg) {
			super(msg);
		}
		
		public FatalError(Exception e) {
			super(e);
		}
	}

	public QuantoCore(PrintStream output) {
		this.consoleEcho = false;
		this.output = output;
		try {
			ProcessBuilder pb = new ProcessBuilder("quanto-core");
			pb.redirectErrorStream(true);
			System.out.println("Initialising QuantoML...");
			backEnd = pb.start();
			
			System.out.println("Connecting pipes...");
			from_backEnd = new BufferedReader(new InputStreamReader(backEnd
					.getInputStream()));
			to_backEnd = new BufferedWriter(new OutputStreamWriter(backEnd
					.getOutputStream()));
			
			System.out.println("Synchonising console...");
			// sync the console
			send("garbage_2039483945;");
			send("HELO;");
			while (!receive().contains("HELO"));
			System.out.println("done.");
			
			
			// Construct the completion engine from the output of the help command.
			completer = new Completer();
			System.out.println("Retrieving commands...");
			
			receive(); // eat the prompt
			send("help;");
			BufferedReader reader = new BufferedReader(new StringReader(receive()));
			// eat a couple of lines of description
			reader.readLine(); reader.readLine();
			for (String ln = reader.readLine(); ln != null; ln = reader.readLine())
				if (! ln.equals("")) completer.addWord(ln);
			
			System.out.println("done.");
			
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
		synchronized (this) {
			StringBuffer message = new StringBuffer();
			try {
				// end of text is marked by " "+BACKSPACE (ASCII 8)
				
				int c = from_backEnd.read();
				while (c != 8) {
					if (c == -1) throw new IOException();
					message.append((char)c);
					c = from_backEnd.read();
				}
				
				// delete the trailing space
				message.deleteCharAt(message.length()-1);
			} catch (IOException e) {
				System.out.println("Back-end last said: " + message.toString());
				System.out.println("Exit value from backend: " + backEnd.exitValue());
				throw new QuantoCore.FatalError(e);
			}
			catch (java.lang.NullPointerException e) {
				output.println("Exit value from backend: " + backEnd.exitValue());
				e.printStackTrace();
				return null;
			}
			
			return message.toString();
		}
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
	
	public Completer getCompleter() {
		return completer;
	}
	
	public void setConsoleEcho(boolean consoleEcho) {
		this.consoleEcho = consoleEcho;
	}

	public boolean getConsoleEcho() {
		return consoleEcho;
	}
	
	/*
	 * Some helpers for the methods below
	 */
	
	/**
	 * Send a command with the given arguments. All of the args should be objects
	 * which implement HasName and have non-null names
	 */
	protected String command(String name, HasName ... args) throws ConsoleError {
		synchronized (this) {
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
			//System.out.println(cmd);
			synchronized (this) {
				if (consoleEcho) output.println(cmd);
				send(cmd.toString());
				try {
					ret = receiveOrFail();
					if (consoleEcho) {
						if (ret.startsWith("GRAPH_XML"))
							output.println("GRAPH_XML...");
						else output.print(ret);
					}
				} finally {
					String pr = receive(); // eat the prompt
					if (consoleEcho) output.print(pr);
				}
			}
			
			return ret;
		}
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
				new HasName.StringName(type.toString().toLowerCase()));
	}
	
	public void add_edge(QuantoGraph graph, QVertex s, QVertex t)
	throws ConsoleError {
		command("add_edge", graph, s, t);
	}
	
	public void attach_rewrites(QuantoGraph graph, Set<QVertex> vs)
	throws ConsoleError {
		command("attach_rewrites", graph, new HasName.SetName(vs));
	}
	
	public String show_rewrites(QuantoGraph graph) throws ConsoleError {
		return command("show_rewrites", graph);
	}
	
	public void apply_rewrite(QuantoGraph graph, int i) throws ConsoleError {
		command("apply_rewrite", graph, new HasName.IntName(i));
	}
	
	public void set_angle(QuantoGraph graph, QVertex v, String angle) throws ConsoleError {
		command("set_angle", graph, v, new HasName.StringName(angle));
	}
	
	public String hilb(QuantoGraph graph, String format) throws ConsoleError {
		return command("hilb", graph, new HasName.StringName(format));
	}
	
	public void delete_vertices(QuantoGraph graph, Set<QVertex> v) throws ConsoleError {
		command("delete_vertices", graph, new HasName.SetName(v));
	}
	
	public void delete_edges(QuantoGraph graph, Set<QEdge> e) throws ConsoleError {
		command("delete_edges", graph, new HasName.SetName(e));
	}
	
	public void undo(QuantoGraph graph) throws ConsoleError {
		command("undo", graph);
	}
	
	public void redo(QuantoGraph graph) throws ConsoleError {
		command("redo", graph);
	}
	
	public void save_graph(QuantoGraph graph, String fileName) throws ConsoleError{
		command("save_graph", graph, new HasName.StringName(fileName));
	}
	
	public QuantoGraph load_graph(String fileName) throws ConsoleError{
		return new QuantoGraph(chomp(command("load_graph", new HasName.StringName(fileName))));
	}

	public String add_bang(QuantoGraph g) throws ConsoleError {
		return chomp(command("add_bang", g));
	}
	
	public void bang_vertices (QuantoGraph g, BangBox bb, Set<QVertex> verts)
	throws ConsoleError {
		command("bang_vertices", g, bb, new HasName.SetName(verts));
	}
	
	public void unbang_vertices (QuantoGraph g, Set<QVertex> verts)
	throws ConsoleError {
		command("unbang_vertices", g, new HasName.SetName(verts));
	}
	
	// here we use a string for target, because we may not be keeping the clip-board
	//  as a QuantoGraph in memory.
	public void copy_subgraph (QuantoGraph source, String target, Set<QVertex> verts)
	throws ConsoleError {
		command("copy_subgraph", source,
				new HasName.StringName(target),
				new HasName.SetName(verts));
	}
	
	// here we use a string for source, because we may not be keeping the clip-board
	//  as a QuantoGraph in memory.
	public void insert_graph (QuantoGraph target, String source)
	throws ConsoleError {
		command("insert_graph", target, new HasName.StringName(source));
	}
}
