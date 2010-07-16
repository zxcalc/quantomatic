package quanto.gui;

import java.io.*;
import java.util.Collection;
import java.util.Set;


import edu.uci.ics.jung.contrib.HasName;
import edu.uci.ics.jung.contrib.HasQuotedName;

/**
 * Regulate communications with the back-end. Primarily accessed via wrappers
 * to the "command" method, which throw QuantoCore.ConsoleError.
 * 
 * In this version, the core contains no GUI code.
 * @author aleks kissinger
 *
 */
public class QuantoCore {
	
	private Process backEnd;
	private BufferedReader from_backEnd;
	private BufferedWriter to_backEnd;
	private PrintStream output;
	private Completer completer;

	// useful for calling as a library
	public final HasName[] noargs = new HasName[]{};

	// Invoked as OS X application
	public static String appName = null;

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

	public QuantoCore(PrintStream setoutput) throws IOException {

		output = System.out;
		output.println("Intialising quanto-core...");

		try {
			ProcessBuilder pb;
			

			if (QuantoCore.appName != null)
			{
				output.println("Setting up app process...");
				pb = new ProcessBuilder(QuantoCore.appName + "/Contents/MacOS/quanto-core-app");
				// add extra macos packaged program path
				//Map<String,String> env = pb.environment();
				//String path = env.get("PATH");
				//env.put("PATH",path + File.pathSeparator + appName + "/Contents/MacOS/");
			} else {
				pb = new ProcessBuilder("quanto-core");
			}
			
			pb.redirectErrorStream(true);
			output.println("Starting process....");
			output.flush(); // make sure this is output, in case it hangs below
			backEnd = pb.start();
			output.println("done.");
			
			output.println("Connecting pipes...");
			output.flush();
			from_backEnd = new BufferedReader(new InputStreamReader(backEnd
					.getInputStream()));
			to_backEnd = new BufferedWriter(new OutputStreamWriter(backEnd
					.getOutputStream()));
			output.println("done.");
			
			output.println("Synchonising console...");
			output.flush();
			// sync the console
			send("garbage_2039483945;");
			send("HELO;");
			while (!receive().contains("HELO"));
			output.println("done.");
			
			
			// Construct the completion engine from the output of the help command.
			completer = new Completer();
			output.print("Retrieving commands...");
			output.flush();
			
			receive(); // eat the prompt
			send("help;");
			BufferedReader reader = new BufferedReader(new StringReader(receive()));
			// eat a couple of lines of description
			reader.readLine(); reader.readLine();
			for (String ln = reader.readLine(); ln != null; ln = reader.readLine())
				if (! ln.equals("")) completer.addWord(ln);
			
			output.println("done.");
			// set future output to go to specific given output stream, rather than system one.
			output = setoutput;
			
		} catch (IOException e) {
			//e.printStackTrace();
			if(backEnd == null) { 
				System.err.println("ERROR: Cannot execute: quanto-core, check it is in the path.");
			} else { 
				backEnd.destroy(); 
				backEnd = null; 
				System.err.println("Exit value from backend: " + backEnd.exitValue()); 
			}
			throw e;
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
			} catch (java.lang.NullPointerException e) {
				if(backEnd == null) {
					output.println("Backend is null.");
				} else {
					output.println("Exit value from backend: " + backEnd.exitValue());
				}
				//e.printStackTrace();
				throw new QuantoCore.FatalError(e);
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
	
	/*
	 * Some helpers for the methods below
	 */
	
	
	/**
	 * Send a command with the given arguments. All of the args should be objects
	 * which implement HasName and have non-null names
	 */
	public String command(String name, HasName ... args) throws ConsoleError {
		return blockCommand(name, null, args);
	}
	public String blockCommand(String name, String block, HasName ... args) throws ConsoleError {
		synchronized (this) {
			StringBuffer cmd = new StringBuffer(name);
			for (HasName arg : args) {
				if (arg.getName() == null)
					throw new ConsoleError(
							"Attempted to pass unnamed object to core.");
				HasName qarg = (arg instanceof HasQuotedName) ? arg : new HasQuotedName.QuotedName(arg);
				cmd.append(' ');
				cmd.append(qarg.getName());
			}
			cmd.append(';');
			
			// only read preferences if there is already a QuantoApp running. Otherwise,
			// we're probably being invoked as a library.
			boolean consoleEcho = (QuantoApp.hasInstance() &&
					QuantoApp.getInstance().getPreference(QuantoApp.CONSOLE_ECHO));
			
			if (consoleEcho) output.println(cmd);
			send(cmd.toString());
			
			// if we are given a block string, send it to the core as block input
			if (block != null) {
				int r = (int) (Math.random() * Integer.MAX_VALUE);
				send("---startblock:" + Integer.toString(r));
				send(block);
				send("---endblock:" + Integer.toString(r));
			}
			
			String ret;
			//System.out.println(cmd);
			
			try {
				ret = receiveOrFail();
				if (consoleEcho) {
					output.print(ret);
				}
			} finally {
				String pr = receive(); // eat the prompt
				if (consoleEcho) output.print(pr);
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
	
	public void rename_graph(QuantoGraph graph, String newName) throws ConsoleError {
		graph.setName(chomp(command("rename_graph", graph, new HasName.StringName(newName))));
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
	
	public void attach_rewrites(QuantoGraph graph, Collection<QVertex> vs)
	throws ConsoleError {
		command("attach_rewrites", graph, new HasQuotedName.QuotedCollectionName(vs));
	}
	
	public void attach_one_rewrite(QuantoGraph graph, Collection<QVertex> vs)
	throws ConsoleError {
		command("attach_one_rewrite", graph, new HasQuotedName.QuotedCollectionName(vs));
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
	
	public String hilb(String graphName, String format) throws ConsoleError {
		return command("hilb",
					new HasName.StringName(graphName),
					new HasName.StringName(format));
	}
	
	public void delete_vertices(QuantoGraph graph, Set<QVertex> v) throws ConsoleError {
		command("delete_vertices", graph, new HasQuotedName.QuotedCollectionName(v));
	}
	
	public void delete_edges(QuantoGraph graph, Set<QEdge> e) throws ConsoleError {
		command("delete_edges", graph, new HasQuotedName.QuotedCollectionName(e));
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
	
	public String input_graph_xml(String xml) throws ConsoleError {
		return chomp(blockCommand("input_graph_xml", xml));
	}
	
	public Ruleset load_ruleset(String rsetName, String fileName) throws ConsoleError{
		return new Ruleset(
				chomp(command("load_ruleset",
					new HasName.StringName(rsetName), 
					new HasName.StringName(fileName))),
				chomp(fileName),
				false);
	}
	
	public void unload_ruleset(Ruleset ruleset) throws ConsoleError{
		command("unload_ruleset", ruleset);
	}

	public String add_bang(QuantoGraph g) throws ConsoleError {
		return chomp(command("add_bang", g));
	}
	
	public void bang_vertices (QuantoGraph g, BangBox bb, Set<QVertex> verts)
	throws ConsoleError {
		command("bang_vertices", g, bb, new HasQuotedName.QuotedCollectionName(verts));
	}
	
	
	public void unbang_vertices (QuantoGraph g, Set<QVertex> verts)
	throws ConsoleError {
		command("unbang_vertices", g, new HasQuotedName.QuotedCollectionName(verts));
	}
	
	public void bbox_merge(QuantoGraph g, Set<BangBox> boxes)
	throws ConsoleError {
		command("bbox_merge", g, new HasQuotedName.QuotedCollectionName(boxes));
		
	}
	
	public void bbox_drop(QuantoGraph g, Set<BangBox> boxes)
	throws ConsoleError {
		command("bbox_drop", g, new HasQuotedName.QuotedCollectionName(boxes));
		
	}

	public void bbox_kill(QuantoGraph g, Set<BangBox> boxes)
	throws ConsoleError {
			command("bbox_kill", g, new HasQuotedName.QuotedCollectionName(boxes));
	}
	
	public void bbox_duplicate(QuantoGraph g, Set<BangBox> boxes)
	throws ConsoleError {
			command("bbox_duplicate", g, new HasQuotedName.QuotedCollectionName(boxes));
	}
	
	// here we use a string for target, because we may not be keeping the clip-board
	//  as a QuantoGraph in memory.
	public void copy_subgraph (QuantoGraph source, String target, Set<QVertex> verts)
	throws ConsoleError {
		command("copy_subgraph", source,
				new HasName.StringName(target),
				new HasQuotedName.QuotedCollectionName(verts));
	}
	
	// here we use a string for source, because we may not be keeping the clip-board
	//  as a QuantoGraph in memory.
	public void insert_graph (QuantoGraph target, String source)
	throws ConsoleError {
		command("insert_graph", target, new HasName.StringName(source));
	}
	
	public void flip_vertices (QuantoGraph g, Set<QVertex> vs)
	throws ConsoleError {
		command("flip_vertices", g, new HasQuotedName.QuotedCollectionName(vs));
	}
	
	public String[] list_rulesets() throws ConsoleError {
		return command("list_rulesets").split("\r\n|\n|\r");
	}
	
	public String[] list_active_rulesets() throws ConsoleError {
		return command("list_active_rulesets").split("\r\n|\n|\r");
	}
	
	public String[] list_rules(String rset) throws ConsoleError {
		return command("list_rules", new HasName.StringName(rset)).split("\r\n|\n|\r");
	}
	
	public void activate_ruleset(Ruleset rset) throws ConsoleError {
		command("activate_ruleset", rset);
		rset.setActive(true);
	}
	
	public void deactivate_ruleset(Ruleset rset) throws ConsoleError {
		command("deactivate_ruleset", rset);
		rset.setActive(false);
	}
	
	public void apply_first_rewrite(String graph) throws ConsoleError {
		command("apply_first_rewrite", new HasName.StringName(graph));
	}
	
	public void apply_first_rewrite(QuantoGraph graph) throws ConsoleError {
		command("apply_first_rewrite", graph);
	}
	
	public QuantoGraph open_rule_lhs(Ruleset rset, String rule)
	throws ConsoleError {
		try {
			QuantoGraph g = new QuantoGraph(
					chomp(command("open_rule_lhs", rset, new HasName.StringName(rule))));
			g.fromXml(graph_xml(g));
			return g;
		} catch (QuantoGraph.ParseException e) {
			throw new ConsoleError("The core sent an invalid graph description: " + e.getMessage());
		}
	}
	
	public QuantoGraph open_rule_rhs(Ruleset rset, String rule)
	throws ConsoleError {
		try {
			QuantoGraph g = new QuantoGraph(
					chomp(command("open_rule_rhs", rset, new HasName.StringName(rule))));
			g.fromXml(graph_xml(g));
			return g;
		} catch (QuantoGraph.ParseException e) {
			throw new ConsoleError("The core sent an invalid graph description: " + e.getMessage());
		}
	}
	
	public void replace_rule(Ruleset rset, Rewrite rule)
	throws ConsoleError {
		command("replace_rule", rset, rule, rule.getLhs(), rule.getRhs());
	}
	
	/*
	 * Derived methods, note these are in CamelCase to emphasise that they
	 * are not actual core commands.
	 */
	public void fastNormalise(String graph) throws ConsoleError {
		try {
			while (true) apply_first_rewrite(graph);
		} catch (ConsoleError e) {
			if (! chomp(e.getMessage()).equals("No more rewrites.")) throw e;
		}
	}
	public void fastNormalise(QuantoGraph graph) throws ConsoleError {
		fastNormalise(graph.getName());
	}
	

}
