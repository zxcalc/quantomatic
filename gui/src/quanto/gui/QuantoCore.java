package quanto.gui;

import java.io.*;
import java.util.Collection;
import java.util.Set;


import edu.uci.ics.jung.contrib.HasName;
import edu.uci.ics.jung.contrib.HasQuotedName;
import java.util.concurrent.ConcurrentLinkedQueue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Regulate communications with the back-end. Primarily accessed via wrappers
 * to the "command" method, which throw QuantoCore.ConsoleError.
 * 
 * In this version, the core contains no GUI code.
 * @author aleks kissinger
 *
 */
public class QuantoCore {

	private final static Logger logger =
		LoggerFactory.getLogger(QuantoCore.class);

	private Process backEnd;
	private BufferedReader from_backEnd;
	private BufferedWriter to_backEnd;
	private ConcurrentLinkedQueue<ConsoleOutput> consoles = new ConcurrentLinkedQueue<ConsoleOutput>();
	private Completer completer;
	private String prompt;

	public static String quantoCoreExecutable = "quanto-core";

	public static class CoreException extends Exception {
		private static final long serialVersionUID = 1053659906558198953L;
		public CoreException() {
			super();
		}
		public CoreException(String msg) {
			super(msg);
		}
		public CoreException(Throwable cause) {
			super(cause);
		}
		public CoreException(String msg, Throwable cause) {
			super(msg, cause);
		}
	}
	public static class CoreFailedToStartException extends CoreException {
		private static final long serialVersionUID = 1053659906558198953L;
		public CoreFailedToStartException() {
			super("The core process could not be executed");
		}
		public CoreFailedToStartException(String msg) {
			super(msg);
		}
		public CoreFailedToStartException(Throwable cause) {
			super("The core process could not be executed", cause);
		}
		public CoreFailedToStartException(String msg, Throwable cause) {
			super(msg, cause);
		}
	}
	public static class CoreCommunicationException extends CoreException {
		private static final long serialVersionUID = 1053659906558198953L;
		public CoreCommunicationException() {
			super("Failed to communicate with the core process");
		}
		public CoreCommunicationException(String msg) {
			super(msg);
		}
		public CoreCommunicationException(Throwable cause) {
			super("Failed to communicate with the core process", cause);
		}
		public CoreCommunicationException(String msg, Throwable cause) {
			super(msg, cause);
		}
	}
	/**
	 * Could not communicate with the backend, because it has terminated
	 */
	public static class CoreTerminatedException extends CoreCommunicationException {
		private static final long serialVersionUID = -234829037423847923L;
		public CoreTerminatedException() {
			super("The core process terminated unexpectedly");
		}

		public CoreTerminatedException(String msg) {
			super(msg);
		}

		public CoreTerminatedException(Throwable cause) {
			super("The core process terminated unexpectedly", cause);
		}

		public CoreTerminatedException(String msg, Throwable cause) {
			super(msg, cause);
		}
	}
	public static class CoreReturnedErrorException extends CoreException {
		private static final long serialVersionUID = 1053659906558198953L;
		public CoreReturnedErrorException() {
			super("The core process returned an error message");
		}
		public CoreReturnedErrorException(String msg) {
			super(msg);
		}
		public CoreReturnedErrorException(Throwable cause) {
			super("The core process returned an error message", cause);
		}
		public CoreReturnedErrorException(String msg, Throwable cause) {
			super(msg, cause);
		}
	}

	public static class FatalError extends RuntimeException {
		private static final long serialVersionUID = -3757849807264018024L;
		public FatalError(String msg) {
			super(msg);
		}

		public FatalError(Throwable cause) {
			super(cause);
		}

		public FatalError(String msg, Throwable cause) {
			super(msg, cause);
		}
	}

	public QuantoCore()
	throws CoreException {

		try {
			ProcessBuilder pb = new ProcessBuilder(quantoCoreExecutable);
			
			pb.redirectErrorStream(true);
			logger.info("Starting {}...", quantoCoreExecutable);
			backEnd = pb.start();
			logger.info("{} started successfully", quantoCoreExecutable);
			
			logger.info("Connecting pipes...");
			from_backEnd = new BufferedReader(new InputStreamReader(backEnd
					.getInputStream()));
			to_backEnd = new BufferedWriter(new OutputStreamWriter(backEnd
					.getOutputStream()));
			logger.info("Pipes connected successfully");
			
			logger.info("Synchonising console...");
			// sync the console
			send("garbage_2039483945;");
			send("HELO;");
			while (!receive().contains("HELO"));
			logger.info("Console synchronised successfully");
			
			
			// Construct the completion engine from the output of the help command.
			completer = new Completer();
			logger.info("Retrieving commands...");
			
			receive(); // eat the prompt
			send("help;");
			BufferedReader reader = new BufferedReader(new StringReader(receive()));
			// eat a couple of lines of description
			reader.readLine(); reader.readLine();
			for (String ln = reader.readLine(); ln != null; ln = reader.readLine())
				if (! ln.equals("")) completer.addWord(ln);
			
			logger.info("Commands retrieved successfully");

			prompt = receive();
		} catch (IOException e) {
			if (backEnd == null) {
				logger.error("Could not execute {} (check PATH)", quantoCoreExecutable);
				logger.error("Error was", e);
				throw new CoreFailedToStartException(e);
			} else { 
				backEnd.destroy();
				backEnd = null;
				logger.error("Failed to set up communication with core process", e);
				throw new CoreCommunicationException("Failed to set up communication with core process", e);
			}
		}
	}

	/**
	 * Attach a console output
	 *
	 * The console output will receive copies of all commands (and their
	 * responses, including prompts) - ie: everything sent or received
	 * in command() or blockCommand() (including indirect uses of these
	 * methods, like the utility methods of this class).
	 *
	 * @p console will @b not receive copies of communication where
	 * send() and receive() (or receiveOrFail()) are used directly.
	 *
	 * @p console will also not receive any communication prior to being
	 * attached, including the prompt.
	 *
	 * @param console The output to attach
	 */
	public void attachConsole(ConsoleOutput console) {
		consoles.add(console);
	}

	/**
	 * Detach an attached console output
	 *
	 * @param console The output to detach
	 */
	public void detachConsole(ConsoleOutput console) {
		consoles.remove(console);
	}

	private void consolesPrintln(Object message) {
		for (ConsoleOutput output : this.consoles) {
			output.println(message);
		}
	}

	private void consolesPrint(Object message) {
		for (ConsoleOutput output : this.consoles) {
			output.print(message);
		}
	}

	private void consolesError(Object message) {
		for (ConsoleOutput output : this.consoles) {
			output.error(message);
		}
	}

	/**
	 * Sends a command to the core process
	 *
	 * @param command The command to send, with no newline
	 * @throws quanto.gui.QuantoCore.CoreCommunicationException
	 * @throws IllegalStateException The core has already been closed
	 */
	public void send(String command)
	throws CoreCommunicationException {
		if (backEnd == null) {
			logger.error("Tried to receive after core had been closed");
			throw new IllegalStateException();
		}
		try {
			to_backEnd.write(command);
			to_backEnd.newLine();
			to_backEnd.flush();
		} catch (IOException e) {
			logger.error("Failed to write to core process", e);
			try {
				logger.error("Tried to write to core process, but it has terminated (exit value: {})", backEnd.exitValue());
				consolesError("Core process terminated!");
				// Not much we can do: throw an exception
				throw new CoreTerminatedException();
			} catch (IllegalThreadStateException ex) {
				logger.error("Failed to write to core process, even though it has not terminated");
				consolesError("Failed to write to core process!");
				throw new CoreCommunicationException();
			}
		}
	}

	/**
	 * Retrieves a response from the core process
	 *
	 * If no command has been sent since the last receive, this may
	 * block indefinitely.
	 *
	 * @return The response
	 * @throws quanto.gui.QuantoCore.CoreCommunicationException
	 * @throws IllegalStateException The core has already been closed
	 */
	public String receive()
	throws CoreCommunicationException {
		synchronized (this) {
			if (backEnd == null) {
				logger.error("Tried to receive after core had been closed");
				throw new IllegalStateException();
			}
			StringBuilder message = new StringBuilder();
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
				logger.error("Failed to read from core process", e);
				if (message.length() > 0) {
					logger.error("Received partial message before read failure: {}", message);
				}

				try {
					logger.error("Core process terminated with exit value {}", backEnd.exitValue());
					consolesError("Core process terminated!");
					throw new CoreTerminatedException(e);
				} catch (IllegalThreadStateException ex) {
					consolesError("Failed to write to core process!");
					throw new CoreCommunicationException(e);
				}
			} catch (java.lang.NullPointerException e) {
				logger.error("Failed to read from core process", e);
				if (message.length() > 0) {
					logger.error("Received partial message before read failure: {}", message);
				}

				try {
					logger.error("Core process terminated with exit value {}", backEnd.exitValue());
					consolesError("Core process terminated!");
					throw new CoreTerminatedException(e);
				} catch (IllegalThreadStateException ex) {
					consolesError("Failed to write to core process!");
					throw new CoreCommunicationException(e);
				}
			}
			
			return message.toString();
		}
	}

	/**
	 * Retrieves a response from the core process
	 *
	 * If the core process returns an error, a CoreReturnedErrorException
	 * will be thrown.
	 *
	 * If no command has been sent since the last receive, this may
	 * block indefinitely.
	 *
	 * @return The response
	 * @throws quanto.gui.QuantoCore.CoreCommunicationException
	 * @throws quanto.gui.QuantoCore.CoreReturnedErrorException
	 * @throws IllegalStateException The core has already been closed
	 */
	public String receiveOrFail()
	throws CoreCommunicationException, CoreReturnedErrorException {
		String rcv = receive();

		if (rcv.startsWith("!!!")) {
			throw new CoreReturnedErrorException(rcv.substring(4));
		}
		return rcv;
	}

	private static class ProcessCleanupThread extends Thread {
		private Process process;

		public ProcessCleanupThread(Process process) {
			super("Process cleanup thread");
			this.process = process;
		}

		@Override
		public void run() {
			try {
				sleep(5000);
			} catch (InterruptedException ex) {}
			process.destroy();
		}
	}

	/**
	 * Quits the core process, and releases associated resources
	 */
	public void destroy(){
		if (backEnd != null) {
			logger.info("Shutting down the core process");
			try {
				send("quit");
				new ProcessCleanupThread(backEnd).start();
			} catch (CoreCommunicationException ex) {
				logger.warn("Failed to send the quit command to the core");
			}
			backEnd = null;
			to_backEnd = null;
			from_backEnd = null;
		}
	}
	
	public Completer getCompleter() {
		return completer;
	}

	public String getPrompt() {
		return prompt;
	}
	

	/**
	 * Send a command with the given arguments. All of the args should be objects
	 * which implement HasName and have non-null names
	 */
	public String command(String name, HasName ... args)
	throws CoreException {
		return blockCommand(name, null, args);
	}
	public String blockCommand(String name, String block, HasName ... args)
	throws CoreException {
		synchronized (this) {
			logger.debug("Sending {} command to backend", name);
			StringBuffer cmd = new StringBuffer(name);
			for (HasName arg : args) {
				if (arg.getName() == null) {
					throw new IllegalArgumentException(
							"Attempted to pass unnamed object to core.");
				}
				HasName qarg = (arg instanceof HasQuotedName) ? arg : new HasQuotedName.QuotedName(arg);
				cmd.append(' ');
				cmd.append(qarg.getName());
				logger.debug("Passing argument {}", qarg.getName());
			}
			cmd.append(';');

			consolesPrintln(cmd);
			send(cmd.toString());
			
			// if we are given a block string, send it to the core as block input
			if (block != null) {
				logger.debug("Sending block input {}", block);
				int r = (int) (Math.random() * Integer.MAX_VALUE);
				send("---startblock:" + Integer.toString(r));
				send(block);
				send("---endblock:" + Integer.toString(r));
			}

			String ret;
			try {
				ret = receiveOrFail();
				consolesPrint(ret);
			} finally {
				String pr = receive(); // eat the prompt
				consolesPrint(pr);
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
	
	
	public String graph_xml(QuantoGraph graph) throws CoreException {
		return command("graph_xml", graph);
	}
	
	public QuantoGraph new_graph() throws CoreException {
		return new QuantoGraph(chomp(command("new_graph")));
	}
	
	public void rename_graph(QuantoGraph graph, String newName) throws CoreException {
		graph.setName(chomp(command("rename_graph", graph, new HasName.StringName(newName))));
	}
	
	public void add_vertex(QuantoGraph graph, QVertex.Type type)
	throws CoreException {
		command("add_vertex", graph, 
				new HasName.StringName(type.toString().toLowerCase()));
	}
	
	public void add_edge(QuantoGraph graph, QVertex s, QVertex t)
	throws CoreException {
		command("add_edge", graph, s, t);
	}
	
	public void attach_rewrites(QuantoGraph graph, Collection<QVertex> vs)
	throws CoreException {
		command("attach_rewrites", graph, new HasQuotedName.QuotedCollectionName(vs));
	}
	
	public void attach_one_rewrite(QuantoGraph graph, Collection<QVertex> vs)
	throws CoreException {
		command("attach_one_rewrite", graph, new HasQuotedName.QuotedCollectionName(vs));
	}
	
	public String show_rewrites(QuantoGraph graph) throws CoreException {
		return command("show_rewrites", graph);
	}
	
	public void apply_rewrite(QuantoGraph graph, int i) throws CoreException {
		command("apply_rewrite", graph, new HasName.IntName(i));
	}
	
	public void set_angle(QuantoGraph graph, QVertex v, String angle) throws CoreException {
		command("set_angle", graph, v, new HasName.StringName(angle));
	}
	
	public String hilb(QuantoGraph graph, String format) throws CoreException {
		return command("hilb", graph, new HasName.StringName(format));
	}
	
	public String hilb(String graphName, String format) throws CoreException {
		return command("hilb",
					new HasName.StringName(graphName),
					new HasName.StringName(format));
	}
	
	public void delete_vertices(QuantoGraph graph, Set<QVertex> v) throws CoreException {
		command("delete_vertices", graph, new HasQuotedName.QuotedCollectionName(v));
	}
	
	public void delete_edges(QuantoGraph graph, Set<QEdge> e) throws CoreException {
		command("delete_edges", graph, new HasQuotedName.QuotedCollectionName(e));
	}
	
	public void undo(QuantoGraph graph) throws CoreException {
		command("undo", graph);
	}
	
	public void redo(QuantoGraph graph) throws CoreException {
		command("redo", graph);
	}
	
	public void save_graph(QuantoGraph graph, String fileName) throws CoreException{
		command("save_graph", graph, new HasName.StringName(fileName));
	}
	
	public QuantoGraph load_graph(String fileName) throws CoreException{
		return new QuantoGraph(chomp(command("load_graph", new HasName.StringName(fileName))));
	}

	public void kill_graph(QuantoGraph graph) throws CoreException{
		command("kill_graph", graph);
	}

	public void kill_graph(String graph) throws CoreException{
		command("kill_graph", new HasName.StringName(graph));
	}
	
	public String input_graph_xml(String xml) throws CoreException {
		return chomp(blockCommand("input_graph_xml", xml));
	}

	public Theory new_ruleset() throws CoreException{
		return new Theory(this, chomp(command("new_ruleset")));
	}
	
	public Theory load_ruleset(String rsetName, String fileName) throws CoreException{
		return new Theory(
				this,
				chomp(command("load_ruleset",
					new HasName.StringName(rsetName), 
					new HasName.StringName(fileName))),
				chomp(fileName),
				false);
	}

	public void rename_ruleset(Theory ruleset, String name) throws CoreException{
		ruleset.setName(
			chomp(
				command("rename_ruleset",
					ruleset,
					new HasName.StringName(name)
				)
			)
		);
	}

	public void save_ruleset(Theory ruleset, String fileName) throws CoreException{
		command("save_ruleset", ruleset, new HasName.StringName(fileName));
	}
	
	public void unload_ruleset(Theory ruleset) throws CoreException{
		command("unload_ruleset", ruleset);
	}

	public String add_bang(QuantoGraph g) throws CoreException {
		return chomp(command("add_bang", g));
	}
	
	public void bang_vertices (QuantoGraph g, BangBox bb, Set<QVertex> verts)
	throws CoreException {
		command("bang_vertices", g, bb, new HasQuotedName.QuotedCollectionName(verts));
	}
	
	
	public void unbang_vertices (QuantoGraph g, Set<QVertex> verts)
	throws CoreException {
		command("unbang_vertices", g, new HasQuotedName.QuotedCollectionName(verts));
	}
	
	public void bbox_merge(QuantoGraph g, Set<BangBox> boxes)
	throws CoreException {
		command("bbox_merge", g, new HasQuotedName.QuotedCollectionName(boxes));
		
	}
	
	public void bbox_drop(QuantoGraph g, Set<BangBox> boxes)
	throws CoreException {
		command("bbox_drop", g, new HasQuotedName.QuotedCollectionName(boxes));
		
	}

	public void bbox_kill(QuantoGraph g, Set<BangBox> boxes)
	throws CoreException {
			command("bbox_kill", g, new HasQuotedName.QuotedCollectionName(boxes));
	}
	
	public void bbox_duplicate(QuantoGraph g, Set<BangBox> boxes)
	throws CoreException {
			command("bbox_duplicate", g, new HasQuotedName.QuotedCollectionName(boxes));
	}
	
	// here we use a string for target, because we may not be keeping the clip-board
	//  as a QuantoGraph in memory.
	public void copy_subgraph (QuantoGraph source, String target, Set<QVertex> verts)
	throws CoreException {
		command("copy_subgraph", source,
				new HasName.StringName(target),
				new HasQuotedName.QuotedCollectionName(verts));
	}
	
	// here we use a string for source, because we may not be keeping the clip-board
	//  as a QuantoGraph in memory.
	public void insert_graph (QuantoGraph target, String source)
	throws CoreException {
		command("insert_graph", target, new HasName.StringName(source));
	}
	
	public void flip_vertices (QuantoGraph g, Set<QVertex> vs)
	throws CoreException {
		command("flip_vertices", g, new HasQuotedName.QuotedCollectionName(vs));
	}
	
	public String[] list_rulesets() throws CoreException {
		return command("list_rulesets").split("\r\n|\n|\r");
	}
	
	public String[] list_active_rulesets() throws CoreException {
		return command("list_active_rulesets").split("\r\n|\n|\r");
	}
	
	public String[] list_rules(String rset) throws CoreException {
		return command("list_rules", new HasName.StringName(rset)).split("\r\n|\n|\r");
	}
	
	public void activate_ruleset(Theory rset) throws CoreException {
		command("activate_ruleset", rset);
		rset.setActive(true);
	}
	
	public void deactivate_ruleset(Theory rset) throws CoreException {
		command("deactivate_ruleset", rset);
		rset.setActive(false);
	}
	
	public void apply_first_rewrite(String graph) throws CoreException {
		command("apply_first_rewrite", new HasName.StringName(graph));
	}
	
	public void apply_first_rewrite(QuantoGraph graph) throws CoreException {
		command("apply_first_rewrite", graph);
	}
	
	public QuantoGraph open_rule_lhs(Theory rset, String rule)
	throws CoreException {
		try {
			QuantoGraph g = new QuantoGraph(
					chomp(command("open_rule_lhs", rset, new HasName.StringName(rule))));
			g.fromXml(graph_xml(g));
			return g;
		} catch (QuantoGraph.ParseException e) {
			throw new CoreException("The core sent an invalid graph description: " + e.getMessage());
		}
	}
	
	public QuantoGraph open_rule_rhs(Theory rset, String rule)
	throws CoreException {
		try {
			QuantoGraph g = new QuantoGraph(
					chomp(command("open_rule_rhs", rset, new HasName.StringName(rule))));
			g.fromXml(graph_xml(g));
			return g;
		} catch (QuantoGraph.ParseException e) {
			throw new CoreException("The core sent an invalid graph description: " + e.getMessage());
		}
	}
	
	public void replace_rule(Theory rset, Rewrite rule)
	throws CoreException {
		command("replace_rule", rset, rule, rule.getLhs(), rule.getRhs());
	}

	public String new_rule(Theory rset, QuantoGraph graph)
	throws CoreException {
		return chomp(command("new_rule", rset, graph));
	}

	public void delete_rule(Theory rset, Rewrite rule)
	throws CoreException {
		command("delete_rule", rset, rule);
	}

	public void delete_rule(Theory rset, String rule)
	throws CoreException {
		command("delete_rule", rset, new HasName.StringName(rule));
	}

	public void rename_rule(Theory rset, String rule, String newName)
	throws CoreException {
		command("rename_rule", rset, new HasName.StringName(rule),
			new HasName.StringName(newName));
	}
	
	/*
	 * Derived methods, note these are in CamelCase to emphasise that they
	 * are not actual core commands.
	 */
	public void fastNormalise(String graph) throws CoreException {
		try {
			while (true) apply_first_rewrite(graph);
		} catch (CoreException e) {
			if (! chomp(e.getMessage()).equals("No more rewrites.")) throw e;
		}
	}
	public void fastNormalise(QuantoGraph graph) throws CoreException {
		fastNormalise(graph.getName());
	}
	

}
