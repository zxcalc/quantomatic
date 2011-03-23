package quanto.core;

import java.io.*;
import java.util.Collection;
import java.util.Set;


import edu.uci.ics.jung.contrib.HasName;
import edu.uci.ics.jung.contrib.HasQuotedName;
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
public abstract class Core {

	private final static Logger logger =
		LoggerFactory.getLogger(Core.class);

        // set to true to dump all communication to stdout
        protected final static boolean DEBUG = true;

	private Process backEnd;

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
	/**
	 * Could not communicate with the backend
	 */
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
	 * Could not communicate with the backend, because it failed to start
	 */
	public static class CoreFailedToStartException extends CoreCommunicationException {
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
	/**
	 * The core returned an error message
	 */
	public static class CoreReturnedErrorException extends CoreException {
		private static final long serialVersionUID = 1053659906558198953L;
		public CoreReturnedErrorException(String msg) {
			super(msg);
		}
	}
        public static class CommandException extends CoreReturnedErrorException {
		private static final long serialVersionUID = 1232814923748927383L;
                private String command;
		public CommandException(String command) {
			super("Could not run command \"" + command + "\"");
                        this.command = command;
		}
		public CommandException(String message, String command) {
			super(message);
                        this.command = command;
		}
                public String getCommand() {
                        return command;
                }
        }
        public static class UnknownCommandException extends CommandException {
		private static final long serialVersionUID = 1232814923748927383L;
		public UnknownCommandException(String command) {
			super("Unknown command \"" + command + "\"", command);
		}
		public UnknownCommandException(String message, String command) {
			super(message, command);
		}
        }
        public static class CommandArgumentsException extends CommandException {
		private static final long serialVersionUID = 1232814923748927383L;
		public CommandArgumentsException(String command) {
			super("Bad arguments for command \"" + command + "\"", command);
		}
		public CommandArgumentsException(String message, String command) {
			super(message, command);
		}
        }
	/**
	 * The core returned an unexpected response
	 */
	public static class CoreBadResponseException extends CoreException {
		private static final long serialVersionUID = 1034523458932534589L;
                private String response;
		public CoreBadResponseException(String msg, String response) {
			super(msg);
                        this.response = response;
		}
		public CoreBadResponseException(String response) {
                        this.response = response;
		}
                public String getResponse() {
                        return response;
                }
	}

	protected Core(boolean redirectErrorStream) throws CoreException {
		try {
			ProcessBuilder pb = new ProcessBuilder(quantoCoreExecutable);
			
			pb.redirectErrorStream(redirectErrorStream);
			logger.info("Starting {}...", quantoCoreExecutable);
			backEnd = pb.start();
			logger.info("{} started successfully", quantoCoreExecutable);
		} catch (IOException e) {
                        logger.error("Could not execute {} (check PATH)", quantoCoreExecutable);
                        logger.error("Error was", e);
                        throw new CoreFailedToStartException(String.format("Could not execute \"%1$\"", quantoCoreExecutable), e);
		}
	}

        protected final boolean isClosed() {
		return backEnd == null;
        }

        protected final InputStream getInputStream() {
                return backEnd.getInputStream();
        }

        protected final InputStream getErrorStream() {
                return backEnd.getErrorStream();
        }

        protected final OutputStream getOutputStream() {
                return backEnd.getOutputStream();
        }

        protected final void forceDestroy() {
                backEnd.destroy();
                backEnd = null;
        }

        protected void writeFailure(Throwable e)
                throws CoreCommunicationException
        {
                try {
                        logger.error("Tried to write to core process, but it has terminated (exit value: {})", backEnd.exitValue());
                        // Not much we can do: throw an exception
                        throw new CoreTerminatedException();
                } catch (IllegalThreadStateException ex) {
                        logger.error("Failed to write to core process, even though it has not terminated");
                        throw new CoreCommunicationException();
                }
        }

        protected void readFailure(Throwable e)
                throws CoreCommunicationException
        {
                try {
                        logger.error("Core process terminated with exit value {}", backEnd.exitValue());
                        throw new CoreTerminatedException(e);
                } catch (IllegalThreadStateException ex) {
                        throw new CoreCommunicationException(e);
                }
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
         * Ask the core nicely to quit.  No communication should be requested
         * after this.
         *
         * @throws quanto.core.Core.CoreCommunicationException
         */
        protected abstract void quit() throws CoreCommunicationException;

	/**
	 * Quits the core process, and releases associated resources
	 */
	public void destroy(){
		if (backEnd != null) {
			logger.info("Shutting down the core process");
			try {
				quit();
				new ProcessCleanupThread(backEnd).start();
			} catch (CoreCommunicationException ex) {
				logger.warn("Failed to send the quit command to the core");
			}
			backEnd = null;
		}
	}
	

	/**
	 * Send a command with the given arguments. All of the args should be objects
	 * which implement HasName and have non-null names
	 */
	public abstract String command(String name, HasName ... args)
                throws CoreException;
	public abstract String blockCommand(String name, String block, HasName ... args)
                throws CoreException;

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
	
	public Theory load_ruleset(String fileName) throws CoreException{
		return new Theory(
				this,
				chomp(command("load_ruleset", 
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
