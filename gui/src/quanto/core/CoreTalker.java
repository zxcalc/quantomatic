package quanto.core;

import java.io.*;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Regulate communications with the back-end. Primarily accessed via wrappers to
 * the "command" method, which throw QuantoCore.ConsoleError.
 * 
 * In this version, the core contains no GUI code.
 * 
 * @author aleks kissinger
 * 
 */
public abstract class CoreTalker {

	private final static Logger logger = Logger.getLogger("quanto.core");

	public static String quantoCoreExecutable = "quanto-core";

	// set to true to dump all communication to stdout
	protected final static boolean DEBUG = false;

	@Retention(RetentionPolicy.RUNTIME)
	@Target(ElementType.METHOD)
	public @interface Command {
		String helpText() default "";
	}

	private Process backEnd;

	protected CoreTalker(boolean redirectErrorStream) throws CoreException {
		try {
			ProcessBuilder pb = new ProcessBuilder(quantoCoreExecutable);

			pb.redirectErrorStream(redirectErrorStream);
			logger.log(Level.FINEST, "Starting {0}...", quantoCoreExecutable);
			backEnd = pb.start();
			logger.log(Level.FINEST, "{0} started successfully", quantoCoreExecutable);
		} catch (IOException e) {
			logger.log(Level.SEVERE,
					   "Could not execute \"" + quantoCoreExecutable + "\": " +
					   e.getMessage(),
					   e);
			throw new CoreExecutionException(String.format(
					"Could not execute \"%1$\": %2$", quantoCoreExecutable,
					e.getMessage()), e);
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

	protected void writeFailure(Throwable e) throws CoreCommunicationException {
		try {
			logger.log(Level.SEVERE,
					"Tried to write to core process, but it has terminated (exit value: {0})",
					backEnd.exitValue());
			// Not much we can do: throw an exception
			throw new CoreTerminatedException();
		} catch (IllegalThreadStateException ex) {
			logger.log(Level.SEVERE,
					   "Failed to write to core process, even though it has not terminated");
			throw new CoreCommunicationException();
		}
	}

	protected void readFailure(Throwable e) throws CoreCommunicationException {
		try {
			logger.log(Level.SEVERE, "Core process terminated with exit value {0}",
					backEnd.exitValue());
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
			} catch (InterruptedException ex) {
			}
			process.destroy();
		}
	}

	public static class Arg {
		public enum Type {
			Integer, Name, Path, VertexType, RawString
		}

		private int i;
		private String s;
		private Type type;

		private Arg(Type type) {
			this.type = type;
		}

		public static Arg intArg(int i) {
			Arg a = new Arg(Type.Integer);
			a.i = i;
			return a;
		}

		public static Arg nameArg(String n) {
			Arg a = new Arg(Type.Name);
			a.s = n;
			return a;
		}

		public static Arg pathArg(File n) {
			Arg a = new Arg(Type.Path);
			a.s = n.getAbsolutePath();
			return a;
		}

		public static Arg vertexTypeArg(String n) {
			Arg a = new Arg(Type.VertexType);
			a.s = n;
			return a;
		}

		public static Arg rawArg(String n) {
			Arg a = new Arg(Type.RawString);
			a.s = n;
			return a;
		}

		public Type getType() {
			return type;
		}

		public String getStringValue() {
			if (type == Type.Integer)
				return Integer.toString(i);
			return s;
		}

		public int getIntValue() {
			if (type != Type.Integer)
				throw new UnsupportedOperationException();
			return i;
		}
	}

	/**
	 * Ask the core nicely to quit. No communication should be requested after
	 * this.
	 * 
	 * @throws CoreCommunicationException
	 */
	protected abstract void quit() throws CoreCommunicationException;

	/**
	 * Send a command with the given arguments. All of the args should be
	 * objects which implement HasName and have non-null names
	 */
	public abstract void command(String name, Arg... args) throws CoreException;

	public abstract String commandAsName(String name, Arg... args)
			throws CoreException;

	public abstract String commandAsRaw(String name, Arg... args)
			throws CoreException;

	public abstract String[] commandAsList(String name, Arg... args)
			throws CoreException;

	/**
	 * Quits the core process, and releases associated resources
	 */
	public void destroy() {
		if (backEnd != null) {
			logger.log(Level.FINEST, "Shutting down the core process");
			try {
				quit();
				new ProcessCleanupThread(backEnd).start();
			} catch (CoreCommunicationException ex) {
				logger.log(Level.WARNING, "Failed to send the quit command to the core");
			}
			backEnd = null;
		}
	}

	private Arg[] unshiftNames(String first, String[] rest) {
		Arg[] args = new Arg[rest.length + 1];
		args[0] = Arg.nameArg(first);
		for (int i = 0; i < rest.length; ++i) {
			args[i + 1] = Arg.nameArg(rest[i]);
		}
		return args;
	}

	private Arg[] doubleUnshiftNames(String first, String second, String[] rest) {
		Arg[] args = new Arg[rest.length + 2];
		args[0] = Arg.nameArg(first);
		args[1] = Arg.nameArg(second);
		for (int i = 0; i < rest.length; ++i) {
			args[i + 2] = Arg.nameArg(rest[i]);
		}
		return args;
	}

	/*
	 * Below here are all the functions implemented by the quanto core
	 */
	
	@Command
	public String console_command(String command) throws CoreException {
		return commandAsRaw("console_command", Arg.rawArg(command));
	}

	@Command
	public void load_ruleset(File location) throws CoreException {
		command("load_ruleset", Arg.pathArg(location));
	}

	@Command
	public void save_ruleset(File location) throws CoreException {
		command("save_ruleset", Arg.pathArg(location));
	}

	@Command
	public void new_rule(String ruleName, String lhsName, String rhsName)
			throws CoreException {
		command("new_rule", Arg.nameArg(ruleName), Arg.nameArg(lhsName),
				Arg.nameArg(rhsName));
	}

	@Command
	public String open_rule_lhs(String rule) throws CoreException {
		return commandAsName("open_rule_lhs", Arg.nameArg(rule));
	}

	@Command
	public String open_rule_rhs(String rule) throws CoreException {
		return commandAsName("open_rule_rhs", Arg.nameArg(rule));
	}

	@Command
	public void update_rule(String rule, String lhsName, String rhsName)
			throws CoreException {
		command("update_rule", Arg.nameArg(rule), Arg.nameArg(lhsName),
				Arg.nameArg(rhsName));
	}

	@Command
	public String[] list_graphs() throws CoreException {
		return commandAsList("list_graphs");
	}

	@Command
	public String[] list_rules() throws CoreException {
		return commandAsList("list_rules");
	}

	@Command
	public String[] list_tags() throws CoreException {
		return commandAsList("list_tags");
	}

	@Command
	public String[] list_rules_with_tag(String tag) throws CoreException {
		return commandAsList("list_rules_with_tag", Arg.nameArg(tag));
	}

	@Command
	public String[] list_active_rules() throws CoreException {
		return commandAsList("list_active_rules");
	}

	@Command
	public void activate_rules_with_tag(String tag) throws CoreException {
		command("activate_rules_with_tag", Arg.nameArg(tag));
	}

	@Command
	public void deactivate_rules_with_tag(String tag) throws CoreException {
		command("deactivate_rules_with_tag", Arg.nameArg(tag));
	}

	@Command
	public void delete_rules_with_tag(String tag) throws CoreException {
		command("delete_rules_with_tag", Arg.nameArg(tag));
	}

	@Command
	public void delete_tag(String tag) throws CoreException {
		command("delete_tag", Arg.nameArg(tag));
	}

	@Command
	public void activate_rule(String rulename) throws CoreException {
		command("activate_rule", Arg.nameArg(rulename));
	}

	@Command
	public void deactivate_rule(String rulename) throws CoreException {
		command("deactivate_rule", Arg.nameArg(rulename));
	}

	@Command
	public void delete_rule(String rulename) throws CoreException {
		command("delete_rule", Arg.nameArg(rulename));
	}

	@Command
	public void tag_rule(String rulename, String tag) throws CoreException {
		command("tag_rule", Arg.nameArg(rulename), Arg.nameArg(tag));
	}

	@Command
	public void untag_rule(String rulename, String tag) throws CoreException {
		command("untag_rule", Arg.nameArg(rulename), Arg.nameArg(tag));
	}

	@Command
	public String new_graph() throws CoreException {
		return commandAsName("new_graph");
	}

	@Command
	public String load_graph(File location) throws CoreException {
		return commandAsName("load_graph", Arg.pathArg(location));
	}

	@Command
	public void save_graph(String graphName, File location)
			throws CoreException {
		command("save_graph", Arg.nameArg(graphName), Arg.pathArg(location));
	}

	@Command
	public String duplicate_graph(String graphName) throws CoreException {
		return commandAsName("duplicate_graph", Arg.nameArg(graphName));
	}

	@Command
	public String rename_graph(String graphName, String newName)
			throws CoreException {
		return commandAsName("rename_graph", Arg.nameArg(graphName),
				Arg.nameArg(newName));
	}

	@Command
	public void kill_graph(String graphName) throws CoreException {
		command("kill_graph", Arg.nameArg(graphName));
	}

	@Command
	public String graph_xml(String graphName) throws CoreException {
		return commandAsRaw("graph_xml", Arg.nameArg(graphName));
	}

	@Command
	public String hilb(String graphName, String format) throws CoreException {
		return commandAsRaw("hilb", Arg.nameArg(graphName), Arg.rawArg(format));
	}

	@Command
	public void undo(String graphName) throws CoreException {
		command("undo", Arg.nameArg(graphName));
	}

	@Command
	public void redo(String graphName) throws CoreException {
		command("redo", Arg.nameArg(graphName));
	}

	@Command
	public String add_vertex(String graphName, String type)
			throws CoreException {
		return commandAsName("add_vertex", Arg.nameArg(graphName),
				Arg.vertexTypeArg(type));
	}

	@Command
	public String rename_vertex(String graphName, String vertexName,
			String newName) throws CoreException {
		return commandAsName("rename_vertex", Arg.nameArg(graphName),
				Arg.nameArg(vertexName), Arg.nameArg(newName));
	}

	@Command
	public void set_angle(String graphName, String vertexName, String angle)
			throws CoreException {
		command("set_angle", Arg.nameArg(graphName), Arg.nameArg(vertexName),
				Arg.nameArg(angle));
	}

	@Command
	public void flip_vertices(String graphName, String... verts)
			throws CoreException {
		command("flip_vertices", unshiftNames(graphName, verts));
	}

	@Command
	public void delete_vertices(String graphName, String... vertexNames)
			throws CoreException {
		command("delete_vertices", unshiftNames(graphName, vertexNames));
	}

	@Command
	public String add_edge(String graphName, String sourceName,
			String targetName) throws CoreException {
		return commandAsName("add_edge", Arg.nameArg(graphName),
				Arg.nameArg(sourceName), Arg.nameArg(targetName));
	}

	@Command
	public void delete_edges(String graphName, String... edgeNames)
			throws CoreException {
		command("delete_edges", unshiftNames(graphName, edgeNames));
	}

	@Command
	public String add_bang(String graphName) throws CoreException {
		return commandAsName("add_bang", Arg.nameArg(graphName));
	}

	@Command
	public void bbox_drop(String graphName, String... boxes)
			throws CoreException {
		command("bbox_drop", unshiftNames(graphName, boxes));

	}

	@Command
	public String bbox_merge(String graphName, String... boxes)
			throws CoreException {
		return commandAsName("bbox_merge", unshiftNames(graphName, boxes));

	}

	@Command
	public String bbox_duplicate(String graphName, String box)
			throws CoreException {
		return commandAsName("bbox_duplicate", Arg.nameArg(graphName),
				Arg.nameArg(box));
	}

	@Command
	public void bbox_kill(String graphName, String... boxes)
			throws CoreException {
		command("bbox_kill", unshiftNames(graphName, boxes));
	}

	@Command
	public void bang_vertices(String graphName, String bb, String... verts)
			throws CoreException {
		command("bang_vertices", doubleUnshiftNames(graphName, bb, verts));
	}

	@Command
	public void unbang_vertices(String graphName, String... verts)
			throws CoreException {
		command("unbang_vertices", unshiftNames(graphName, verts));
	}

	@Command
	public void copy_subgraph(String source, String target, String... verts)
			throws CoreException {
		command("copy_subgraph", doubleUnshiftNames(source, target, verts));
	}

	@Command
	public void insert_graph(String source, String target) throws CoreException {
		command("insert_graph", Arg.nameArg(source), Arg.nameArg(target));
	}

	@Command
	public void attach_rewrites(String graphName, String... vertexNames)
			throws CoreException {
		command("attach_rewrites", unshiftNames(graphName, vertexNames));
	}

	@Command
	public void attach_one_rewrite(String graphName, String... vertexNames)
			throws CoreException {
		command("attach_one_rewrite", unshiftNames(graphName, vertexNames));
	}

	@Command
	public String show_rewrites(String graphName) throws CoreException {
		return commandAsRaw("show_rewrites", Arg.nameArg(graphName));
	}

	@Command
	public void apply_rewrite(String graphName, int i) throws CoreException {
		command("apply_rewrite", Arg.nameArg(graphName), Arg.intArg(i));
	}

	@Command
	public void apply_first_rewrite(String graphName) throws CoreException {
		command("apply_first_rewrite", Arg.nameArg(graphName));
	}
}
