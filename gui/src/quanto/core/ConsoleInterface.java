/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.core;

import edu.uci.ics.jung.contrib.HasName;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.util.LinkedList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Simulates a console interface to the core backend
 *
 * @author alemer
 */
public class ConsoleInterface {
	private final static Logger logger =
		LoggerFactory.getLogger(ConsoleInterface.class);

        public interface ResponseListener {
                public void responseReceived(String response);
        }

        public static class ParseException extends Exception {
                public ParseException() {
                }
                public ParseException(String message) {
                        super(message);
                }
        }

        private Core core;
        private ResponseListener responseListener;
	private Completer completer;

        public ConsoleInterface(Core core)
        {
                this.core = core;

                // Construct the completion engine from the output of the help command.
                completer = new Completer();
                logger.info("Retrieving commands...");

                try
                {
                        String result = core.command("help");
                        BufferedReader reader = new BufferedReader(new StringReader(result));
                        // eat a couple of lines of description
                        reader.readLine(); reader.readLine();
                        for (String ln = reader.readLine(); ln != null; ln = reader.readLine())
                                if (! ln.equals("")) completer.addWord(ln);
                        logger.info("Commands retrieved successfully");
                }
                catch (IOException ex) {
                        logger.error("Failed to retreive commands for completion", ex);
                }
                catch (Core.CoreException ex) {
                        logger.error("Failed to retreive commands for completion", ex);
                }

        }

        public void setResponseListener(ResponseListener responseListener) {
                this.responseListener = responseListener;
        }

        public ResponseListener getResponseListener() {
                return responseListener;
        }

	public Completer getCompleter() {
		return completer;
	}

        /**
         * Execute the command asynchronously, depending on the response
         * listener to deal with the reply.
         *
         * Note: currently, this is a fake - it just calls
         * inputCommandSync.
         *
         * @param input
         * @throws quanto.gui.QuantoCore.CoreException
         */
        public void inputCommandAsync(String input)
                throws Core.CoreException, ParseException
        {
                inputCommandSync(input, true);
        }

        private static class Command {
                public Command(String command) {
                        this.command = command;
                }
                public String command;
                public LinkedList<HasName> args = new LinkedList<HasName>();
                public HasName[] getArgsArray()
                {
                        return args.toArray(new HasName[args.size()]);
                }
        }

        public String inputCommandSync(String input, boolean notify)
                throws Core.CoreException, ParseException
        {
                try {
                        Reader r = new StringReader(input);
                        StreamTokenizer t = new StreamTokenizer(r);
                        t.slashSlashComments(false);
                        t.slashStarComments(false);
                        t.lowerCaseMode(false);
                        t.eolIsSignificant(false);
                        t.parseNumbers();
                        t.quoteChar('"');
                        t.quoteChar('\'');
                        t.wordChars('a', 'z');
                        t.wordChars('A', 'Z');
                        t.wordChars('0', '9');
                        t.wordChars('_', '_');

                        int type = StreamTokenizer.TT_EOF;
                        Command current = null;
                        LinkedList<Command> commands = new LinkedList<Command>();
                        while ((type = t.nextToken()) != StreamTokenizer.TT_EOF)
                        {
                                if (current == null)
                                {
                                        if (type == StreamTokenizer.TT_NUMBER)
                                                throw new ParseException("Expected command, got " + t.nval);
                                        if (type != StreamTokenizer.TT_WORD)
                                                throw new ParseException("Expected command, got " + type);
                                        current = new Command(t.sval);
                                }
                                else if (type == StreamTokenizer.TT_WORD ||
                                         type == '\'' || type == '\"')
                                {
                                        current.args.addLast(new HasName.StringName(t.sval));
                                }
                                else if (type == StreamTokenizer.TT_NUMBER)
                                {
                                        if (t.nval != (double)(int)t.nval) {
                                                throw new ParseException("Only integers allowed");
                                        }
                                        current.args.addLast(new HasName.IntName((int)t.nval));
                                }
                                else if (type == ';' || type == StreamTokenizer.TT_EOL)
                                {
                                        commands.addLast(current);
                                        current = null;
                                }
                                else
                                {
                                        throw new ParseException("Stray character: " + type);
                                }
                        }
                        if (current != null)
                                commands.addLast(current);

                        StringBuilder result = new StringBuilder();
                        for (Command cmd : commands)
                        {
                                try
                                {
                                        result.append(core.command(cmd.command, cmd.getArgsArray()));
                                }
                                catch (Core.CoreReturnedErrorException ex)
                                {
                                        result.append("Error: ").append(ex.getMessage()).append('\n');
                                }
                        }
                        if (notify && (responseListener != null)) {
                                responseListener.responseReceived(result.toString());
                        }
                        return result.toString();
                } catch (IOException ex) {
                        throw new ParseException();
                }
        }
}
