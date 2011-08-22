/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import java.io.IOException;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;
import quanto.core.CommandException;
import quanto.core.CoreCommunicationException;
import quanto.core.CoreException;
import quanto.core.CoreExecutionException;
import quanto.core.CoreTerminatedException;
import quanto.core.UnknownCommandException;
import static quanto.core.protocol.Utils.*;

/**
 *
 * @author alemer
 */
public class ProtocolManager
{
	private final static Logger logger = Logger.getLogger("quanto.core.protocol");

	public static String quantoCoreExecutable = "quanto-core";

    private DebugInputStream dbgInputStream;
    private DebugOutputStream dbgOutputStream;
    private RequestWriter writer;
    private ProtocolReader reader;
    private Process backend;
    private int nextRequestId = 1;

    public ProtocolManager() throws CoreException {
    }

    public void startCore() throws CoreException {
		try {
			ProcessBuilder pb = new ProcessBuilder(quantoCoreExecutable, "--protocol");

			pb.redirectErrorStream(false);
			logger.log(Level.FINEST, "Starting {0}...", quantoCoreExecutable);
			backend = pb.start();
			logger.log(Level.FINEST, "{0} started successfully", quantoCoreExecutable);

            new StreamRedirector(backend.getErrorStream(), System.err).start();
            dbgInputStream = new DebugInputStream(backend.getInputStream());
            reader = new ProtocolReader(dbgInputStream);
            dbgOutputStream = new DebugOutputStream(backend.getOutputStream());
            writer = new RequestWriter(dbgOutputStream);
		} catch (IOException e) {
			logger.log(Level.SEVERE,
					   "Could not execute \"" + quantoCoreExecutable + "\": " +
					   e.getMessage(),
					   e);
			throw new CoreExecutionException(String.format(
					"Could not execute \"%1$\": %2$", quantoCoreExecutable,
					e.getMessage()), e);
		}

        try {
            reader.waitForReady();
            logger.log(Level.FINE,
                    "The core is running version {} of the protocol",
                    reader.getVersion());
            // FIXME: we should check that the core is running the version we expect
		} catch (IOException e) {
			logger.log(Level.SEVERE,
                       "The core failed to initiate the protocol correctly",
					   e);
			throw new CoreExecutionException(
                       "The core failed to initiate the protocol correctly",
					   e);
		}
    }

    public void setDebuggingEnabled(boolean enabled) {
        dbgInputStream.setDebuggingActive(enabled);
        dbgOutputStream.setDebuggingActive(enabled);
    }

    public boolean isDebuggingEnabled() {
        return dbgInputStream.isDebuggingActive();
    }

	private CoreCommunicationException writeFailure(Throwable e) {
		try {
			logger.log(Level.SEVERE,
					"Tried to write to core process, but it has terminated (exit value: {0})",
					backend.exitValue());
			// Not much we can do: throw an exception
			return new CoreTerminatedException();
		} catch (IllegalThreadStateException ex) {
			logger.log(Level.SEVERE,
					   "Failed to write to core process, even though it has not terminated");
			return new CoreCommunicationException();
		}
	}

	private CoreCommunicationException readFailure(Throwable e)  {
		try {
			logger.log(Level.SEVERE, "Core process terminated with exit value {0}",
					backend.exitValue());
			return new CoreTerminatedException(e);
		} catch (IllegalThreadStateException ex) {
			return new CoreCommunicationException(e);
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
                logger.log(Level.FINER, "Waiting for 5 seconds for the core to exit");
				sleep(5000);
			} catch (InterruptedException ex) {
                logger.log(Level.FINER, "Thread interupted");
			}
            logger.log(Level.FINER, "Forcibly terminating the core process");
			process.destroy();
		}
	}

	/**
	 * Quits the core process, and releases associated resources
	 */
	public void killCore() {
        if (backend != null) {
            logger.log(Level.FINEST, "Shutting down the core process");
            try {
                dbgInputStream.close();
                dbgOutputStream.close();
                new ProcessCleanupThread(backend).start();
            } catch (IOException ex) {
                logger.log(Level.WARNING, "Failed to close communication channels to the core");
            }
            backend = null;
        }
	}

    private String generateRequestId() {
        return Integer.toString(nextRequestId++);
    }

    private CommandException errorResponseToException(String code, String message) {
        return new CommandException(code, message);
    }

    private Response getResponse(Response.MessageType expectedType) throws CoreException {
        try {
            Response resp = reader.parseNextResponse();
            if (resp.isError()) {
                throw errorResponseToException(resp.getErrorCode(), resp.getErrorMessage());
            } else if (resp.getMessageType() == Response.MessageType.UnknownRequest) {
                throw new UnknownCommandException(resp.getRequestCode());
            } else if (resp.getMessageType() == Response.MessageType.UnknownResponse) {
                throw new ProtocolException("Got an unknown response message type");
            } else if (resp.getMessageType() != expectedType) {
                throw new ProtocolException("Expected a " + expectedType.toString() + " response, but got a " + resp.getMessageType().toString() + " response");
            }
            return resp;
        } catch (IOException ex) {
            throw readFailure(ex);
        }
    }

    private void getOkResponse() throws CoreException {
        getResponse(Response.MessageType.Ok);
    }

    private String getNameResponse() throws CoreException {
        return getResponse(Response.MessageType.Name).getStringData();
    }

    private String[] getNameListResponse() throws CoreException {
        return getResponse(Response.MessageType.NameList).getStringListData();
    }
 
    private byte[] getRawDataResponse() throws CoreException {
        return getResponse(Response.MessageType.RawData).getByteData();
    }
 
    private String getXmlResponse() throws CoreException {
        return utf8ToString(getResponse(Response.MessageType.Xml).getByteData());
    }

    /**
     * Execute an arbitrary console command.
     *
     * @param command  the command to execute, as typed by the user
     * @return the result of the command
     * @throws CoreException there was a communication error with the core
     */
    public String consoleCommand(String command) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("CC", generateRequestId());
            writer.addDataChunk(command);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        Response resp = getResponse(Response.MessageType.Console);
        return resp.getStringData();
    }

    public String[] consoleCommandList() throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("CL", generateRequestId());
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String[] consoleHelp(String command) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("CH", generateRequestId());
            writer.addString(command);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        Response resp = getResponse(Response.MessageType.ConsoleHelp);
        String[] result = new String[] {
            resp.getCommandArgs(),
            resp.getCommandHelp()
        };
        return result;
    }

    public void changeTheory(String theory) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("T", generateRequestId());
            writer.addString(theory);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }
        getOkResponse();
    }

    public String[] listGraphs() throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GL", generateRequestId());
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String loadEmptyGraph(String suggestedName) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GOE", generateRequestId());
            if (suggestedName != null)
                writer.addString(suggestedName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public String loadGraphFromFile(String fileName) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GOF", generateRequestId());
            writer.addString(fileName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public String loadGraphFromData(byte[] data) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GOD", generateRequestId());
            writer.addDataChunk(data);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public String copyGraph(String graph) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GOG", generateRequestId());
            writer.addString(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public void copySubgraphAndOverwrite(String from, String to, Collection<String> vertexNames) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GOS", generateRequestId());
            writer.addString(from);
            writer.addString(to);
            writer.addStringList(vertexNames);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void saveGraphToFile(String graph) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GS", generateRequestId());
            writer.addString(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public String renameGraph(String from, String to) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GR", generateRequestId());
            writer.addString(from);
            writer.addString(to);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public void discardGraph(String graph) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GD", generateRequestId());
            writer.addString(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public byte[] saveGraphToData(String graph) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GE", generateRequestId());
            writer.addString("native");
            writer.addString(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getRawDataResponse();
    }

    public enum GraphExportFormat {
        HilbertTerm,
        Mathematica,
        Matlab,
        Tikz
    }

    public String exportGraph(String graph, GraphExportFormat format) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GE", generateRequestId());
            switch (format) {
                case HilbertTerm:
                    writer.addString("hilb");
                    break;
                case Mathematica:
                    writer.addString("mathematica");
                    break;
                case Matlab:
                    writer.addString("matlab");
                    break;
                case Tikz:
                    writer.addString("tikz");
                    break;
                default:
                    throw new IllegalArgumentException("Bad format");
            }
            writer.addString(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return utf8ToString(getRawDataResponse());
    }

    public String[] listVertices(String graph) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GVV", generateRequestId());
            writer.addString(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String[] listEdges(String graph) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GVE", generateRequestId());
            writer.addString(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String[] listBangBoxes(String graph) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GVB", generateRequestId());
            writer.addString(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String vertexDataAsXml(String graph, String vertex) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GVW", generateRequestId());
            writer.addString(graph);
            writer.addString(vertex);
            writer.addString("xml");
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getXmlResponse();
    }

    public String vertexUserData(String graph, String vertex) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GVX", generateRequestId());
            writer.addString(graph);
            writer.addString(vertex);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return utf8ToString(getRawDataResponse());
    }

    public String edgeDataAsXml(String graph, String edge) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GVF", generateRequestId());
            writer.addString(graph);
            writer.addString(edge);
            writer.addString("xml");
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getXmlResponse();
    }

    public String edgeUserData(String graph, String edge) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GVG", generateRequestId());
            writer.addString(graph);
            writer.addString(edge);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return utf8ToString(getRawDataResponse());
    }

    public String[] bangBoxVertices(String graph, String bangBox) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GVC", generateRequestId());
            writer.addString(graph);
            writer.addString(bangBox);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String bangBoxUserData(String graph, String bangBox) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GVD", generateRequestId());
            writer.addString(graph);
            writer.addString(bangBox);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return utf8ToString(getRawDataResponse());
    }

    public void undo(String graph) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GMU", generateRequestId());
            writer.addString(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void redo(String graph) throws CoreException {
        if (backend == null)
            throw new IllegalStateException("The core is not running");

        try {
            writer.addHeader("GMR", generateRequestId());
            writer.addString(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }
}
