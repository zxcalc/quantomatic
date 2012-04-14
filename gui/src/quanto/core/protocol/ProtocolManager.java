/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;
import quanto.core.CoreCommunicationException;
import quanto.core.CoreException;
import quanto.core.CoreExecutionException;
import quanto.core.CoreTerminatedException;
import static quanto.core.protocol.Utils.*;

/**
 *
 * @author alemer
 */
public class ProtocolManager {

    private final static Logger logger = Logger.getLogger("quanto.core.protocol");

    public static String quantoCoreExecutable = "quanto-core";
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

            reader = new ProtocolReader(backend.getInputStream());
            writer = new RequestWriter(backend.getOutputStream());
        } catch (IOException e) {
            logger.log(Level.SEVERE,
                    "Could not execute \"" + quantoCoreExecutable + "\": "
                    + e.getMessage(),
                    e);
            throw new CoreExecutionException(String.format(
                    "Could not execute \"%1$\": %2$", quantoCoreExecutable,
                    e.getMessage()), e);
        }

        try {
            reader.waitForReady();
            logger.log(Level.FINE,
                    "The core is running version {0} of the protocol",
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

    private CoreCommunicationException writeFailure(Throwable e) {
        try {
            logger.log(Level.SEVERE,
                    "Tried to write to core process, but it has terminated (exit value: {0})",
                    backend.exitValue());
            // Not much we can do: throw an exception
            if (reader.getLastInvalidOutput() != null &&
                    !reader.getLastInvalidOutput().isEmpty()) {
                // probably an exception trace
                return new CoreTerminatedException(
                        "The core terminated with the following message:\n\n" +
                        reader.getLastInvalidOutput());
            } else {
                return new CoreTerminatedException();
            }
        } catch (IllegalThreadStateException ex) {
            logger.log(Level.SEVERE,
                    "Failed to write to core process, even though it has not terminated");
            return new CoreCommunicationException();
        }
    }

    private CoreCommunicationException readFailure(Throwable e) {
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
                reader.close();
                writer.close();
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
        if (code.equals("BADARGS"))
            return new CommandArgumentsException(message);
        else
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
        } catch (ProtocolException ex) {
            try {
                // try to get the exit value, because that's the only way
                // to see if it's terminated
                backend.exitValue();
                // yes, it exited
                if (reader.getLastInvalidOutput() != null &&
                        !reader.getLastInvalidOutput().isEmpty()) {
                    // probably an exception trace
                    throw new CoreTerminatedException(
                            "The core terminated with the following message:\n\n" +
                            reader.getLastInvalidOutput(), ex);
                } else {
                    throw new CoreTerminatedException(ex);
                }
            } catch (IllegalThreadStateException ex2) {
                // no, it didn't exit
                throw ex;
            }
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

    private int getCountResponse() throws CoreException {
        return getResponse(Response.MessageType.Count).getIntData();
    }

    /**
     * Execute an arbitrary console command.
     *
     * @param command  the command to execute, as typed by the user
     * @return the result of the command
     * @throws CoreException there was a communication error with the core
     */
    public String consoleCommand(String command) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("CC", generateRequestId());
            writer.addDataChunkArg(command);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        Response resp = getResponse(Response.MessageType.Console);
        return resp.getStringData();
    }

    public String[] consoleCommandList() throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("CL", generateRequestId());
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String[] consoleHelp(String command) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("CH", generateRequestId());
            writer.addStringArg(command);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        Response resp = getResponse(Response.MessageType.ConsoleHelp);
        String[] result = new String[]{
            resp.getCommandArgs(),
            resp.getCommandHelp()
        };
        return result;
    }

    public void changeTheory(String theory) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("TS", generateRequestId());
            writer.addStringArg(theory);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }
        getOkResponse();
    }

    public String currentTheory() throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("TG", generateRequestId());
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }
        return getNameResponse();
    }

    public String[] listGraphs() throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GL", generateRequestId());
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String loadEmptyGraph() throws CoreException {
        return loadEmptyGraph(null);
    }

    public String loadEmptyGraph(String suggestedName) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GOE", generateRequestId());
            if (suggestedName != null) {
                writer.addStringArg(suggestedName);
            }
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public String loadGraphFromFile(String fileName) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GOF", generateRequestId());
            writer.addStringArg(fileName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public String loadGraphFromData(String suggestedName, byte[] data) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GOD", generateRequestId());
            writer.addStringArg(suggestedName == null ? "" : suggestedName);
            writer.addDataChunkArg(data);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public String copyGraph(String graph) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GOG", generateRequestId());
            writer.addStringArg(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public void copySubgraphAndOverwrite(String from, String to, Collection<String> vertexNames) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GOS", generateRequestId());
            writer.addStringArg(from);
            writer.addStringArg(to);
            writer.addStringListArg(vertexNames);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void saveGraphToFile(String graph, String filename) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GS", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(filename);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public String renameGraph(String from, String to) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GR", generateRequestId());
            writer.addStringArg(from);
            writer.addStringArg(to);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public void discardGraph(String graph) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GD", generateRequestId());
            writer.addStringArg(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public byte[] saveGraphToData(String graph) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GE", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg("native");
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
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GE", generateRequestId());
            writer.addStringArg(graph);
            switch (format) {
                case HilbertTerm:
                    writer.addStringArg("hilb");
                    break;
                case Mathematica:
                    writer.addStringArg("mathematica");
                    break;
                case Matlab:
                    writer.addStringArg("matlab");
                    break;
                case Tikz:
                    writer.addStringArg("tikz");
                    break;
                default:
                    throw new IllegalArgumentException("Bad format");
            }
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return utf8ToString(getRawDataResponse());
    }

    public String exportGraphAsXml(String graph) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GE", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg("xml");
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getXmlResponse();
    }

    public String[] listVertices(String graph) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GVVL", generateRequestId());
            writer.addStringArg(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String graphUserData(String graph, String dataName) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GVGU", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(dataName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }
        
        return utf8ToString(getRawDataResponse());
    }
    
    public String[] listEdges(String graph) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GVEL", generateRequestId());
            writer.addStringArg(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String[] listBangBoxes(String graph) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GVBL", generateRequestId());
            writer.addStringArg(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String vertexDataAsXml(String graph, String vertex) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GVVD", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(vertex);
            writer.addStringArg("xml");
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getXmlResponse();
    }

    public String vertexUserData(String graph, String vertex, String dataName) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GVVU", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(vertex);
            writer.addStringArg(dataName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return utf8ToString(getRawDataResponse());
    }

    public String edgeDataAsXml(String graph, String edge) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GVED", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(edge);
            writer.addStringArg("xml");
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getXmlResponse();
    }

    public String edgeUserData(String graph, String edge, String dataName) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GVEU", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(edge);
            writer.addStringArg(dataName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return utf8ToString(getRawDataResponse());
    }

    public String[] bangBoxVertices(String graph, String bangBox) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GVBV", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(bangBox);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String bangBoxUserData(String graph, String bangBox, String dataName) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GVVU", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(bangBox);
            writer.addStringArg(dataName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return utf8ToString(getRawDataResponse());
    }

    public void undo(String graph) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMU", generateRequestId());
            writer.addStringArg(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void redo(String graph) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMR", generateRequestId());
            writer.addStringArg(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void undoRewrite(String graph) throws CoreException {
         if (backend == null) {
             throw new IllegalStateException("The core is not running");
         }

         try {
             writer.addHeader("GMUR", generateRequestId());
             writer.addStringArg(graph);
             writer.closeMessage();
         } catch (IOException ex) {
             throw writeFailure(ex);
         }

         getOkResponse();
     }

     public void redoRewrite(String graph) throws CoreException {
         if (backend == null) {
             throw new IllegalStateException("The core is not running");
         }

         try {
             writer.addHeader("GMRR", generateRequestId());
             writer.addStringArg(graph);
             writer.closeMessage();
         } catch (IOException ex) {
             throw writeFailure(ex);
         }

         getOkResponse();
     }
    
    public void startUndoGroup(String graph) throws CoreException {
         if (backend == null) {
             throw new IllegalStateException("The core is not running");
         }

         try {
             writer.addHeader("GMSU", generateRequestId());
             writer.addStringArg(graph);
             writer.closeMessage();
         } catch (IOException ex) {
             throw writeFailure(ex);
         }

         getOkResponse();
     }

    public void endUndoGroup(String graph) throws CoreException {
         if (backend == null) {
             throw new IllegalStateException("The core is not running");
         }

         try {
             writer.addHeader("GMFU", generateRequestId());
             writer.addStringArg(graph);
             writer.closeMessage();
         } catch (IOException ex) {
             throw writeFailure(ex);
         }

         getOkResponse();
     }
    
    public void insertGraph(String source, String target) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMI", generateRequestId());
            writer.addStringArg(target);
            writer.addStringArg(source);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void setGraphUserData(String graph, String dataName, String data) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMGU", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(dataName);
            writer.addDataChunkArg(data);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }
    
    public void deleteGraphUserData(String graph, String dataName) throws CoreException {
         if (backend == null) {
             throw new IllegalStateException("The core is not running");
         }

         try {
             writer.addHeader("GMDGU", generateRequestId());
             writer.addStringArg(graph);
             writer.addStringArg(dataName);
             writer.closeMessage();
         } catch (IOException ex) {
             throw writeFailure(ex);
         }

         getOkResponse();
     }
    
    public String addVertex(String graph, String vertexType) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMVA", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(vertexType);
            writer.addStringArg("xml");
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }
        
        return getXmlResponse();
    }

    public String[] renameVertex(String graph, String from, String to) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMVR", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(from);
            writer.addStringArg(to);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }
        return getNameListResponse();
    }

    public void deleteVertices(String graph, Collection<String> vertices) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMVD", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringListArg(vertices);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void setVertexData(String graph, String vertex, String data) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMVS", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(vertex);
            writer.addTaggedDataChunkArg('N', data);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void setVertexUserData(String graph, String vertex, String dataName, String data) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMVU", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(vertex);
            writer.addStringArg(dataName);
            writer.addDataChunkArg(data);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void deleteVertexUserData(String graph, String vertex, String dataName) throws CoreException {
         if (backend == null) {
             throw new IllegalStateException("The core is not running");
         }

         try {
             writer.addHeader("GMDVU", generateRequestId());
             writer.addStringArg(graph);
             writer.addStringArg(vertex);
             writer.addStringArg(dataName);
             writer.closeMessage();
         } catch (IOException ex) {
             throw writeFailure(ex);
         }

         getOkResponse();
     }
    
    public String addEdge(String graph, String edgeType, boolean directed, String sourceVertex, String targetVertex) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMEA", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(edgeType);
            writer.addStringArg(directed ? "d" : "u");
            writer.addStringArg(sourceVertex);
            writer.addStringArg(targetVertex);
            writer.addStringArg("xml");
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getXmlResponse();
    }

    public String renameEdge(String graph, String from, String to) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMER", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(from);
            writer.addStringArg(to);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public void deleteEdges(String graph, Collection<String> edges) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMED", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringListArg(edges);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void setEdgeData(String graph, String edge, String data) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMES", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(edge);
            writer.addTaggedDataChunkArg('N', data);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void setEdgeUserData(String graph, String edge, String dataName, String data) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMEU", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(edge);
            writer.addStringArg(dataName);
            writer.addDataChunkArg(data);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void deleteEdgeUserData(String graph, String edge, String dataName) throws CoreException {
         if (backend == null) {
             throw new IllegalStateException("The core is not running");
         }

         try {
             writer.addHeader("GMDEU", generateRequestId());
             writer.addStringArg(graph);
             writer.addStringArg(edge);
             writer.addStringArg(dataName);
             writer.closeMessage();
         } catch (IOException ex) {
             throw writeFailure(ex);
         }

         getOkResponse();
     }
    
    public String addBangBox(String graph, Collection<String> vertices) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMBA", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringListArg(vertices != null ? vertices : Collections.<String>emptyList());
            writer.addStringArg("name");
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public void renameBangBox(String graph, String from, String to) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMBR", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(from);
            writer.addStringArg(to);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void dropBangBoxes(String graph, Collection<String> bangBoxes) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMBD", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringListArg(bangBoxes);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void killBangBoxes(String graph, Collection<String> bangBoxes) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMBK", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringListArg(bangBoxes);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public String duplicateBangBox(String graph, String bangBox) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMBC", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(bangBox);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public String mergeBangBoxes(String graph, Collection<String> bangBoxes) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMBM", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringListArg(bangBoxes);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public void bangVertices(String graph, String bangBox, Collection<String> vertices) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMBB", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(bangBox);
            writer.addStringListArg(vertices);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void unbangVertices(String graph, Collection<String> vertices) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMBL", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringListArg(vertices);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void setBangBoxUserData(String graph, String bangBox, String dataName, String data) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("GMBU", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(bangBox);
            writer.addStringArg(dataName);
            writer.addDataChunkArg(data);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }
    
    public void deleteBangBoxUserData(String graph, String bangBox, String dataName) throws CoreException {
         if (backend == null) {
             throw new IllegalStateException("The core is not running");
         }

         try {
             writer.addHeader("GMDBU", generateRequestId());
             writer.addStringArg(graph);
             writer.addStringArg(bangBox);
             writer.addStringArg(dataName);
             writer.closeMessage();
         } catch (IOException ex) {
             throw writeFailure(ex);
         }

         getOkResponse();
     }

    public void importRulesetFromFile(String fileName) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RSO", generateRequestId());
            writer.addStringArg(fileName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void importRulesetFromData(byte[] data) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RSI", generateRequestId());
            writer.addDataChunkArg(data);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void replaceRulesetFromFile(String fileName) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RSP", generateRequestId());
            writer.addStringArg(fileName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void replaceRulesetFromData(byte[] data) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RSJ", generateRequestId());
            writer.addDataChunkArg(data);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void exportRulesetToFile(String fileName) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RSS", generateRequestId());
            writer.addStringArg(fileName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public byte[] exportRulesetToData() throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RSE", generateRequestId());
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getRawDataResponse();
    }

    public String[] listRules() throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RRL", generateRequestId());
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String[] listActiveRules() throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RRA", generateRequestId());
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String openRuleLhs(String rule) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RRP", generateRequestId());
            writer.addStringArg(rule);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public String openRuleRhs(String rule) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RRQ", generateRequestId());
            writer.addStringArg(rule);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public void setRule(String ruleName, String lhsGraph, String rhsGraph) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RRU", generateRequestId());
            writer.addStringArg(ruleName);
            writer.addStringArg(lhsGraph);
            writer.addStringArg(rhsGraph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void renameRule(String oldName, String newName) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RRR", generateRequestId());
            writer.addStringArg(oldName);
            writer.addStringArg(newName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void deleteRule(String ruleName) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RRD", generateRequestId());
            writer.addStringArg(ruleName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void activateRule(String ruleName) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RRY", generateRequestId());
            writer.addStringArg(ruleName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void deactivateRule(String ruleName) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RRN", generateRequestId());
            writer.addStringArg(ruleName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public String[] listTags() throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RTL", generateRequestId());
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public String[] listRulesByTag(String tag) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RTR", generateRequestId());
            writer.addStringArg(tag);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public void tagRule(String ruleName, String tag) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RTT", generateRequestId());
            writer.addStringArg(ruleName);
            writer.addStringArg(tag);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void untagRule(String ruleName, String tag) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RTU", generateRequestId());
            writer.addStringArg(ruleName);
            writer.addStringArg(tag);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void forgetTag(String tag) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RTF", generateRequestId());
            writer.addStringArg(tag);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void deleteRulesByTag(String tag) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RTD", generateRequestId());
            writer.addStringArg(tag);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void activateRulesByTag(String tag) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RTY", generateRequestId());
            writer.addStringArg(tag);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public void deactivateRulesByTag(String tag) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("RTN", generateRequestId());
            writer.addStringArg(tag);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public int attachRewrites(String graph) throws CoreException {
        return attachRewrites(graph, Collections.<String>emptyList());
    }

    public int attachRewrites(String graph, Collection<String> vertices) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("WA", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringListArg(vertices);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getCountResponse();
    }

    public int attachOneRewrite(String graph) throws CoreException {
        return attachOneRewrite(graph, Collections.<String>emptyList());
    }

    public int attachOneRewrite(String graph, Collection<String> vertices) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("WO", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringListArg(vertices);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getCountResponse();
    }

    public void applyAttachedRewrite(String graph, int offset) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("WW", generateRequestId());
            writer.addStringArg(graph);
            writer.addIntArg(offset);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public String listAttachedRewrites(String graph) throws CoreException {
        if (backend == null) {
            throw new IllegalStateException("The core is not running");
        }

        try {
            writer.addHeader("WL", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg("xml");
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getXmlResponse();
    }
}
