/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collection;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;
import quanto.core.CoreCommunicationException;
import quanto.core.CoreException;
import quanto.core.CoreTerminatedException;
import static quanto.core.protocol.Utils.*;

/**
 * Manages communication with the core.
 *
 * @author alemer
 */
public class CoreTalker {

    private final static Logger logger = Logger.getLogger("quanto.core.protocol");

    private RequestWriter writer;
    private ResponseReader reader;
    private int nextRequestId = 1;

    public CoreTalker() {
    }
	
	public void connect(InputStream input, OutputStream output) throws IOException, ProtocolException {
		reader = new ResponseReader(input);
		writer = new RequestWriter(output);
		
		reader.waitForReady();
		logger.log(Level.FINE,
				"The core is running version {0} of the protocol",
				reader.getVersion());
		// FIXME: we should check that the core is running the version we expect
	}
	
	public void disconnect() {
        try {
			reader.close();
			writer.close();
		} catch (IOException ex) {
			logger.log(Level.WARNING, "Failed to close communication channels to the core", ex);
		}
		writer = null;
		reader = null;
	}

    protected CoreCommunicationException writeFailure(IOException e) {
		logger.log(Level.SEVERE,
				"Failed to write to core process; last received message was \"{0}\"",
				reader.getLastMessage());
		return new CoreCommunicationException(e);
    }

    protected CoreCommunicationException readFailure(IOException e) {
		if (reader.isClosed()) {
            logger.log(Level.SEVERE, "Core process disconnected; last received message was \"{0}\"",
					reader.getLastMessage());
            return new CoreTerminatedException(e);
		} else {
			return new CoreCommunicationException(e);
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

    private String getJsonResponse() throws CoreException {
        return getResponse(Response.MessageType.Json).getStringData();
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
        }

        try {
            writer.addHeader("CL", generateRequestId());
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameListResponse();
    }

    public void changeTheory(String theory) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public String loadEmptyGraph() throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
        }

        try {
            writer.addHeader("GOE", generateRequestId());
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getNameResponse();
    }

    public String loadGraphFromFile(String fileName) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public void copySubgraphAndOverwrite(String from, String to, Collection<String> vertexNames) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public String exportGraphAsJson(String graph) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
        }

        try {
            writer.addHeader("GE", generateRequestId());
            writer.addStringArg(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getJsonResponse();
    }

    public String graphUserData(String graph, String dataName) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public String vertexUserData(String graph, String vertex, String dataName) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public String edgeUserData(String graph, String edge, String dataName) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public String bangBoxUserData(String graph, String bangBox, String dataName) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
        }

        try {
            writer.addHeader("GVBU", generateRequestId());
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
         if (writer == null) {
             throw new IllegalStateException("Not connected to the core");
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
         if (writer == null) {
             throw new IllegalStateException("Not connected to the core");
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
         if (writer == null) {
             throw new IllegalStateException("Not connected to the core");
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
         if (writer == null) {
             throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
         if (writer == null) {
             throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
        }

        try {
            writer.addHeader("GMVA", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(vertexType);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }
        
        return getJsonResponse();
    }

    public String[] renameVertex(String graph, String from, String to) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
         if (writer == null) {
             throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
        }

        try {
            writer.addHeader("GMEA", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringArg(edgeType);
            writer.addStringArg(directed ? "d" : "u");
            writer.addStringArg(sourceVertex);
            writer.addStringArg(targetVertex);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getJsonResponse();
    }

    public void deleteEdges(String graph, Collection<String> edges) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public void setEdgeUserData(String graph, String edge, String dataName, String data) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
         if (writer == null) {
             throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
        }

        try {
            writer.addHeader("GMBA", generateRequestId());
            writer.addStringArg(graph);
            writer.addStringListArg(vertices != null ? vertices : Collections.<String>emptyList());
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getJsonResponse();
    }

    public void renameBangBox(String graph, String from, String to) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public String[] bangVertices(String graph, String bangBox, Collection<String> vertices) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

        return getNameListResponse();
    }

    public void unbangVertices(String graph, Collection<String> vertices) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
         if (writer == null) {
             throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public void replaceRulesetFromFile(String fileName) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public void exportRulesetToFile(String fileName) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public String[] listRules() throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public String ruleUserData(String ruleName, String dataName) throws CoreException {
         if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
        }

        try {
            writer.addHeader("RUD", generateRequestId());
            writer.addStringArg(ruleName);
            writer.addStringArg(dataName);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return utf8ToString(getRawDataResponse());
    }

    public void setRuleUserData(String ruleName, String dataName, String data) throws CoreException {

        try {
            writer.addHeader("SRUD", generateRequestId());
            writer.addStringArg(ruleName);
            writer.addStringArg(dataName);
            writer.addDataChunkArg(data);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        getOkResponse();
    }

    public String[] listTags() throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public void deleteRulesByTag(String tag) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public int attachRewrites(String graph, Collection<String> vertices) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
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

    public String applyAttachedRewrite(String graph, int offset) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
        }

        try {
            writer.addHeader("WW", generateRequestId());
            writer.addStringArg(graph);
            writer.addIntArg(offset);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getJsonResponse();
    }

    public String listAttachedRewrites(String graph) throws CoreException {
        if (writer == null) {
            throw new IllegalStateException("Not connected to the core");
        }

        try {
            writer.addHeader("WL", generateRequestId());
            writer.addStringArg(graph);
            writer.closeMessage();
        } catch (IOException ex) {
            throw writeFailure(ex);
        }

        return getJsonResponse();
    }
 }
