package quanto.oldcore.protocol.userdata;

import java.util.logging.Level;
import java.util.logging.Logger;
import quanto.core.CoreException;
import quanto.oldcore.data.CoreGraph;
import quanto.oldcore.protocol.CommandException;
import quanto.oldcore.protocol.CoreTalker;
import quanto.oldcore.protocol.userdata.dataserialization.DataSerializer;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class GraphUserDataSerializer<T> extends QuantoAppUserDataSerializer<T> {

    private DataSerializer<T> serializer;
    private String suffix;
    private final static Logger logger = Logger.getLogger("quanto.protocol.userdata");

    public GraphUserDataSerializer(CoreTalker talker, DataSerializer<T> serializer, String suffix) {
        super(talker);
        this.serializer = serializer;
        this.suffix = suffix;
    }

    //Graph User Data
    public T getGraphUserData(CoreGraph graph) throws CoreException {
		try {
			return serializer.fromString(talker.graphUserData(graph.getCoreName(), prefix + suffix));
		} catch (CommandException e) {
            if (e.getCode().equals("NOSUCHGRAPHUSERDATA")) {
                logger.log(Level.FINER,
                        "No such data {0}{1} on graph {2}",
                        new Object[]{prefix, suffix, graph.getCoreName()});
            } else {
                logger.log(Level.WARNING, "Could not get data " +
                    prefix + suffix + " on graph " + graph.getCoreName(), e);
            }
            return null;
		}
    }
    public void setGraphUserData(CoreGraph graph, T data) throws CoreException {
        String dataString = serializer.toString(data);
        if (dataString == null) {
			return;
		}
        try {
            talker.setGraphUserData(graph.getCoreName(), prefix + suffix, dataString);
        } catch (CommandException e) {
            logger.log(Level.WARNING, "Could not set data " +
                    prefix + suffix + " on graph" + graph.getCoreName(), e);
        }
    }

    public void deleteGraphUserData(CoreGraph graph) throws CoreException {
        try {
            talker.deleteGraphUserData(graph.getCoreName(), prefix + suffix);
        } catch (CommandException e) {
             logger.log(Level.WARNING, "Could delete set data " +
                     prefix + suffix + " on graph" + graph.getCoreName(), e);
        }
    }

    public T getVertexUserData(CoreGraph graph, String vertexName) throws CoreException {
        try {
            return serializer.fromString(talker.vertexUserData(graph.getCoreName(), vertexName, prefix + suffix));
        } catch (CommandException e) {
            if (e.getCode().equals("NOSUCHVERTEXUSERDATA")) {
                logger.log(Level.FINER,
                        "No such data {0}{1} on vertex {2} of graph {3}",
                        new Object[]{prefix, suffix, vertexName, graph.getCoreName()});
            } else {
                logger.log(Level.WARNING, "Could not get data " +
                        prefix + suffix + " on vertex " + vertexName +
                        " of graph " + graph.getCoreName(), e);
            }
            return null;
        }
    }

    public void setVertexUserData(CoreGraph graph, String vertexName, T data) throws CoreException {
        String dataString = serializer.toString(data);
        if (dataString == null) {
			return;
		}
        try {
            talker.setVertexUserData(graph.getCoreName(), vertexName, prefix + suffix, dataString);
        } catch (CommandException e) {
            logger.log(Level.WARNING, "Could not set vertex user data " +
                    prefix + suffix + " on vertex " + vertexName +
                    " of graph " + graph.getCoreName(), e);
        }
    }
    public void deleteVertexUserData(CoreGraph graph, String vertexName) throws CoreException {
        try {
            talker.deleteVertexUserData(graph.getCoreName(), vertexName, prefix + suffix);
        } catch (CommandException e) {
            logger.log(Level.WARNING, "Cound not delete user data " +
                    prefix + suffix + " on vertex " + vertexName +
                    " of graph " + graph.getCoreName(), e);
        }
    }

    public T getEdgeUserData(CoreGraph graph, String edgeName) throws CoreException {
        try {
            return serializer.fromString(talker.edgeUserData(graph.getCoreName(), edgeName, prefix + suffix));
        } catch (CommandException e) {
            if (e.getCode().equals("NOSUCHEDGEUSERDATA")) {
                logger.log(Level.FINER,
                        "No such data {0}{1} on edge {2} of graph {3}",
                        new Object[]{prefix, suffix, edgeName, graph.getCoreName()});
            } else {
                logger.log(Level.WARNING, "Could not get data " +
                        prefix + suffix + " on edge " + edgeName +
                        " of graph " + graph.getCoreName(), e);
            }
            return null;
        }
    }

    public void setEdgeUserData(CoreGraph graph, String edgeName, T data) throws CoreException {
        String dataString = serializer.toString(data);
        if (dataString == null) {
			return;
		}
        try {
            talker.setEdgeUserData(graph.getCoreName(), edgeName, prefix + suffix, dataString);
        } catch (CommandException e) {
            logger.log(Level.WARNING, "Could not set user data " +
                    prefix + suffix + " on edge " + edgeName +
                    " of graph " + graph.getCoreName(), e);
        }
    }
    public void deleteEdgeUserData(CoreGraph graph, String edgeName) throws CoreException {
        try {
            talker.deleteEdgeUserData(graph.getCoreName(), edgeName, prefix + suffix);
        } catch (CommandException e) {
            logger.log(Level.WARNING, "Cound not delete user data " +
                    prefix + suffix + " on edge " + edgeName +
                    " of graph " + graph.getCoreName(), e);
        }
    }

    public T getBangBoxUserData(CoreGraph graph, String bbName) throws CoreException {
        try {
            return serializer.fromString(talker.bangBoxUserData(graph.getCoreName(), bbName, prefix + suffix));
        } catch (CommandException e) {
            if (e.getCode().equals("NOSUCHBANGBOXUSERDATA")) {
                logger.log(Level.FINER,
                        "No such data {0}{1} on !-box {2} of graph {3}",
                        new Object[]{prefix, suffix, bbName, graph.getCoreName()});
            } else {
                logger.log(Level.WARNING, "Could not get data " +
                        prefix + suffix + " on !-box " + bbName +
                        " of graph " + graph.getCoreName(), e);
            }
            return null;
        }
    }

    public void setBangBoxUserData(CoreGraph graph, String bbName, T data) throws CoreException {
        String dataString = serializer.toString(data);
        if (dataString == null) {
			return;
		}
        try {
            talker.setBangBoxUserData(graph.getCoreName(), bbName, prefix + suffix, dataString);
        } catch (CommandException e) {
            logger.log(Level.WARNING, "Could not set user data " +
                    prefix + suffix + " on !-box " + bbName +
                    " of graph " + graph.getCoreName(), e);
        }
    }

    public void deleteBangBoxUserData(CoreGraph graph, String bbName) throws CoreException {
        try {
            talker.deleteBangBoxUserData(graph.getCoreName(), bbName, prefix + suffix);
        } catch (CommandException e) {
            logger.log(Level.WARNING, "Cound not delete user data " +
                    prefix + suffix + " on !-box " + bbName +
                    " of graph " + graph.getCoreName(), e);
        }
    }
}
