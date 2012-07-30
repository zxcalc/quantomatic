package quanto.core.protocol.userdata;

import quanto.core.CoreException;
import quanto.core.data.CoreGraph;
import quanto.core.protocol.ProtocolManager;
import quanto.core.protocol.userdata.dataserialization.DataSerializer;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class GraphUserDataSerializer<T> extends QuantoAppUserDataSerializer<T> {

    private DataSerializer serializer;
    private String suffix;
    private final static Logger logger = Logger.getLogger("quanto.protocol.userdata");

    public GraphUserDataSerializer(ProtocolManager talker, DataSerializer<T> serializer, String suffix) {
        super(talker);
        this.serializer = serializer;
        this.suffix = suffix;
    }

    //Graph User Data
    public T getGraphUserData(CoreGraph graph) {
		try {
			return (T) serializer.fromString(talker.graphUserData(graph.getCoreName(), prefix + suffix));
		} catch (CoreException e) {
			logger.log(Level.WARNING, "Could not get data " +
                    prefix + suffix + " on graph " + graph.getCoreName(), e);
            return null;
		}
    }
    public void setGraphUserData(CoreGraph graph, T data) {
        String dataString = serializer.toString(data);
        if (dataString == null)
            return;
        try {
            talker.setGraphUserData(graph.getCoreName(), prefix + suffix, dataString);
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Could not set data " + prefix + suffix
                    + " on graph" + graph.getCoreName(), e);
        }
    }

    public void deleteGraphUserData(CoreGraph graph) {
        try {
            talker.deleteGraphUserData(graph.getCoreName(), prefix + suffix);
        } catch (CoreException e) {
             logger.log(Level.WARNING, "Could delete set data " + prefix + suffix
                    + " on graph" + graph.getCoreName(), e);
        }
    }

    public T getVertexUserData(CoreGraph graph, String vertexName) {
        try {
            return (T) serializer.fromString(talker.vertexUserData(graph.getCoreName(), vertexName, prefix + suffix));
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Could not get vertex user data " + prefix + suffix + "on " +
                    "vertex " + vertexName, e);
            return null;
        }
    }

    public void setVertexUserData(CoreGraph graph, String vertexName, T data) {
        String dataString = serializer.toString(data);
        if (dataString == null)
            return;
        try {
            talker.setVertexUserData(graph.getCoreName(), vertexName, prefix + suffix, dataString);
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Could not set vertex user data " + prefix + suffix + " on" +
                    " vertex " + vertexName);
        }
    }
    public void deleteVertexUserData(CoreGraph graph, String vertexName) {
        try {
            talker.deleteVertexUserData(graph.getCoreName(), vertexName, prefix + suffix);
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Cound not delete user data " + prefix + suffix + " on" +
                    " vertex " + vertexName, e);
        }
    }

    public T getEdgeUserData(CoreGraph graph, String edgeName) {
        try {
            return (T) serializer.fromString(talker.edgeUserData(graph.getCoreName(), edgeName, prefix + suffix));
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Could not get user data " + prefix + suffix + "on " +
                    " " + edgeName, e);
            return null;
        }
    }

    public void setEdgeUserData(CoreGraph graph, String edgeName, T data) {
        String dataString = serializer.toString(data);
        if (dataString == null)
            return;
        try {
            talker.setEdgeUserData(graph.getCoreName(), edgeName, prefix + suffix, dataString);
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Could not set user data " + prefix + suffix + " on" +
                    " " + edgeName);
        }
    }
    public void deleteEdgeUserData(CoreGraph graph, String edgeName) {
        try {
            talker.deleteEdgeUserData(graph.getCoreName(), edgeName, prefix + suffix);
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Cound not delete user data " + prefix + suffix + " on" +
                    " " + edgeName, e);
        }
    }

    public T getBangBoxUserData(CoreGraph graph, String bbName) {
        try {
            return (T) serializer.fromString(talker.bangBoxUserData(graph.getCoreName(), bbName, prefix + suffix));
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Could not get user data " + prefix + suffix + "on " +
                    " " + bbName, e);
            return null;
        }
    }

    public void setBangBoxUserData(CoreGraph graph, String bbName, T data) {
        String dataString = serializer.toString(data);
        if (dataString == null)
            return;
        try {
            talker.setBangBoxUserData(graph.getCoreName(), bbName, prefix + suffix, dataString);
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Could not set user data " + prefix + suffix + " on" +
                    " " + bbName);
        }
    }

    public void deleteBangBoxUserData(CoreGraph graph, String bbName) {
        try {
            talker.deleteBangBoxUserData(graph.getCoreName(), bbName, prefix + suffix);
        } catch (CoreException e) {
            logger.log(Level.WARNING, "Cound not delete user data " + prefix + suffix + " on" +
                    " " + bbName, e);
        }
    }
}
