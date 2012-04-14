package quanto.core.protocol;

import quanto.core.data.CoreGraph;

public interface UserDataSerializer<D> {

	public String dataTag = null;
	public String dataToString(D data);
	public D      stringToData(String s);
	
	public void setVertexUserData(ProtocolManager talker, CoreGraph g, String vertexName, D data);
	public void setGraphUserData(ProtocolManager talker, CoreGraph g, D data);
	
	public D getVertexUserData(ProtocolManager talker, CoreGraph g, String vertexName);
	public D getEdgeUserData(ProtocolManager talker, CoreGraph g, String edgeName);
	public D getBangBoxUserData(ProtocolManager talker, CoreGraph g, String bangBoxName);
	public D getGraphUserData(ProtocolManager talker, CoreGraph g);
	
	public void deleteVertexUserData(ProtocolManager talker, CoreGraph g, String vertexName);
	public void deleteEdgeUserData(ProtocolManager talker, CoreGraph g, String edgeName);
	public void deleteBangBoxUserData(ProtocolManager talker, CoreGraph g, String bangBoxName);
	public void deleteGraphUserData(ProtocolManager talker, CoreGraph g);
}
