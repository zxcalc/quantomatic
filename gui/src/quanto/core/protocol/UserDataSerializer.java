package quanto.core.protocol;

import quanto.core.data.CoreGraph;

public interface UserDataSerializer<D> {

	public String dataTag = null;
	public String dataToString(D data);
	public D      stringToData(String s);
	
	public void setVertexUserData(ProtocolManager talker, CoreGraph g, String vertexName, D data);
	public void setGraphUserData(ProtocolManager talker, CoreGraph g, D data);
	
	public D getVertexUserData(ProtocolManager talker, CoreGraph g, String vertexName);
	public D getGraphUserData(ProtocolManager talker, CoreGraph g);
}
