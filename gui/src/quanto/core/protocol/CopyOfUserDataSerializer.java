package quanto.core.protocol;

import java.awt.geom.Point2D;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import quanto.core.CoreException;
import quanto.core.data.CoreGraph;

/* A trivial implementation of the class which serializes Strings to ... Strings */
public class CopyOfUserDataSerializer implements UserDataSerializer<String> {

     private final static Logger logger = Logger.getLogger("quanto.protocol");
     public String dataTag = "copy_of";
     
     public CopyOfUserDataSerializer() {}
     
     public String dataToString(String vertexName) {
          return vertexName;
     }
     
     public String stringToData(String s) {
          return s;
     }

     public void setVertexUserData(ProtocolManager talker, CoreGraph g, String vertexName, String data){
          String dataString = dataToString(data);
          if (dataString == null) return;
          if (dataString.equals("")) return;
          try {
               talker.setVertexUserData(g.getCoreName(), vertexName, 
                         this.dataTag, dataString);
          } catch (CoreException e) {
               logger.log(Level.FINE, "Could not set data on vertex " 
                                             + vertexName, e);
          }
     }
     
     public String getVertexUserData(ProtocolManager talker, CoreGraph g, String vertexName) {
          String s;
          try {
               s = stringToData(talker.vertexUserData(g.getCoreName(), vertexName,
                         this.dataTag));
          } catch (CoreException e) {
               logger.log(Level.FINE, "Could not get copy_of on vertex " 
                         + vertexName, e);
               return null;
          }
          
          return s;
     }
     
     public void deleteVertexUserData(ProtocolManager talker, CoreGraph g,
               String VertexName) {
          
          try {
               talker.deleteVertexUserData(g.getCoreName(), VertexName, 
                         this.dataTag);
          } catch (CoreException e) {
               logger.log(Level.FINE, "Could not delete data on vertex " 
                                             + VertexName, e);
          }
     }
     
     /* Irrelevant for this type */
     public String getGraphUserData(ProtocolManager talker, CoreGraph g) {return null;}
     public void setGraphUserData(ProtocolManager talker, CoreGraph g, String data) {}
     public void deleteGraphUserData(ProtocolManager talker, CoreGraph g) {}
}

