package quanto.gui

import scala.collection.mutable.HashMap
import quanto.data.{GraphLoadException, Graph}

case class HGraphException (message: String) extends Exception

object HGraph{
  /*
  * note that the parent Map tree is not built when a graph is loaded. we only keep a set of graph, so when a
  * hgraph is opened, the relation map is automatically built.
  * */

   /* add a tree/map of graph here */
  /* "main" for toplevel graph, the the rest graph , node name should be used, and they should be unique */
  private val graphMap = new HashMap[String, Graph]
  private val parentOfMap = new HashMap[String, String]
  val toplevelKey = "main"
  var current = toplevelKey;
  parentOfMap +=  toplevelKey -> null

  def clearGraphMap () = {
    parentOfMap.clear();
    graphMap.clear();
    current = toplevelKey;
  }

  def current_= (g:Graph) {
    graphMap += toplevelKey -> g
  }

  def initGraph (gl : List[(String, Graph)]) = {
    gl map (graphMap.+=)
  }

  def addGraph (key : String,  g : Graph) {
    graphMap += key -> g;
    parentOfMap += key -> current
  }

  def addParentKey (key : String, pKey : String){
    parentOfMap += key -> pKey
  }

  def updateGraph (key : String, parentKey : String, g : Graph) {
    graphMap += key -> g;
    parentOfMap += key -> parentKey
  }

  def updateKey (oldK : String, newK : String) {
    graphMap +=  newK -> graphMap (oldK)
    graphMap -= (oldK)

    if (parentOfMap(oldK) != null) {
      parentOfMap +=  newK -> parentOfMap (oldK)
      parentOfMap -= (oldK)
    }
  }

  def validate () : Boolean  = {
    //TODO: check that
    // 1, no looping
    // 2, every key has a graph / is grounded
    true;
  }

  def getGraph (key : String) : Graph =  {
    try{
      graphMap (key);
    }catch{
      case _ : Throwable =>
        //println ("No such a graph : " + key)
        null
    }
  }

  def notToplevel () : Boolean = {
    current !=  toplevelKey
  }

  def getParentKey (key : String) : String = {
    try{
      parentOfMap (key)
    } catch{
      case _ : Throwable => null
    }
  }


  def getParentGraph (key : String) : Graph =  {
    try{
      val par =  parentOfMap (key)
      try{
        graphMap (par)
      }catch{
        case _ : Throwable => throw new HGraphException ("No such a graph : " + key)
      }
    }catch{
      case HGraphException (msg) => throw new HGraphException (msg)
      case _  : Throwable => throw new HGraphException ("No parent graph for : " + key)
    }
  }

  def getHierachicalString () : String = {
    def getKeyUntilNull (k : String, curHierachy: String) : String = {
      val parK = getParentKey (k)
      if ( parK != null){
        getKeyUntilNull (parK,  " -> " + k + curHierachy )
      }else{
        toplevelKey + curHierachy
      }
    }
    getKeyUntilNull (current, "");
  }


  def exportToList () : List[(String, Graph)] = {
    graphMap.toList
  }

}
