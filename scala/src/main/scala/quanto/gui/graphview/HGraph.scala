package quanto.gui

import scala.collection.mutable.HashMap
import quanto.data.{GraphLoadException, Graph}

case class HGraphException (message: String) extends Exception

object HGraph{
  /* add a tree/map of graph here */
  /* "main" for toplevel graph, the the rest graph , node name should be used, and they should be unique */
  private val graphMap = new HashMap[String, Graph]
  private val parentOfMap = new HashMap[String, String]
  private val toplevelKey = "main"
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

  def addGraph (key : String,  g : Graph) {
    graphMap += key -> g;
    parentOfMap += key -> current
  }

  def updateGraph (key : String, parentKey : String, g : Graph) {
    graphMap += key -> g;
    parentOfMap += key -> parentKey
  }

  def updateKey (oldK : String, newK : String) {
    graphMap +=  newK -> graphMap (oldK)
    graphMap -= (oldK)

    parentOfMap +=  newK -> parentOfMap (oldK)
    parentOfMap -= (oldK)
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
    parentOfMap (key)
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
        getKeyUntilNull (parK, curHierachy + " <- " + k)
      }else{
        toplevelKey + curHierachy
      }
    }
    getKeyUntilNull (current, "");
  }

}
