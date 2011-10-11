(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Vertex pair filters for matching  *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
signature FILTER
= sig
  structure Graph : GRAPH
  
  type input
  type T
  
  (* Initialize the filter from its required input data. *)
  val init : input -> T
  
  (* Apply the filter to a pair of vertices. *)
  val apply : T -> Graph.T -> Graph.T -> Graph.Vertex.name -> Graph.Vertex.name -> bool
  
end;


(* The trivial filter. Filters nothing. *)
functor TrivialFilterFun(
  structure Graph : GRAPH
) : FILTER
= struct
  structure Graph = Graph;
  
  type input = unit;
  type T = unit;
  
  fun init () = ();
  
  fun apply _ _ _ _ _ = true;
  
end;

(* A pair of filters. Lets a pair of vertices pass if it passes both filters. *)
functor FilterPairFun(
  structure Graph : GRAPH
  and First : FILTER
  and Second : FILTER
  sharing First.Graph.SharingBasicGraph = Second.Graph.SharingBasicGraph
  sharing First.Graph.SharingBasicGraph = Graph.SharingBasicGraph
) : FILTER
= struct
  structure Graph = Graph;
  structure First = First;
  structure Second = Second;
  
  type input = First.T*Second.T;
  type T = First.T*Second.T;
  
  fun init i = i;
  
  fun apply (f,s) p t pn tn = First.apply f p t pn tn
                     andalso Second.apply s p t pn tn;
  
end;