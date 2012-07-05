signature TOP_DNET =
sig
  type T
  structure G : BANG_GRAPH
  
  val mk : G.T GraphName.NTab.T -> T
  val prune : G.T -> T -> T
  val fold : (G.T -> 'a -> 'a) -> 'a -> T -> T
end