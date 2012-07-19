signature CONTOUR =
sig
  type T
  structure G : BANG_GRAPH
  structure L : LITERAL

  val empty : T
  val add_node : T * L.T -> T
  val get_first_node: T -> L.T
  val is_contained : L.T * T -> bool

  val mk : G.T * T -> T
  val target_function : G.T -> T

  val check_strong_compatibility : T * T -> bool
  val check_weak_compatibility : T * T -> bool

end



functor Contour ( G : BANG_GRAPH ) : CONTOUR =
struct

  structure G = G
  structure L = Literal(G)

  type T = L.T list;

  val empty = [];

  (* Add a node to a contour *)
  fun add_node(c,l) = l::c

  (* Get the first node in a contour *)
  fun get_first_node(l::literals) = l
    | get_first_node(l::[]) = l

  (* Helper function to find if a literal is contained in a list of literals *)
  fun is_contained(x,[]) = false
    | is_contained(x,l::[]) = if (V.string_of_name (#name x) = V.string_of_name (#name l)) then true else false
    | is_contained(x,l::literals) = if (V.string_of_name (#name x) = V.string_of_name (#name l)) then true else is_contained(x,literals)

  (* Helper function to remove literals corresponding to the same node *)
  fun remove_duplicate(e,l::literals) = if is_contained(l,e) then remove_duplicate(e,literals) else remove_duplicate (l::e,literals)
    | remove_duplicate(e,l::[]) = if is_contained(l,e) then e else l::e


  (* Build the first contour given a graph *)
  fun target_function(g) =
      let
        val vnames = G.get_unbboxed(g)
        val v::vlist = V.NSet.list_of(vnames)
        val l = L.mk(g,v)
        val c = empty
      in
        l::c
      end

  (* Build a contour starting from another contour *)
  fun mk(g,c) = 
        let
          val g_list = replicate (length c) g
          val gl_list = g_list ~~ c

          val adj_nodes = maps L.get_adj gl_list
          val e = empty
        in
          remove_duplicate(e,adj_nodes)
        end


  (* This function checks if c1 is strongly compatible with c2, which may be different from checking if c2 is strongly compatible with c1 *)
  fun check_strong_compatibility (c1,c2) = false

  (* This function checks if c1 is weakly compatible with c2, which may be different from checking if c2 is weakly compatible with c1 *)
  fun check_weak_compatibility (c1,c2) = false

end