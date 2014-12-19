(*
 * An abstract structure supporting direct and inverse images, along with fast inversion
 * and composition. Other functors should work with amap's with more efficient/useful
 * representations.
 *)

signature ABS_MAP =
sig

  (* the graph of a mapping *)
  type ('a,'b) amapgraph
  val mk_graph : (unit -> 'a list) * ('a -> 'b list) -> ('a,'b) amapgraph

  (* store a representation and a means of converting to/from graphs *)
  datatype ('a,'b,'c) amap = MAP of 'c * ('c -> ('a,'b) amapgraph)
  val mk : 'c * ('c -> ('a,'b) amapgraph) -> ('a,'b,'c) amap
  
  val update_rep : ('c -> 'c) -> ('a,'b,'c) amap -> ('a,'b,'c) amap
  val get_rep    : ('a,'b,'c) amap -> 'c
  val set_rep    : 'c -> ('a,'b,'c) amap -> ('a,'b,'c) amap

  exception no_such_elem_exp of unit
  exception multiple_values_exp of unit
  exception duplicate_exp of unit
  exception bad_coercion_exp of unit

  val graph_of : ('a,'b,'c) amap -> ('a,'b) amapgraph
  val fold_graph : ('a * 'b -> 'c -> 'c) -> ('a,'b) amapgraph -> 'c -> 'c
  
  val compose : ('b,'c,'y) amap * ('a,'b,'x) amap -> ('a,'c,('a,'c) amapgraph) amap
  
  (* convenience function for composing 3 maps together *)
  val compose3 : ('c,'d,'z) amap * ('b,'c,'y) amap * ('a,'b,'x) amap -> ('a,'d,('a,'d) amapgraph) amap
  
  val fold : ('a * 'b -> 'd -> 'd) -> ('a,'b,'c) amap -> 'd -> 'd
  val exists : ('a * 'b -> bool) -> ('a,'b,'c) amap -> bool
  val forall : ('a * 'b -> bool) -> ('a,'b,'c) amap -> bool
  val find : ('a * 'b -> bool) -> ('a,'b,'c) amap -> ('a * 'b) option

  val pretty : string -> ('a -> Pretty.T) -> ('b -> Pretty.T) -> ('a,'b,'c) amap -> Pretty.T
  
  
end


structure Map :> ABS_MAP =
struct

  exception no_such_elem_exp of unit
  exception multiple_values_exp of unit
  exception duplicate_exp of unit
  exception bad_coercion_exp of unit

  datatype ('a,'b) amapgraph = MAPG of {
    dom: unit -> 'a list, di: 'a -> 'b list }
  fun mk_graph (dom,di) = MAPG { dom=dom, di=di }

  datatype ('a,'b,'c) amap = MAP of 'c * ('c -> ('a,'b) amapgraph)
  fun mk x = MAP x
  
  fun get_rep (MAP (rep,_)) = rep
  fun update_rep f (MAP (rep,gf)) = MAP (f(rep),gf)
  fun set_rep rep1 (MAP (_,gf)) = MAP (rep1,gf)
  (*val set_rep = update_rep o K*)

  fun graph_of (MAP (rep,gf)) = gf rep
  
  (* private: used for functions which return abstract maps (with trivial representation) *)
  fun of_graph gr = MAP (gr, I)

  fun fold_graph f g base = let
    val MAPG { dom, di, ... } = g
  in fold (fn x => fold (fn y => f (x,y)) (di x)) (dom ()) base
  end
  
  fun fold f = fold_graph f o graph_of
  
  fun find (f : 'a * 'b -> bool) m = let
    exception found_exp of 'a * 'b
  in (
       fold (fn (x,y) => fn () => if (f (x,y)) then raise found_exp (x,y) else ()) m ();
       NONE
     ) handle found_exp (x,y) => SOME (x,y)
  end
  
  fun forall f m = case (find (not o f) m) of SOME _ => false | NONE => true
  fun exists f m = case (find f m) of SOME _ => true | NONE => false

  fun compose_graphs (g2, g1) =
  let
    val MAPG { dom=dom1, di=di1 } = g1
    val MAPG { dom=_, di=di2 } = g2
  in MAPG { dom = dom1, di = maps di2 o di1 }
  end
  
  fun compose (m2,m1) = of_graph (compose_graphs (graph_of m2, graph_of m1))
  fun compose3 (m3,m2,m1) = compose (compose (m3,m2), m1)
  
  fun pretty desc pretty_key pretty_val mp =
  let
    val MAPG { dom, di, ... } = graph_of mp
    fun entry x ys = Pretty.block [Pretty.str "[", pretty_key x, Pretty.str "]", Pretty.str " -> ",
                                   Pretty.list "[" "]" (map pretty_val ys)]
    val entries = map (fn x => entry x (di x)) (dom ())
  in Pretty.chunks [Pretty.block [Pretty.str desc, Pretty.str " {"],
       Pretty.block [Pretty.str "  ",Pretty.chunks entries],
     Pretty.str "}"]
  end

end

