(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature MATCH 
= sig
  structure MCtxt : MATCH_CTXT
  (* sub-struct abbreviations *)
  structure BVertex : BVERTEX
  structure Edge : EDGE
  sharing Edge = MCtxt.Edge
  sharing BVertex = MCtxt.BVertex 

  type T
  val empty : T
  val mk_id : BVertex.NSet.T (* boundary *)
              -> BVertex.NSet.T (* all vnames *)
              -> Edge.NSet.T (* all enames *)
              -> T 
  val get_emap : T -> Edge.name Edge.NTab.T
  val get_ivinvmap : T -> BVertex.name BVertex.NTab.T
  val get_mctxt : T -> MCtxt.T
  val get_vmap : T -> BVertex.name BVertex.NTab.T
  val match_bvertex : BVertex.name -> BVertex.name -> T -> T
  val match_edges : Edge.data Edge.NTab.T ->
                    Edge.data Edge.NTab.T ->
                    T -> (Edge.name Edge.NTab.T * T) Seq.seq
  val match_vertex_idata : BVertex.name * BVertex.IData.data ->
                           BVertex.name * BVertex.IData.data -> T -> T option
  val pretty : T -> Pretty.T
  val print : T -> unit
  val set_mctxt : MCtxt.T -> T -> T
  val tgt_vertex_is_unmatched : T -> BVertex.name -> bool
  val update_emap : (Edge.name Edge.NTab.T -> Edge.name Edge.NTab.T)
                    -> T -> T
  val update_ivinvmap : (BVertex.name BVertex.NTab.T ->
       BVertex.name BVertex.NTab.T) -> T -> T
  val update_mctxt : (MCtxt.T -> MCtxt.T) -> T -> T
  val update_vmap : (BVertex.name BVertex.NTab.T ->
                     BVertex.name BVertex.NTab.T) -> T -> T
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor MatchFun(MCtxt : MATCH_CTXT) 
: MATCH
= struct

structure MCtxt = MCtxt;
structure BVertex = MCtxt.BVertex;
structure Edge = MCtxt.Edge;

(* Edge matching not Needed: one step later we check nodes and that
   enforced edge injection conditions needed for var-nodes/half-edges*)
(* instantiations *)
datatype T = Match of 
         {(* pat to tgt - do we need the data? *)
          vmap: BVertex.name BVertex.NTab.T, 
          emap: Edge.name Edge.NTab.T, 
          (* tgt to pat for internal (1-1 matching) vertices *)
          (* [many pat boundary vertices can go to 1 tgt vertex] *)
          ivinvmap : BVertex.name BVertex.NTab.T,
          (* instantiations for vertex/edge data varaiables *)
          mctxt: MCtxt.T 
         };

val empty = Match {vmap = BVertex.NTab.empty,
                   emap = Edge.NTab.empty,
                   ivinvmap = BVertex.NTab.empty,
                   mctxt = MCtxt.empty
                  };

(* make identity match from boundary names, vnames, enames *)
fun mk_id bvnames vnames enames = 
    let 
      val vmap = 
          BVertex.NSet.fold 
            (fn n => BVertex.NTab.ins (n,n))
            vnames BVertex.NTab.empty;
      val emap = 
          Edge.NSet.fold 
            (fn n => Edge.NTab.ins (n,n))
            enames Edge.NTab.empty;
      val ivinvmap = 
          BVertex.NSet.fold 
            BVertex.NTab.delete bvnames vmap;
    in Match {vmap = vmap, emap = emap, 
              ivinvmap = ivinvmap, mctxt = MCtxt.empty}
    end;

(* updates *)
fun update_vmap f (Match rep) = 
    Match {vmap = f (#vmap rep), 
           emap = #emap rep,
           ivinvmap = #ivinvmap rep,
           mctxt = #mctxt rep};
fun update_emap f (Match rep) = 
    Match {vmap = #vmap rep, 
           emap = f (#emap rep),
           ivinvmap = #ivinvmap rep,
           mctxt = #mctxt rep};
fun update_ivinvmap f (Match rep) = 
    Match {vmap = #vmap rep, 
           emap = #emap rep,
           ivinvmap = f (#ivinvmap rep),
           mctxt = #mctxt rep};
fun update_mctxt f (Match rep) = 
    Match {vmap = #vmap rep, 
           emap = #emap rep,
           ivinvmap = #ivinvmap rep,
           mctxt = f (#mctxt rep)};

fun get_vmap (Match rep) = #vmap rep;
fun get_emap (Match rep) = #emap rep;
fun get_ivinvmap (Match rep) = #ivinvmap rep;
fun get_mctxt (Match rep) = #mctxt rep;
val set_mctxt = update_mctxt o K;

(* implicit: m n *)
val tgt_vertex_is_unmatched = not oo BVertex.NTab.contains o get_ivinvmap;

(* try match two internal vertices data, using internal data matching
   function from MCxtx *)
fun match_vertex_idata (n,d) (tn,td) m = 
    (case MCtxt.vertex_idata_match d td (get_mctxt m)
      of NONE => NONE (* vertices don't match *)
       | SOME mctxt2 => 
         SOME (m |> set_mctxt mctxt2
                 |> update_vmap (BVertex.NTab.ins (n,tn))
                 |> update_ivinvmap (BVertex.NTab.ins (tn,n))));

fun match_bvertex n1 n2 m = 
    update_vmap (BVertex.NTab.ins (n1,n2)) m;

fun match_edges pat_edges tgt_edges m = 
    Seq.map
      (fn ((_,ematch),mctxt2) => 
          (ematch, 
           m |> update_emap (Edge.NTab.merge_disjoint ematch)
             |> set_mctxt mctxt2))
      (MCtxt.multi_edge_match pat_edges tgt_edges (get_mctxt m));

fun pretty m = 
    Pretty.chunks 
    ([Pretty.str "Match: ",
      (Pretty.indent 2 o Pretty.chunks)
        ([Pretty.block 
           [Pretty.str "VMatchings: ",
            Pretty.list "{" "}" 
                        (map (fn (pn,tn) => 
                                 Pretty.block 
                                   [Pretty.str "(",
                                    BVertex.pretty_name pn,
                                    Pretty.str "->",
                                    BVertex.pretty_name tn, 
                                    Pretty.str ")"])
                             (BVertex.NTab.list_of (get_vmap m)))]]
      @ [Pretty.block 
           [Pretty.str "InInvVMap: ",
            Pretty.list "{" "}" 
                        (map (fn (tn,pn) => 
                                 Pretty.block 
                                   [Pretty.str "(",
                                    BVertex.pretty_name pn,
                                     Pretty.str "<-",
                                    BVertex.pretty_name tn, 
                                    Pretty.str ")"])
                             (BVertex.NTab.list_of (get_ivinvmap m)))]]
      @ [Pretty.block 
           [Pretty.str "EMatchings: ",
            Pretty.list "{" "}" 
                        (map (fn (pn,tn) => 
                                 Pretty.block 
                                   [Pretty.str "(",
                                     Edge.pretty_name pn,
                                    Pretty.str "->",
                                    Edge.pretty_name tn, 
                                    Pretty.str ")"])
                             (Edge.NTab.list_of (get_emap m)))]]
      @ [Pretty.block [Pretty.str "MCtxt: ", MCtxt.pretty (get_mctxt m)]])
    ]);

val print = Pretty.writeln o pretty;

end; (* Match structure *)

structure Match = MatchFun(AngleMatchCtxt);
