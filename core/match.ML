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
  val mk_id : BBox.NSet.T (* !-box names *)
              -> BVertex.NSet.T (* boundary *)
              -> BVertex.NSet.T (* all vnames *)
              -> Edge.NSet.T (* all enames *)
              -> T 
  val get_emap : T -> Edge.name Edge.NTab.T
  val get_ivinvmap : T -> BVertex.name BVertex.NTab.T
  val get_bvinvmap : T -> BVertex.NSet.T BVertex.NTab.T
  val get_bbmatch : T -> BBoxMatch.T
  val get_mctxt : T -> MCtxt.T
  val get_vmap : T -> BVertex.name BVertex.NTab.T
  val match_edges : Edge.data Edge.NTab.T ->
                    Edge.data Edge.NTab.T ->
                    T -> (Edge.name Edge.NTab.T * T) Seq.seq
  val match_bvertex : BVertex.name -> BVertex.name -> T -> T option
  val match_ivertex : BVertex.name * BVertex.IData.data ->
                      BVertex.name * BVertex.IData.data -> T 
                      -> T option
  val match_vertex : BVertex.name * BVertex.data ->
                     BVertex.name * BVertex.data -> T 
                     -> T option (* NONE if cannot be added as a match *)
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
structure BBoxMatch = BBoxMatch;

structure DEdge = 
  AddDataNameFun(
    structure Nm = Edge 
    type data = bool
    (* false = source, true = target *)
    fun ord_data (true, true) = EQUAL
      | ord_data (true, false) = GREATER
      | ord_data (false, true) = LESS
      | ord_data (false, false) = EQUAL
    fun pretty (true, n) = Pretty.block [Pretty.str "->", Nm.pretty_name n]
      | pretty (false,n) = Pretty.block [Pretty.str "<-", Nm.pretty_name n]
    val default_name = (true,Nm.default_name));

(* Edge matching not Needed: one step later we check nodes and that
   enforced edge injection conditions needed for var-nodes/half-edges*)
(* instantiations *)
datatype T = Match of 
         {(* pat to tgt, injective *)
          vmap: BVertex.name BVertex.NTab.T, 
          (* 1-1 except for two edges (to boundary nodes) can go 
             to 1 in tgt if tgt edge is a self loop *)
          emap: Edge.name Edge.NTab.T, 
          (* tgt to pat 1-1 for internal pat vertices *)
          ivinvmap : BVertex.name BVertex.NTab.T,
          (* tgt to pat (1-many) for boundary pat vertices *)        
          bvinvmap : BVertex.NSet.T BVertex.NTab.T,
          (* bang box isomorphism *)
          bbmatch : BBoxMatch.T,
          (* instantiations for vertex/edge data varaiables *)
          mctxt: MCtxt.T 
         };

val empty = Match {vmap = BVertex.NTab.empty,
                   emap = Edge.NTab.empty,
                   ivinvmap = BVertex.NTab.empty,
                   bvinvmap = BVertex.NTab.empty,
                   bbmatch = BBoxMatch.empty,
                   mctxt = MCtxt.empty
                  };

(* make identity match from boundary names, vnames, enames 
   vnames is all vnames, bvnames are the boundary subset of them, 
   ename is all enames. 
*)
fun mk_id boxnames bvnames vnames enames = 
    let 
      val vmap = (* mk id *)
          BVertex.NSet.fold 
            (fn n => BVertex.NTab.ins (n,n))
            vnames BVertex.NTab.empty;
      val emap = (* mk id *)
          Edge.NSet.fold 
            (fn n => Edge.NTab.ins (n,n))
            enames Edge.NTab.empty;
      val ivinvmap = 
          BVertex.NSet.fold 
            BVertex.NTab.delete bvnames vmap;
      val bvinvmap = (* mk id on bvnames *)
          BVertex.NSet.fold 
            (fn n => BVertex.NTab.ins (n,BVertex.NSet.single n))
            bvnames
            BVertex.NTab.empty
 
    in Match {vmap = vmap, emap = emap, 
              ivinvmap = ivinvmap, 
              bvinvmap = bvinvmap,
              bbmatch = BBoxMatch.mk_id boxnames,
              mctxt = MCtxt.empty}
    end;

(* updates *)
fun update_vmap f (Match rep) = 
    Match {vmap = f (#vmap rep), 
           emap = #emap rep,
           ivinvmap = #ivinvmap rep,
           bvinvmap = #bvinvmap rep,
           bbmatch = #bbmatch rep,
           mctxt = #mctxt rep};
fun update_emap f (Match rep) = 
    Match {vmap = #vmap rep, 
           emap = f (#emap rep),
           ivinvmap = #ivinvmap rep,
           bvinvmap = #bvinvmap rep,
           bbmatch = #bbmatch rep,
           mctxt = #mctxt rep};
fun update_ivinvmap f (Match rep) = 
    Match {vmap = #vmap rep, 
           emap = #emap rep,
           ivinvmap = f (#ivinvmap rep),
           bvinvmap = #bvinvmap rep,
           bbmatch = #bbmatch rep,
           mctxt = #mctxt rep};
fun update_bvinvmap f (Match rep) = 
    Match {vmap = #vmap rep, 
           emap = #emap rep,
           ivinvmap = #ivinvmap rep,
           bvinvmap = f (#bvinvmap rep),
           bbmatch = #bbmatch rep,
           mctxt = #mctxt rep};
fun update_bbmatch f (Match rep) = 
    Match {vmap = #vmap rep, 
           emap = #emap rep,
           ivinvmap = #ivinvmap rep,
           bvinvmap = #bvinvmap rep,
           bbmatch = f (#bbmatch rep),
           mctxt = #mctxt rep};
fun update_mctxt f (Match rep) = 
    Match {vmap = #vmap rep, 
           emap = #emap rep,
           ivinvmap = #ivinvmap rep,
           bvinvmap = #bvinvmap rep,
           bbmatch = #bbmatch rep,
           mctxt = f (#mctxt rep)};

fun get_vmap (Match rep) = #vmap rep;
fun get_emap (Match rep) = #emap rep;
fun get_ivinvmap (Match rep) = #ivinvmap rep;
fun get_bvinvmap (Match rep) = #bvinvmap rep;
fun get_bbmatch (Match rep) = #bbmatch rep;
fun get_mctxt (Match rep) = #mctxt rep;
val set_mctxt = update_mctxt o K;

(* implicit: m n *)
(* tgt is matched some internal vertex in pat *)
val tgt_vertex_is_imatched = BVertex.NTab.contains o get_ivinvmap;
(* tgt is matched some boundary vertex in pat *)
val tgt_vertex_is_bmatched = BVertex.NTab.contains o get_bvinvmap;

(* is boundary matchable if not matched, but is still fine if already bmatched. *)
val tgt_vertex_is_bmatchable = not oo tgt_vertex_is_imatched;

(* completely unmatched = can be matched by a new internal in pat *)
fun tgt_vertex_is_unmatched m n = 
    not (tgt_vertex_is_imatched m n)
    andalso 
    not (tgt_vertex_is_bmatched m n);

(* try match two internal vertices data, using internal data matching
   function from MCxtx *)
fun match_ivertex (n,d) (tn,td) m = 
    (* map must be injective for internal pat vertices *)
    if tgt_vertex_is_unmatched m tn then 
      case MCtxt.vertex_idata_match d td (get_mctxt m)
       of NONE => NONE (* vertices don't match *)
        | SOME mctxt2 => 
          SOME (m |> set_mctxt mctxt2
                  |> update_vmap (BVertex.NTab.ins (n,tn))
                  |> update_ivinvmap (BVertex.NTab.ins (tn,n)))
    else NONE; (* matched by some other pat vertex *)

fun match_bvertex n tn m = 
    if tgt_vertex_is_bmatchable m tn then 
      (* matched only by other pat bvertices *)
      SOME 
        (m |> update_vmap (BVertex.NTab.ins (n,tn))
           |> update_bvinvmap 
           (fn bvinvmap => 
               case BVertex.NTab.lookup bvinvmap tn 
                of NONE => BVertex.NTab.ins (tn, BVertex.NSet.single n) bvinvmap
                 | SOME pat_bvs => 
                   BVertex.NTab.update (tn, BVertex.NSet.add n pat_bvs) bvinvmap))
    else NONE; (* some internal vertex is already matched to the target *)

(* try to match two vertices *)
fun match_vertex (n, BVertex.BVert) (tn, _) m = match_bvertex n tn m
  (* internal pat vertex cannot match a bvertex (half-edge) *)
  | match_vertex (_, BVertex.IVert _) (_,BVertex.BVert) m = NONE
  (* internal to internal match match *)
  | match_vertex (pn, BVertex.IVert pd) (tn, BVertex.IVert td) m = 
    match_ivertex (pn,pd) (tn,td) m;

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
