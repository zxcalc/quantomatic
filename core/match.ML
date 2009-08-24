(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*   Matching between two graphs without bang boxes but with boundary      *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature MATCH 
= sig
  structure MCtxt : MATCH_CTXT
  (* sub-struct abbreviations *)
  structure BVertex : BVERTEX
  structure Edge : EDGE
  structure EMap : NAME_AMORPH
  
  structure Sharing : BGRAPH_SHARING
  sharing Sharing.EdgeSharing = Edge.EdgeSharingWithData
  sharing Sharing.BVertexSharing = BVertex.SharingWithIData
  sharing Edge.Ehn = EMap.Dom
  sharing Edge.Ehn = EMap.Cod
  sharing Edge.EdgeSharingWithData = MCtxt.Edge.EdgeSharingWithData
  sharing BVertex.SharingWithIData = MCtxt.BVertex.SharingWithIData
  
  type T
  val empty : T
  val mk_id : BVertex.NSet.T (* boundary *)
              -> BVertex.NSet.T (* all vnames *)
              -> Edge.NSet.T (* all enames *)
              -> T 
  val get_emap : T -> EMap.T
  val get_invmap : T -> BVertex.name BVertex.NTab.T
  val get_lfinvmap : T -> BVertex.NSet.T BVertex.NTab.T
  val get_mctxt : T -> MCtxt.T
  val get_vmap : T -> BVertex.name BVertex.NTab.T
  val get_vinsts : T -> BVertex.IData.data BVertex.NTab.T

  (* opposites *)
  val tgt_edge_is_half_unmatched : T -> Edge.name -> bool
  val tgt_edge_is_fully_matched : T -> Edge.name -> bool

  (* *)
  val tgt_vertex_is_unmatched : T -> BVertex.name -> bool

  (* match many edges *)
  val multi_edge_match : Edge.data Edge.NTab.T 
                         -> Edge.data Edge.NTab.T 
                         -> T 
                         -> (Edge.data Edge.NTab.T (* left over unmatched *)
                             * (Edge.NSet.T (* matches edges *)
                                * T)) Seq.seq
  val strict_multi_edge_match : Edge.data Edge.NTab.T 
                                -> Edge.data Edge.NTab.T 
                                -> T -> (Edge.data Edge.NTab.T (* empty *)
                                        * (Edge.NSet.T (* matched = all tgt edges *) 
                                           * T)) Seq.seq (* no left over in tgt *)

  val match_lfvertex : BVertex.name (* leaf name *)
                       -> BVertex.name * BVertex.data (* tgt name and data *)
                       -> T -> T
  val match_vertex : BVertex.name * BVertex.data
                     -> BVertex.name * BVertex.data 
                     -> T -> T option

  (* FIXME: change to seq/option types, respect failure! don't use exeptions *)
  (* adds given half to a free half of edge *)
  val add_leaf_ehn2edge_emap : Edge.Ehn.name * Edge.data (* pat *)
                               -> Edge.name * Edge.data (* tgt *)
                               -> T -> T
  (* only adds this half *)
  val add_leaf_ehn2ehn_emap : Edge.Ehn.name * Edge.data 
                              -> Edge.Ehn.name * Edge.data 
                              -> T -> T

  (* adds both halfs as matching *)
  val add_full_leaf_ehn2ehn_emap : Edge.Ehn.name * Edge.data 
                              -> Edge.Ehn.name * Edge.data 
                              -> T -> T option 

  val pretty : T -> Pretty.T
  val print : T -> unit
  val set_mctxt : MCtxt.T -> T -> T


(* 
  val update_emap : (EMap.T -> EMap.T) -> T -> T
  val update_invmap : (BVertex.name BVertex.NTab.T ->
                       BVertex.name BVertex.NTab.T) -> T -> T
  val update_mctxt : (MCtxt.T -> MCtxt.T) -> T -> T
  val update_vmap : (BVertex.name BVertex.NTab.T ->
                     BVertex.name BVertex.NTab.T) -> T -> T
*)
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor MatchFun(MCtxt : MATCH_CTXT) 
(* : MATCH *)
= struct

structure MCtxt = MCtxt;
structure BVertex = MCtxt.BVertex;
structure Edge = MCtxt.Edge;

structure Sharing = struct
  structure VertexSharing = BVertex.SharingWithData;
  structure EdgeSharing = Edge.EdgeSharingWithData;
  structure BVertexSharing = BVertex.SharingWithIData;
end;

structure Ehn = Edge.Ehn;
structure EMap = NameAmorphFun(Ehn);

(* Matching Invarient: matching is  *)

(* a pat leaf-edge "x" has an invarient direction mapping "s(x) ->
   t(y)" and "t(x) -> t(y)" (or "s(x) -> s(y)" and "t(x) -> s(y)") iff
   "y" is a looping edge in the target; in this case the pat leaf-node
   is mapped to the loopy tgt node.  All other half-edges must be
   mapped 1-1; pattern vertices can match many-1. The EMap is 1-1,
   normally "s(x) -> s(y) <=> t(x) <=> t(y)". Two leaf-edges map to a
   single loopy edge by by having opposite invarients. *)

(* Edge matching not Needed: one step later we check nodes and that
   enforced edge injection conditions needed for var-nodes/half-edges*)
(* instantiations *)
datatype T = Match of 
         {(* pat to tgt, injective *)
          vmap: BVertex.name BVertex.NTab.T,
          (* vertex data instantiations from dom of vmap names,
             created when mapping bvertices to internal ones *)
          vinsts : BVertex.IData.data BVertex.NTab.T, (* NOTE: not used by rule! *)
          (* 1-1, inj half-edge mapping *)
          emap: EMap.T,
          (* tgt to pat 1-1 for non-leaf pat vertices *)
          invmap : BVertex.name BVertex.NTab.T,
          (* tgt to pat (1-many) for leaf vertices (boundaries with 1 edge) *)        
          lfinvmap : BVertex.NSet.T BVertex.NTab.T,
          (* instantiations for vertex/edge data varaiables *)
          mctxt: MCtxt.T 
         };

val empty = Match {vmap = BVertex.NTab.empty,
                   vinsts = BVertex.NTab.empty,
                   emap = EMap.empty,
                   invmap = BVertex.NTab.empty,
                   lfinvmap = BVertex.NTab.empty,
                   mctxt = MCtxt.empty
                  };

(* make identity match from lfvnames, vnames, enames;
   vnames is all vnames, lfvnames are the leaf subset of them, 
   enames is all enames. 
*)
fun mk_id lfvnames vnames enames = 
    let 
      val vmap = (* mk id *)
          BVertex.NSet.fold 
            (fn n => BVertex.NTab.ins (n,n))
            vnames BVertex.NTab.empty;
      val emap = (* id never has leaf-edge mapped to loops, so direct map *)
          Edge.NSet.fold 
            (* tgt mapping not needed; we always start by mapping src
            - helps keep table small *)
            (fn n => EMap.add (Ehn.mk_start n) (Ehn.mk_start n)
                     o EMap.add (Ehn.mk_end n) (Ehn.mk_end n))
            enames EMap.empty;
      val invmap = 
          BVertex.NSet.fold 
            BVertex.NTab.delete lfvnames vmap;
      val lfinvmap = (* mk id on bvnames *)
          BVertex.NSet.fold 
            (fn n => BVertex.NTab.ins (n,BVertex.NSet.single n))
            lfvnames
            BVertex.NTab.empty
    in Match {vmap = vmap, 
              vinsts = BVertex.NTab.empty,
              emap = emap, 
              invmap = invmap, 
              lfinvmap = lfinvmap,
              mctxt = MCtxt.empty}
    end;

(* updates *)
fun update_vmap f (Match rep) = 
    Match {vmap = f (#vmap rep), 
           vinsts = #vinsts rep,
           emap = #emap rep,
           invmap = #invmap rep,
           lfinvmap = #lfinvmap rep,
           mctxt = #mctxt rep};
fun update_emap f (Match rep) = 
    Match {vmap = #vmap rep, 
           vinsts = #vinsts rep,
           emap = f (#emap rep),
           invmap = #invmap rep,
           lfinvmap = #lfinvmap rep,
           mctxt = #mctxt rep};
fun update_invmap f (Match rep) = 
    Match {vmap = #vmap rep, 
           vinsts = #vinsts rep,
           emap = #emap rep,
           invmap = f (#invmap rep),
           lfinvmap = #lfinvmap rep,
           mctxt = #mctxt rep};
fun update_lfinvmap f (Match rep) = 
    Match {vmap = #vmap rep, 
           vinsts = #vinsts rep,
           emap = #emap rep,
           invmap = #invmap rep,
           lfinvmap = f (#lfinvmap rep),
           mctxt = #mctxt rep};
fun update_mctxt f (Match rep) = 
    Match {vmap = #vmap rep, 
           vinsts = #vinsts rep,
           emap = #emap rep,
           invmap = #invmap rep,
           lfinvmap = #lfinvmap rep,
           mctxt = f (#mctxt rep)};
fun update_vinsts f (Match rep) = 
    Match {vmap = #vmap rep, 
           vinsts = f(#vinsts rep),
           emap = #emap rep,
           invmap = #invmap rep,
           lfinvmap = #lfinvmap rep,
           mctxt = #mctxt rep};


fun get_vmap (Match rep) = #vmap rep;
fun get_vinsts (Match rep) = #vinsts rep;
fun get_emap (Match rep) = #emap rep;
fun get_invmap (Match rep) = #invmap rep;
fun get_lfinvmap (Match rep) = #lfinvmap rep;
fun get_mctxt (Match rep) = #mctxt rep;
val set_mctxt = update_mctxt o K;

(* implicit: m n *)
(* tgt is matched by some non-leaf vertex in pat *)
val tgt_vertex_is_matched = BVertex.NTab.contains o get_invmap;
(* tgt is matched by some leaves in pat *)
val tgt_vertex_is_lfmatched = BVertex.NTab.contains o get_lfinvmap;

(* is boundary matchable if not matched, but is still fine if already bmatched. *)
val tgt_vertex_is_matchable = not oo tgt_vertex_is_matched;

(* completely unmatched = can be matched by a new internal in pat *)
fun tgt_vertex_is_unmatched m n = 
    not (tgt_vertex_is_matched m n)
    andalso 
    not (tgt_vertex_is_lfmatched m n);


(* for when boundary vertices get instantiated to internal tgt vertices *)
(* implicit: m *)
fun add_vinsts (n, BVertex.BVert) m = m
  | add_vinsts (n, BVertex.IVert d) m = 
    update_vinsts 
      (fn vinsts => 
          case BVertex.NTab.lookup vinsts n 
           of NONE => BVertex.NTab.ins (n,d) vinsts (* add instantiation *)
            | SOME _ => vinsts) (* already instantiated *)
      m;


(* try match non-leaf pat vertex to tgt-vertex, using internal data matching
   function from Mctxt *)
fun match_vertex (n,BVertex.IVert _) (tn,BVertex.BVert) m = NONE
  | match_vertex (n,BVertex.IVert d) (tn,BVertex.IVert td) m = 
    (* map must be injective for internal pat vertices *)
    if tgt_vertex_is_matchable m tn then 
      case MCtxt.vertex_idata_match d td (get_mctxt m)
       of NONE => NONE (* vertices don't match *)
        | SOME mctxt2 => 
          SOME (m |> set_mctxt mctxt2
                  |> update_vmap (BVertex.NTab.ins (n,tn))
                  |> update_invmap (BVertex.NTab.ins (tn,n)))
    else NONE (* matched by some other pat vertex *)
  | match_vertex (n,BVertex.BVert) (tn,td) m = 
    if tgt_vertex_is_matchable m tn then 
      SOME (m |> update_vmap (BVertex.NTab.ins (n,tn))
              |> update_invmap (BVertex.NTab.ins (tn,n))
              |> add_vinsts (n,td))
    else NONE; (* matched by some other pat vertex *)

(* add a match from a leaf pat vertex to a tgt vertex; always succeeds *)
fun match_lfvertex lfn (tn,td) m = 
    (m |> update_vmap (BVertex.NTab.ins (lfn,tn))
       |> add_vinsts (lfn,td)
       |> update_lfinvmap
       (fn lfinvmap => 
           case BVertex.NTab.lookup lfinvmap tn 
            of NONE => BVertex.NTab.ins (tn, BVertex.NSet.single lfn) lfinvmap
             | SOME lf_bvs => 
               BVertex.NTab.update (tn, BVertex.NSet.add lfn lf_bvs) lfinvmap));

(* given tgt edge is completely unmapped: both start and end are unmatched *)
fun tgt_edge_is_unmapped m e = 
    let val emap = get_emap m
    in case EMap.lookup_codf emap (Ehn.mk_start e) 
        of NONE => (case EMap.lookup_codf emap (Ehn.mk_end e) 
                     of NONE => true
                      | SOME _ => false)
         | SOME _ => false
    end;

(* tgt edge has at least half unmapped (it is unmapped or half mapped) *)
fun tgt_edge_is_half_unmatched m te = 
    let val emap = get_emap m
    in case (EMap.lookup_codf emap (Ehn.mk_start te))
        of NONE => true
         | SOME _ =>
           case (EMap.lookup_codf emap (Ehn.mk_end te))
            of NONE => true
             | SOME _ => false
    end;
val tgt_edge_is_fully_matched = not oo tgt_edge_is_half_unmatched;

fun add_eh_match (peh, ped) (teh, ted) m = 
    (case MCtxt.edge_match ped ted (get_mctxt m)
      of NONE => NONE
       | SOME mctxt2 => 
         SOME (m |> update_emap (EMap.add peh teh)
                 |> set_mctxt mctxt2));

(* try to add pe -> te, where pe is an edge doing to a leaf, so it
   will only map half of a tgt edge. *)
fun try_add_leaf_ehn2edge_emap (peh,ped) (te,td) m = 
    let val emap = get_emap m
        val teh_start = Ehn.mk_start te
    in case EMap.lookup_codf emap teh_start
        of NONE => (* map to start of te *) 
           add_eh_match (peh,ped) (teh_start,td) m
         | SOME _ => 
           let val teh_end = Ehn.mk_end te in
             (case EMap.lookup_codf emap teh_end
               of NONE => (* map to end of te *)
                  add_eh_match (peh,ped) (teh_end,td) m
                (* tgt egde already fully mapped *)
                | SOME _ => NONE) end
    end;

exception emap_exp of string;

fun add_leaf_ehn2edge_emap ph t m = 
    case try_add_leaf_ehn2edge_emap ph t m 
     of NONE => raise emap_exp "add_leaf_edge_emap"
      | SOME x => x;

(* FIXME: don't use exceptions *)
fun add_full_leaf_ehn2ehn_emap (peh,ped) (teh,ted) m = 
    SOME (m |> update_emap (EMap.add peh teh 
                            o EMap.add (Edge.Ehn.other peh) 
                                       (Edge.Ehn.other teh))
            |> update_mctxt (the o MCtxt.edge_match ped ted))
    handle Option.Option => NONE
         | EMap.add_exp _ => NONE;

fun add_leaf_ehn2ehn_emap (peh,ped) (teh,ted) m = 
    let val emap = get_emap m
    in case EMap.lookup_codf emap teh
        of NONE => (* map to start of te *) 
           (case EMap.lookup_domf emap peh
             of NONE => 
                (case add_eh_match (peh,ped) (teh,ted) m 
                  of NONE => raise emap_exp "add_leaf_ehn2ehn_emap1"
                   | SOME m2 => m2)
              | SOME _ => raise emap_exp "add_leaf_ehn2ehn_emap2")
         | SOME _ => raise emap_exp "add_leaf_ehn2ehn_emap3"
    end;


(* try to add to the direct emap: pe = te; pe is not an leaf edge  *)
fun try_add_emap (pe,pd) (te,td) m = 
    if tgt_edge_is_unmapped m te then
      (case MCtxt.edge_match pd td (get_mctxt m)
        of NONE => NONE
         | SOME ctxt2 => 
           SOME (m |> (update_emap 
                         (EMap.add (Ehn.mk_start pe) (Ehn.mk_start te)
                          o EMap.add (Ehn.mk_end pe) (Ehn.mk_end te)))
                   |> set_mctxt ctxt2))
    else NONE;

(* tries to match new pattern edge with (first) one in the target set,
raises exception if target set is empty - we handle this give NONE *)
(* given:
     pes: a set of edges between two vertices in the pat, 
     tes: a set of edges between two vertices in the tgt 
   return: 
     option of matches using up all edges in pes, 
     tells us which left-over tes, and which are used. 
     (in future this might be a seq - if edge matching gave back a sequence)
*)
(* IMPROVE: avoid exception handling for failure - use fold_opt *)
(* assumes edge matching order is not relevant/matching makes it
insensitive: I think this is true for AC data matching *)
(* for non-leaf edges/left over leaf edges pes = non-loop tes *)
fun multi_edge_match pes tes m = 
    let 
      fun match_more (pe1,ped) (* patten edge name and data *)
                     (unmtes2, 
                      (mtes2, (* matched tgt edges, unmatched ones *)
                      m2)) (* match so far *)
        = let (* pull and match pat edge against next target edge  *)
            val ((te1,ted),unmtes3) = the (Edge.NTab.pull_elem unmtes2)
            (* match edge data *)
            val mctxt2 = the (MCtxt.edge_match ped ted (get_mctxt m2))
            (* update match context, and add non-loopy edge map *)
            val m3 = m2 |> (the o try_add_emap (pe1, ped) (te1, ted))
                        |> set_mctxt mctxt2
            (* te1 is now matched *)
            val mtes3 = Edge.NSet.add te1 mtes2
          in (unmtes3, (mtes3, m3)) end;
      val (tes_result, m2) = 
          Edge.NTab.fold match_more pes (tes, (Edge.NSet.empty, m))
    in Seq.single (tes_result, m2)
    end handle Option.Option => Seq.empty;

fun strict_multi_edge_match pes tes m = 
    Seq.filter 
      (fn (umn, x) => Edge.NTab.is_name_empty umn)
      (multi_edge_match pes tes m);

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
                             (BVertex.NTab.list_of (get_invmap m)))]]
         @ [EMap.pretty (get_emap m)]
      @ [Pretty.block [Pretty.str "MCtxt: ", MCtxt.pretty (get_mctxt m)]])
    ]);

val print = Pretty.writeln o pretty;

end; (* Match structure *)

structure DB_Match = MatchFun(AngleMatchCtxt);
structure Match : MATCH = DB_Match;
