(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*   Matching between two graphs without bang boxes but with boundary      *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature MATCH_SHARING 
= sig
  type T
  structure G : BANG_GRAPH_SHARING
  structure MCtxt : MATCH_CTXT_SHARING
  structure EMap : NAME_ISO_SHARING
  structure BBMatch : BBOX_MATCH_SHARING

  sharing MCtxt.G = G
  sharing BBMatch.G = G
  sharing EMap.Cod = G.Edge.Ehn
  sharing EMap.Dom = G.Edge.Ehn
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature MATCH 
= sig
  structure MCtxt : MATCH_CTXT
  structure G : BANG_GRAPH
  structure EMap : NAME_AMORPH
  structure BBMatch : BBOX_MATCH

  type T (* the type of matches *)

  (* sharing via sharing structure *)
  structure Sharing : MATCH_SHARING
  sharing type Sharing.T = T
  sharing Sharing.BBMatch = BBMatch.Sharing
  sharing Sharing.EMap = EMap.Sharing
  sharing Sharing.G = G.SharingBangGraph
  sharing Sharing.MCtxt = MCtxt.Sharing

  (* init = empty, but with pat and tgt's exterior as interface *)
  val init : (G.Vertex.NSet.T * G.Edge.NSet.T 
               * G.BBox.NSet.T) (* other names to avoid when matching *)
             -> G.T -> G.T -> T 
  val mk_id : G.T (* given a graph *)
              -> T (* make the id match for it *)

  (* basic gets *)
  val get_emap : T -> EMap.T
  val get_viso : T -> G.Vertex.Amorph.T
  val get_bvmap : T -> G.Vertex.Map.T
  val get_mctxt : T -> MCtxt.T
  val get_bbmatch : T -> BBMatch.T
  val get_names : T -> (G.Vertex.NSet.T * G.Edge.NSet.T * G.BBox.NSet.T)
  val set_names : (G.Vertex.NSet.T * G.Edge.NSet.T * G.BBox.NSet.T) -> T -> T 

  (* more bbox stuff *)
  val get_bbmap : T -> BBMatch.BBMap.T
  val add_bbox_drop : G.BBox.name -> T -> T;
  val add_bbox_kills : G.BBox.name list -> T -> T;
  val add_bbox_map : G.BBox.name -> G.BBox.name -> T -> T;
  val add_bbox_copy : (G.BBox.name (* old bbname *)
                 * (G.OVertex.name G.OVertex.NTab.T (* vertex copying *)
                      * G.Edge.name G.Edge.NTab.T) (* edge copying *)
                 * G.BBox.name) (* new bbox name *) 
                -> T -> T;

  (* opposites *)
  val tgt_edge_has_unmatched_half : T -> G.Edge.name -> bool
  val tgt_edge_is_fully_matched : T -> G.Edge.name -> bool

  (* *)
  val tgt_edgehalf_is_unmapped : T -> G.Edge.Ehn.name -> bool
  val tgt_vertex_is_matched : T -> G.OVertex.name -> bool

  (* for incremental matching of edges *)
  (* raise option exception on failure *)
  val match_another_halfedge : G.Edge.Ehn.name * G.Edge.data (* pattern hedge *) 
      -> (G.Edge.data G.Edge.Ehn.NTab.T (* left over unmatched *)
          * (G.Edge.NSet.T (* matches edges *)
             * T)) 
      -> (G.Edge.data G.Edge.Ehn.NTab.T (* left over unmatched *)
          * (G.Edge.NSet.T (* matches edges *)
             * T)) 
  val match_another_fulledge : G.Edge.name * G.Edge.data (* pattern edge *)
      -> (G.Edge.data G.Edge.Ehn.NTab.T (* left over unmatched *)
          * (G.Edge.NSet.T (* matches edges *)
             * T)) 
      -> (G.Edge.data G.Edge.Ehn.NTab.T (* left over unmatched *)
          * (G.Edge.NSet.T (* matches edges *)
             * T))

  (* match many edges *)
  val multi_edge_match : 
         (G.dir_or_undir * G.Edge.data) G.Edge.NTab.T  (* pattern edges *)
      -> (G.dir_or_undir * G.Edge.data) G.Edge.Ehn.NTab.T (* target half-edges *)
      -> T 
      -> ((G.dir_or_undir * G.Edge.data) G.Edge.Ehn.NTab.T (* left over unmatched *)
          * (G.Edge.NSet.T (* matches edges *)
             * T)) Seq.seq

  val match_vertex : G.OVertex.name * G.OVertex.data
                     -> G.OVertex.name * G.OVertex.data 
                     -> T -> T option

  (* match pattern boundary point to target vertex/edge-point *)
  val match_boundary : G.OVertex.name -> G.OVertex.name -> T -> T

  (* FIXME: change to seq/option types, respect failure! don't use exeptions *)
  (* adds given half to a free half of edge *)
  val add_leaf_ehn2edge_emap 
       : G.Edge.Ehn.name * (G.dir_or_undir * G.Edge.data) (* pat *)
      -> G.Edge.name * (G.dir_or_undir * G.Edge.data) (* tgt *)
      -> T -> T
  (* only adds this half *)
  val add_leaf_ehn2ehn_emap : G.Edge.Ehn.name * (G.dir_or_undir * G.Edge.data) 
                              -> G.Edge.Ehn.name * (G.dir_or_undir * G.Edge.data) 
                              -> T -> T

  (* adds both halfs as matching *)
  val add_full_leaf_ehn2ehn_emap : G.Edge.Ehn.name * G.Edge.data 
                              -> G.Edge.Ehn.name * G.Edge.data 
                              -> T -> T option 

  val pretty : T -> Pretty.T
  val print : T -> unit
  val set_mctxt : MCtxt.T -> T -> T


(* 
  val update_emap : (EMap.T -> EMap.T) -> T -> T
  val update_viso : (V.name V.NTab.T ->
                       V.name V.NTab.T) -> T -> T
  val update_mctxt : (MCtxt.T -> MCtxt.T) -> T -> T
  val update_bvmap : (V.name V.NTab.T ->
                     V.name V.NTab.T) -> T -> T
*)
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor MatchFun(
  structure MCtxt : MATCH_CTXT
  structure G : BANG_GRAPH
  sharing G.SharingBangGraph = MCtxt.Sharing.G
) 
 : MATCH 
= struct

structure MCtxt = MCtxt;

structure G = G;
structure V = G.OVertex;
structure E = G.Edge;

structure BBox = G.BBox;
structure BBMatch = BBoxMatchFun(G);
structure BBMap = BBMatch.BBMap;

structure Ehn = E.Ehn;
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
         {
          (* names context for fresh names when copying bboxes in pat *)
          names : (V.NSet.T * E.NSet.T * BBox.NSet.T),
          (* Internal vertex bijection from pat to tgt *)
          viso : V.Amorph.T,
          (* map for boundary vertices pat to tgt vertices  *)
          bvmap: V.Map.T,
          (* 1-1, inj half-edge mapping *)
          emap: EMap.T,
          (* bang box expansion of pattern *)
          bbmatch : BBMatch.T,
          (* instantiations for vertex/edge data varaiables *)
          mctxt: MCtxt.T
         };

fun init (vs,es,bs) pat tgt = 
  Match {
    names = (V.NSet.union_merge (G.get_vnames pat) 
                (V.NSet.union_merge (G.get_vnames tgt) vs),  
             E.NSet.union_merge (G.get_enames pat)
                (E.NSet.union_merge (G.get_enames tgt) es), 
             BBox.NSet.union_merge (G.get_bbnames pat) 
                (BBox.NSet.union_merge (G.get_bbnames tgt) bs)),
    viso = V.Amorph.empty,
    bvmap = V.Map.empty,
    emap = EMap.empty,
    bbmatch = BBMatch.init_from_pat pat,
    mctxt = MCtxt.empty
  };

(* make identity match from bvnames, vnames, enames;
   vnames is all vnames, bvnames are the boundary subset of them, 
   enames is all enames. 
*)
fun mk_id g = 
    let 
      val vnames = G.get_vnames g;
      val enames = G.get_enames g;
      val bbnames = G.get_bbnames g; 
      val boundary_vnames = G.get_boundary g;
      val bbmatch = 
          BBMatch.mk_id boundary_vnames
                        (G.VtoBoxMap.get_codset (G.get_bboxes g));
      val bvmap = (* mk id *)
          V.NSet.fold (fn n => V.Map.add_to_dom n n) 
                      boundary_vnames V.Map.empty;
      val emap = (* id never has leaf-edge mapped to loops, so direct map *)
          E.NSet.fold 
            (* tgt mapping not needed; we always start by mapping src
               - helps keep table small *)
            (fn n => EMap.add (Ehn.mk_start n) (Ehn.mk_start n)
                     o EMap.add (Ehn.mk_end n) (Ehn.mk_end n))
            enames EMap.empty;
      val viso = 
          V.NSet.fold (fn n => V.Amorph.add n n) 
                      (V.NSet.subtract vnames boundary_vnames) 
                      V.Amorph.empty;
    in Match {names = (vnames,enames,G.get_bbnames g),
              bvmap = bvmap, 
              viso = viso, 
              emap = emap, 
              bbmatch = bbmatch,
              mctxt = MCtxt.empty}
    end;

(* updates *)
fun update_names f (Match rep) = 
    Match {names = f (#names rep),
           bvmap = #bvmap rep, 
           emap = #emap rep,
           viso = #viso rep,
           bbmatch = #bbmatch rep,
           mctxt = #mctxt rep};
fun update_bvmap f (Match rep) = 
    Match {names = #names rep,
           bvmap = f (#bvmap rep), 
           emap = #emap rep,
           viso = #viso rep,
           bbmatch = #bbmatch rep,
           mctxt = #mctxt rep};
fun update_emap f (Match rep) = 
    Match {names = #names rep,
           bvmap = #bvmap rep, 
           emap = f (#emap rep),
           viso = #viso rep,
           bbmatch = #bbmatch rep,
           mctxt = #mctxt rep};
fun update_viso f (Match rep) = 
    Match {names = #names rep,
           bvmap = #bvmap rep, 
           emap = #emap rep,
           viso = f (#viso rep),
           bbmatch = #bbmatch rep,
           mctxt = #mctxt rep};
fun update_mctxt f (Match rep) = 
    Match {names = #names rep,
           bvmap = #bvmap rep, 
           emap = #emap rep,
           viso = #viso rep,
           bbmatch = #bbmatch rep,
           mctxt = f (#mctxt rep)};
fun update_bbmatch f (Match rep) = 
    Match {names = #names rep,
           bvmap = #bvmap rep, 
           emap = #emap rep,
           viso = #viso rep,
           bbmatch = f (#bbmatch rep),
           mctxt = #mctxt rep};

fun get_bvmap (Match rep) = #bvmap rep;
fun get_emap (Match rep) = #emap rep;
fun get_viso (Match rep) = #viso rep;
fun get_mctxt (Match rep) = #mctxt rep;
val set_mctxt = update_mctxt o K;
fun get_names (Match rep) = #names rep;
val set_names = update_names o K;

fun get_bbmatch (Match rep) = #bbmatch rep;
val get_bbmap = BBMatch.get_bbmap o get_bbmatch;

val add_bbox_drop = update_bbmatch o BBMatch.add_drop;
val add_bbox_kills = update_bbmatch o BBMatch.add_kills;
val add_bbox_map = update_bbmatch oo BBMatch.add_bbmap;
val add_bbox_copy = update_bbmatch o BBMatch.add_copy;


(* implicit: m n *)
(* tgt is matched by some non-leaf vertex in pat *)
val tgt_vertex_is_matched = V.Amorph.dom_contains o get_viso;

(* given tgt edge is completely unmapped: both start and end are unmatched *)
fun tgt_edgehalf_is_unmapped m eh = 
    is_none (EMap.lookup_codf (get_emap m) eh);

fun tgt_edge_is_unmapped m e = 
    let val emap = get_emap m
    in case EMap.lookup_codf emap (Ehn.mk_start e) 
        of NONE => (case EMap.lookup_codf emap (Ehn.mk_end e) 
                     of NONE => true
                      | SOME _ => false)
         | SOME _ => false
    end;

(* tgt edge has at least half unmapped (it is unmapped or half mapped) *)
fun tgt_edge_has_unmatched_half m te = 
    let val emap = get_emap m
    in case (EMap.lookup_codf emap (Ehn.mk_start te))
        of NONE => true
         | SOME _ =>
           case (EMap.lookup_codf emap (Ehn.mk_end te))
            of NONE => true
             | SOME _ => false
    end;
val tgt_edge_is_fully_matched = not oo tgt_edge_has_unmatched_half;

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

(* IMPROVE: don't use exceptions? *)
fun add_full_leaf_ehn2ehn_emap (peh,ped) (teh,ted) m = 
    SOME (m |> update_emap (EMap.add peh teh 
                            o EMap.add (E.Ehn.other peh) 
                                       (E.Ehn.other teh))
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
fun try_add_emap (pe1,ped) (te1,ted) m = 
    if tgt_edge_is_unmapped m te1 then
      (case MCtxt.edge_match ped ted (get_mctxt m)
        of NONE => NONE
         | SOME ctxt2 => 
           SOME (m |> (update_emap 
                         (EMap.add (Ehn.mk_start pe1) (Ehn.mk_start te1)
                          o EMap.add (Ehn.mk_end pe1) (Ehn.mk_end te1)))
                   |> set_mctxt ctxt2))
    else NONE;



fun match_another_halfedge
      (peh,ped) (* patten edge name and data *)
      (unmtes2, (* unmatched edges *)
        (mtes2,m2)) (* matched set of enames, match so far *)
    = let (* pull and match pat edge against next target edge. For now we treat all vertices as being symmetric on edge; IMPROVE: should be a parameter of vertex data? *)
        val ((teh1,ted),unmtes3) = the (E.Ehn.NTab.pull_elem unmtes2);
        val te1 = E.Ehn.get_ename teh1;
        (* match edge data *)
        val mctxt2 = the (MCtxt.edge_match ped ted (get_mctxt m2))
        (* update match context, and add non-loopy edge map *)
        val m3 = m2 |> the o (add_eh_match (peh, ped) (teh1, ted))
                    |> set_mctxt mctxt2;
        (* te1 is now matched *)
        val mtes3 = if tgt_edge_has_unmatched_half m3 te1 then mtes2 
                    else E.NSet.add te1 mtes2;
      in (unmtes3, (mtes3,m3)) end;

fun match_another_fulledge
      (pe1,ped) (* patten edge name and data *)
      (unmtes2, (* unmatched edges *)
        (mtes2,m2)) (* matched set of enames, match so far *)
    = let (* pull and match pat edge against next target edge  *)
        val ((teh1,ted),unmtes3) = the (E.Ehn.NTab.pull_elem unmtes2);
        val te1 = E.Ehn.get_ename teh1;
        (* match edge data *)
        val mctxt2 = the (MCtxt.edge_match ped ted (get_mctxt m2))
        (* update match context, and add non-loopy edge map *)
        val m3 = m2 |> (the o try_add_emap (pe1, ped) (te1, ted))
                    |> set_mctxt mctxt2;
        (* te1 is now matched *)
        val mtes3 = E.NSet.add te1 mtes2;
      in (unmtes3, (mtes3,m3)) end;


(* assumes that only one of p_from or p_to is a leaf vertex *)
fun multi_edge_match pes tes m = 
    ((tes, (* unmatched tgt edges *) 
      (E.NSet.empty,m)) (* matched enames so far and the match to update *)
         |> E.NTab.fold match_another_fulledge pes
         |> Seq.single)
    handle Option.Option => Seq.empty; (* ran out of target egdes *)


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
fun multi_fulledge_match pes tes m = 
    let 
      fun match_more (pe1,ped) (* patten edge name and data *)
                     (unmtes2, (* unmatched tgt edges *)
                      (mtes2, (* matched tgt edges *)
                      m2)) (* match so far *)
        = let (* pull and match pat edge against next target edge  *)
            val ((te1,ted),unmtes3) = the (E.NTab.pull_elem unmtes2);
            (* match edge data *)
            val mctxt2 = the (MCtxt.edge_match ped ted (get_mctxt m2))
            (* update match context, and add non-loopy edge map *)
            val m3 = m2 |> (the o try_add_emap (pe1, ped) (te1, ted))
                        |> set_mctxt mctxt2;
            (* te1 is now matched *)
            val mtes3 = E.NSet.add te1 mtes2;
          in (unmtes3, (mtes3, m3)) end;
      val (tes_result, m2) = 
          E.NTab.fold match_more pes (tes, (E.NSet.empty, m))
    in Seq.single (tes_result, m2)
    end handle Option.Option => Seq.empty;

(* fun strict_multi_edge_match pes tes m = 
    Seq.filter 
      (fn (umn, x) => E.NTab.is_name_empty umn)
      (multi_edge_match pes tes m);
*)



(* try match non-leaf pat vertex to tgt-vertex, using internal data matching
   function from Mctxt *)
fun match_vertex (n,V.IVert _) (tn,V.EVert) m = NONE
  | match_vertex (n,V.IVert d) (tn,V.IVert td) m = 
    (* map must be injective for internal pat vertices *)
    if not(tgt_vertex_is_matched m tn) then 
      case MCtxt.vertex_idata_match d td (get_mctxt m)
       of NONE => NONE (* vertices don't match *)
        | SOME mctxt2 => 
          SOME (m |> set_mctxt mctxt2
                  |> update_viso (V.Amorph.add tn n))
    else NONE (* matched by some other pat vertex *)
  | match_vertex (n,V.EVert) (tn,td) m = (* edge-point mapped to vertex *)
    SOME (m |> update_bvmap (V.Map.add_to_dom n tn));

(* pn is asumed to be a boundary point. *)
fun match_boundary pn tn m = 
    m |> update_bvmap (V.Map.add_to_dom pn tn);


(* pretty printing *)
fun pretty_names (vs,es,bs) = 
  Pretty.block 
    [Pretty.str "Vs: ", V.NSet.pretty vs,
     Pretty.str "; Es: ", E.NSet.pretty es,
     Pretty.str "; Bs: ", BBox.NSet.pretty bs];

fun pretty m = 
    Pretty.chunks 
    ([Pretty.str "Match: ",
      (Pretty.indent 2 o Pretty.chunks)
      ([pretty_names (get_names m)]
       @ [Pretty.block 
           [Pretty.str "Boundary Map: ",
            V.Map.pretty (get_bvmap m)]]
       @ [BBMatch.pretty (get_bbmatch m)]
       @ [Pretty.block 
           [Pretty.str "Internal Vertex Amorph: ",
            V.Amorph.pretty (get_viso m)]]
        @ [EMap.pretty (get_emap m)]
        @ [Pretty.block [Pretty.str "MCtxt: ", MCtxt.pretty (get_mctxt m)]])
    ]);

val print = Pretty.writeln o pretty;


structure Sharing : MATCH_SHARING 
= struct
  structure G = G.SharingBangGraph
  structure MCtxt = MCtxt.Sharing
  structure BBMatch = BBMatch.Sharing
  structure EMap = EMap.Sharing
  type T = T;
end;

end; (* Match structure *)

(* 
structure DB_Match = MatchFun(
  structure MCtxt = AngleMatchCtxt 
  structure G = RGGraph);

structure Match : MATCH = DB_Match;
*)
