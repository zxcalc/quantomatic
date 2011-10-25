(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*   Matching between two graphs without bang boxes but with boundary      *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature MATCH_SHARING 
= sig
  type T
  structure G : BANG_GRAPH_SHARING
  structure MCtxt : MATCH_CTXT_SHARING
  structure EMap : NAME_INJ_SHARING
  structure BBMatch : BBOX_MATCH_SHARING

  sharing MCtxt.G = G
  sharing BBMatch.G = G
  sharing EMap.Cod = G.EData.Ehn
  sharing EMap.Dom = G.EData.Ehn
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature MATCH 
= sig
  structure MCtxt : MATCH_CTXT
  structure G : BANG_GRAPH
  structure EMap : NAME_INJENDO
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
  val init : (V.NSet.T * E.NSet.T 
               * B.NSet.T) (* other names to avoid when matching *)
             -> G.T -> G.T -> T 
  val mk_id : G.T (* given a graph *)
              -> T (* make the id match for it *)

  (* basic gets *)
  val get_emap : T -> EMap.T
  val get_viso : T -> VInjEndo.T
  val get_evmap : T -> VMap.T
  val get_mctxt : T -> MCtxt.T
  val get_bbmatch : T -> BBMatch.T
  val get_names : T -> (V.NSet.T * E.NSet.T * B.NSet.T)
  val set_names : (V.NSet.T * E.NSet.T * B.NSet.T) -> T -> T 

  (* more bbox stuff *)
  val get_bbmap : T -> BBMatch.BMap.T
  val add_bbox_drop : B.name -> T -> T;
  val add_bbox_kills : B.name list -> T -> T;
  val add_bbox_map : B.name -> B.name -> T -> T;
  val add_bbox_copy : (B.name (* old bbname *)
                 * (V.name V.NTab.T (* vertex copying *)
                      * E.name E.NTab.T) (* edge copying *)
                 * B.name) (* new bbox name *) 
                -> T -> T;

  (* basic matching functions *)
  val match_edgehalf : H.name * G.EData.data 
      -> H.name * G.EData.data 
      -> T -> T option

  val match_edge : bool (* matched in reverse direction *) 
      -> E.name * G.EData.data 
      -> E.name * G.EData.data 
      -> T -> T option

  (*  *)
  val match_vertex : V.name * G.OVData.data
                     -> V.name * G.OVData.data 
                     -> T -> T option

  (* match pattern boundary point to target vertex/edge-point *)
  val match_boundary : V.name -> V.name -> T -> T

  (* *)
  (* val tgt_edgehalf_is_matched *)

  val pretty : T -> Pretty.T
  val print : T -> unit
  val set_mctxt : MCtxt.T -> T -> T

(* 
  val update_emap : (EMap.T -> EMap.T) -> T -> T
  val update_viso : (V.name V.NTab.T ->
                       V.name V.NTab.T) -> T -> T
  val update_mctxt : (MCtxt.T -> MCtxt.T) -> T -> T
  val update_evmap : (V.name V.NTab.T ->
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
structure EMap = NameInjEndoFun(Ehn);


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
          names : (V.NSet.T * E.NSet.T * B.NSet.T),
          (* Internal vertex bijection from pat to tgt *)
          viso : V.InjEndo.T,
          (* map for boundary vertices pat to tgt vertices  *)
          evmap: V.Map.T,
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
             B.NSet.union_merge (G.get_bbnames pat) 
                (B.NSet.union_merge (G.get_bbnames tgt) bs)),
    viso = V.InjEndo.empty,
    evmap = V.Map.empty,
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
                        (G.VBMap.get_codset (G.get_bboxes g));
      val evmap = (* mk id *)
          V.NSet.fold (fn n => V.Map.add_to_dom n n) 
                      boundary_vnames V.Map.empty;
      val emap = (* id never has leaf-edge mapped to loops, so direct map *)
          E.NSet.fold 
            (* tgt mapping not needed; we always start by mapping src
               - helps keep table small *)
            (fn n => EMap.add (H.mk_start n) (H.mk_start n)
                     o EMap.add (H.mk_end n) (H.mk_end n))
            enames EMap.empty;
      val viso = 
          V.NSet.fold (fn n => V.InjEndo.add n n) 
                      (V.NSet.subtract vnames boundary_vnames) 
                      V.InjEndo.empty;
    in Match {names = (vnames,enames,G.get_bbnames g),
              evmap = evmap, 
              viso = viso, 
              emap = emap, 
              bbmatch = bbmatch,
              mctxt = MCtxt.empty}
    end;

(* updates *)
fun update_names f (Match rep) = 
    Match {names = f (#names rep),
           evmap = #evmap rep, 
           emap = #emap rep,
           viso = #viso rep,
           bbmatch = #bbmatch rep,
           mctxt = #mctxt rep};
fun update_evmap f (Match rep) = 
    Match {names = #names rep,
           evmap = f (#evmap rep), 
           emap = #emap rep,
           viso = #viso rep,
           bbmatch = #bbmatch rep,
           mctxt = #mctxt rep};
fun update_emap f (Match rep) = 
    Match {names = #names rep,
           evmap = #evmap rep, 
           emap = f (#emap rep),
           viso = #viso rep,
           bbmatch = #bbmatch rep,
           mctxt = #mctxt rep};
fun update_viso f (Match rep) = 
    Match {names = #names rep,
           evmap = #evmap rep, 
           emap = #emap rep,
           viso = f (#viso rep),
           bbmatch = #bbmatch rep,
           mctxt = #mctxt rep};
fun update_mctxt f (Match rep) = 
    Match {names = #names rep,
           evmap = #evmap rep, 
           emap = #emap rep,
           viso = #viso rep,
           bbmatch = #bbmatch rep,
           mctxt = f (#mctxt rep)};
fun update_bbmatch f (Match rep) = 
    Match {names = #names rep,
           evmap = #evmap rep, 
           emap = #emap rep,
           viso = #viso rep,
           bbmatch = f (#bbmatch rep),
           mctxt = #mctxt rep};

fun get_evmap (Match rep) = #evmap rep;
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
val tgt_vertex_is_matched = V.InjEndo.dom_contains o get_viso;

(* given tgt edge is completely unmapped: both start and end are unmatched *)
fun tgt_edgehalf_is_matched m eh = 
    is_some (EMap.lookup_codf (get_emap m) eh);
fun tgt_edge_is_matched m e =
    tgt_edgehalf_is_matched m (H.mk_start e) 
    orelse tgt_edgehalf_is_matched m (H.mk_end e); 
fun tgt_edge_has_unmatched_half m e = 
    not (tgt_edgehalf_is_matched m (H.mk_start e))
    orelse not (tgt_edgehalf_is_matched m (H.mk_end e)); 
fun tgt_edge_is_fully_matched m e = 
    tgt_edgehalf_is_matched m (H.mk_start e) 
    andalso tgt_edgehalf_is_matched m (H.mk_end e); 


(* assumes compatability with EMap (edge-half map) *)
fun match_edgehalf (peh, ped) (teh, ted) m = 
    (case MCtxt.edge_match ped ted (get_mctxt m)
      of NONE => NONE
       | SOME mctxt2 => 
         SOME (m |> update_emap (EMap.add peh teh)
                 |> set_mctxt mctxt2));

(* assumes compatability with EMap (edge-half map) *)
fun match_edge revdir (pe, ped) (te, ted) m = 
    (case MCtxt.edge_match ped ted (get_mctxt m)
      of NONE => NONE
       | SOME mctxt2 => 
         let val (peh1,peh2) = if revdir then (H.mk_end pe, H.mk_start pe)
                               else (H.mk_start pe, H.mk_end pe)
         in
           SOME (m |> update_emap (EMap.add peh2 (H.mk_end te))
                   |> update_emap (EMap.add peh1 (H.mk_start te))
                   |> set_mctxt mctxt2)
         end);

(* try match non-leaf pat vertex to tgt-vertex, using internal data matching
   function from Mctxt *)
fun match_vertex (n,V.IVert _) (tn,V.EVert) m = NONE
  | match_vertex (n,V.IVert d) (tn,V.IVert td) m = 
    (* map must be injective for internal pat vertices *)
    if tgt_vertex_is_matched m tn then NONE
    else (case MCtxt.vertex_idata_match d td (get_mctxt m)
       of NONE => NONE (* vertices don't match *)
        | SOME mctxt2 => 
          SOME (m |> set_mctxt mctxt2
                  |> update_viso (V.InjEndo.add tn n)))
      (* matched by some other pat vertex *)
  | match_vertex (n,V.EVert) (tn,td) m = (* edge-point mapped to vertex *)
    SOME (m |> update_evmap (V.Map.add_to_dom n tn));

(* pn is asumed to be a boundary point. *)
fun match_boundary pn tn m = 
    m |> update_evmap (V.Map.add_to_dom pn tn);


(* pretty printing *)
fun pretty_names (vs,es,bs) = 
  Pretty.block 
    [Pretty.str "Vs: ", V.NSet.pretty vs,
     Pretty.str "; Es: ", E.NSet.pretty es,
     Pretty.str "; Bs: ", B.NSet.pretty bs];

fun pretty m = 
    Pretty.chunks 
    ([Pretty.str "Match: ",
      (Pretty.indent 2 o Pretty.chunks)
      ([pretty_names (get_names m)]
       @ [Pretty.block 
           [Pretty.str "Boundary Map: ",
            V.Map.pretty (get_evmap m)]]
       @ [BBMatch.pretty (get_bbmatch m)]
       @ [Pretty.block 
           [Pretty.str "Internal Vertex InjEndo: ",
            V.InjEndo.pretty (get_viso m)]]
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
