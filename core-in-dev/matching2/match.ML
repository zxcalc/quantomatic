(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*   Matching between two graphs without bang boxes but with boundary      *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature MATCH_SHARING 
= sig
  type T
  structure G : BANG_GRAPH_SHARING
  structure EMap : NAME_ISO_SHARING
  structure BBMatch : BBOX_MATCH_SHARING

  sharing BBMatch.G = G
  sharing EMap.Cod = G.Edge.Ehn
  sharing EMap.Dom = G.Edge.Ehn
end;

signature MATCH 
= sig
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

  (* init = empty, but with pat and tgt's exterior as interface *)
  val init : (G.Vertex.NSet.T * G.Edge.NSet.T 
               * G.BBox.NSet.T) (* other names to avoid when matching *)
             -> G.T -> G.T -> T 

  (* basic gets *)
  val get_emap : T -> EMap.T
  val get_viso : T -> G.Vertex.Amorph.T
  val get_bvmap : T -> G.Vertex.Map.T
  val get_bbmatch : T -> BBMatch.T
  val get_names : T -> (G.Vertex.NSet.T * G.Edge.NSet.T * G.BBox.NSet.T)
  val get_unif : T -> G.Param.unif
  val set_names : (G.Vertex.NSet.T * G.Edge.NSet.T * G.BBox.NSet.T) -> T -> T 
  
  exception match_exp of unit;
    (* basic matching functions; throw match_exp () if fail *)
  val match_edgehalf : G.Edge.Ehn.name * G.Edge.data 
      -> G.Edge.Ehn.name * G.Edge.data 
      -> T -> T

  val match_edge : bool (* matched in reverse direction *) 
      -> G.Edge.name * G.Edge.data 
      -> G.Edge.name * G.Edge.data 
      -> T -> T

  (*  *)
  val match_vertex : G.OVertex.name * G.OVertex.data
                     -> G.OVertex.name * G.OVertex.data 
                     -> T -> T

  (* match pattern boundary point to target vertex/edge-point *)
  val match_boundary : G.OVertex.name -> G.OVertex.name -> T -> T

  
  (* updates  *)
  val update_names : (   G.Vertex.NSet.T * G.Edge.NSet.T * G.BBox.NSet.T
                      -> G.Vertex.NSet.T * G.Edge.NSet.T * G.BBox.NSet.T)
                     -> T -> T
  val update_bvmap : (G.Vertex.Map.T -> G.Vertex.Map.T) -> T -> T
  val update_emap : (EMap.T -> EMap.T) -> T -> T

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
  
  val pretty : T -> Pretty.T
  val print : T -> unit
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor MatchFun(
  structure G : BANG_GRAPH
) 
 : MATCH 
= struct

structure G = G;
structure V = G.OVertex;
structure E = G.Edge;

structure BBox = G.BBox;
structure BBMatch = BBoxMatchFun(G);
structure BBMap = BBMatch.BBMap;

structure Ehn = E.Ehn;
structure EMap = NameAmorphFun(Ehn);

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
          (* data unification *)
          unif: G.Param.unif
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
    unif = G.Param.empty_unif
  };

(* updates *)
fun update_names f (Match rep) = 
    Match {names = f (#names rep),
           bvmap = #bvmap rep, 
           emap = #emap rep,
           viso = #viso rep,
           bbmatch = #bbmatch rep,
           unif = #unif rep};
fun update_bvmap f (Match rep) = 
    Match {names = #names rep,
           bvmap = f (#bvmap rep), 
           emap = #emap rep,
           viso = #viso rep,
           bbmatch = #bbmatch rep,
           unif = #unif rep};
fun update_emap f (Match rep) = 
    Match {names = #names rep,
           bvmap = #bvmap rep, 
           emap = f (#emap rep),
           viso = #viso rep,
           bbmatch = #bbmatch rep,
           unif = #unif rep};
fun update_viso f (Match rep) = 
    Match {names = #names rep,
           bvmap = #bvmap rep, 
           emap = #emap rep,
           viso = f (#viso rep),
           bbmatch = #bbmatch rep,
           unif = #unif rep};
fun update_bbmatch f (Match rep) = 
    Match {names = #names rep,
           bvmap = #bvmap rep, 
           emap = #emap rep,
           viso = #viso rep,
           bbmatch = f (#bbmatch rep),
           unif = #unif rep};
fun update_unif f (Match rep) = 
    Match {names = #names rep,
           bvmap = #bvmap rep, 
           emap = #emap rep,
           viso = #viso rep,
           bbmatch = #bbmatch rep,
           unif = f (#unif rep)};

fun get_bvmap (Match rep) = #bvmap rep;
fun get_emap (Match rep) = #emap rep;
fun get_viso (Match rep) = #viso rep;
fun get_names (Match rep) = #names rep;
fun get_unif (Match rep) = #unif rep;
val set_names = update_names o K;

fun get_bbmatch (Match rep) = #bbmatch rep;
val get_bbmap = BBMatch.get_bbmap o get_bbmatch;

val add_bbox_drop = update_bbmatch o BBMatch.add_drop;
val add_bbox_kills = update_bbmatch o BBMatch.add_kills;
val add_bbox_map = update_bbmatch oo BBMatch.add_bbmap;
val add_bbox_copy = update_bbmatch o BBMatch.add_copy;

(* matching *)
exception match_exp of unit;

fun match_edgehalf (peh, ped) (teh, ted) m =
                (m |> update_emap (EMap.add peh teh)
                   |> update_unif (the o G.Param.unif_edge ped ted)
                )handle EMap.add_exp (_,_,_) => raise match_exp ()
                      | Option               => raise match_exp ();
                      
fun match_edge rev (pe, ped) (te, ted) m = let 
                val (peh1,peh2) = if rev then (Ehn.mk_end pe, Ehn.mk_start pe)
                                  else (Ehn.mk_start pe, Ehn.mk_end pe)
             in (m |> update_emap (EMap.add peh2 (Ehn.mk_end te))
                   |> update_emap (EMap.add peh1 (Ehn.mk_start te))
                   |> update_unif (the o G.Param.unif_edge ped ted)
                )handle EMap.add_exp (_,_,_) => raise match_exp ()
                      | Option               => raise match_exp ()
             end;
             
fun match_vertex (pv,pvd) (tv,tvd) m =
                (m |> update_viso (V.Amorph.add pv tv)
                   |> update_unif (the o G.Param.unif_vertex pvd tvd)
                )handle V.Amorph.add_exp (_,_,_) => raise match_exp ()
                      | Option                   => raise match_exp ();
                      
fun match_boundary pb tb m = m |> update_bvmap (V.Map.add_to_dom pb tb);

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
        @ [EMap.pretty (get_emap m)])
    ]);

val print = Pretty.writeln o pretty;

structure Sharing : MATCH_SHARING 
= struct
  structure G = G.SharingBangGraph
  structure BBMatch = BBMatch.Sharing
  structure EMap = EMap.Sharing
  type T = T;
end;
  
end;