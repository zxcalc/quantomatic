(**
 * A mapping h : G --> H between graphs that witnesses that H is obtained from G
 * by contracting wires (i.e. collapsing extra edge points).
 *
 * This is encoded by treating graphs as 1D simplicial sets (which are pretty much
 * the same thing). The difference is that vertices have a dual personality, both
 * as vertices and as "degenerate" edges, i.e. edges of length 0. This allows us
 * to represent with simplicial maps the process of "squashing" an edge into a
 * vertex.
 *
 * A wire contraction consists of a bijection of bboxes, along with a
 * a simplicial map that:
 *  1. is surjective
 *  2. preserves vertex/edge data
 *  3. preserves bbox membership
 *  4. has the property that for all vertices v in H, h^-1(v) is contractible
 *     (i.e. it forms a single, connected chain).
 * 
 *)

(* underlying function for a simplicial mapping *)
structure SiMap = NameFunction(structure Dom = VE; structure Cod = VE)

signature WIRE_CONTRACTION =
sig
  exception bad_wire_contraction_exp of string
  structure Graph : BANG_GRAPH
  type T
  val empty  : T
  val mk     : Graph.T * Graph.T * SiMap.T * BSub.T -> T
  val mk_opt : Graph.T * Graph.T * SiMap.T * BSub.T -> T option
end

functor WireContraction(Graph : BANG_GRAPH) : WIRE_CONTRACTION =
struct
  exception bad_wire_contraction_exp of string
  structure Graph = Graph
  datatype T = WireCtr of {
    dom   : Graph.T,
    cod   : Graph.T,
    simap : SiMap.T,
    bmap  : BSub.T
  }

  (* functions used in contraction check *)
  fun face1 g ve = case VE.dest ve of VERTEX _ => ve | EDGE e => VE.mk (VERTEX (Graph.get_edge_source g e))
  fun face2 g ve = case VE.dest ve of VERTEX _ => ve | EDGE e => VE.mk (VERTEX (Graph.get_edge_target g e))
  fun split_set ve_set = VE.NSet.fold (fn ve => fn (vs,es) => (
    case VE.dest ve of VERTEX v => (V.NSet.add v vs,es) | EDGE e => (vs, E.NSet.add e es)
  )) ve_set (V.NSet.empty, E.NSet.empty)

  (* returns true of the subgraph is a connected tree. in practise, this will only be chains because of
   * the type constraints
   *
   * NOTE: This could go in Graph, if it is useful elsewhere. 
   *)
  exception not_tree_exp of unit
  fun is_subgraph_contactible g (vs,es) =
  if V.NSet.is_singleton vs
  then true (* short-circuit tree traversal for this common case *)
  else
    let
      fun tree_traverse sched (found_vs, found_es) =
        case V.NSet.get_local_bot sched
          of SOME v =>
              let
                val found_vs' = V.NSet.add v found_vs
                val (sched',found_es') = E.NSet.fold (fn e => fn (sch,fnd) => (
                  if E.NSet.contains es e
                  then let
                         val v1 = Graph.edge_get_other_vertex g e v
                       in
                         if V.NSet.contains found_vs v then raise not_tree_exp ()
                         else (V.NSet.add v1 sch, E.NSet.add e fnd)
                       end
                  else (sch,fnd)
                ))
                (E.NSet.subtract (Graph.get_adj_edges g v) (found_es))
                (V.NSet.delete v sched, found_es)
              in
                tree_traverse sched' (found_vs',found_es')
              end
           | NONE => (found_vs,found_es)
      val (vs',es') = (case V.NSet.get_local_bot vs
                         of SOME v => tree_traverse (V.NSet.single v)
                          | NONE => I)
                      (V.NSet.empty,E.NSet.empty)
    in
      (V.NSet.eq vs vs') andalso (E.NSet.eq es es')
    end
    handle not_tree_exp () => false



  fun mk (g, h, simap, bmap) = let
    val (dom_vs,dom_es) = split_set (SiMap.get_dom_set simap)
    val () = if V.NSet.eq (dom_vs) (Graph.get_vertices g) andalso
                E.NSet.eq (dom_es) (Graph.get_edges g)
             then ()
             else raise bad_wire_contraction_exp "simap not total"
    val (cod_vs,cod_es) = split_set (SiMap.get_cod_set simap)
    
    val () = if V.NSet.eq (cod_vs) (Graph.get_vertices h) andalso
                E.NSet.eq (cod_es) (Graph.get_edges h)
             then ()
             else raise bad_wire_contraction_exp "simap not surjective"

    val () = if B.NSet.eq (BSub.get_dom_set bmap) (Graph.get_bboxes g) andalso
                B.NSet.eq (BSub.get_cod_set bmap) (Graph.get_bboxes h)
             then ()
             else raise bad_wire_contraction_exp "bmap not a bijection of bboxes"
    
    fun check_data_and_degen ve =
      case (VE.dest ve, VE.dest (SiMap.get simap ve))
        of (VERTEX v1, VERTEX v2) => Graph.vdata_eq (Graph.get_vertex_data g v1, Graph.get_vertex_data h v2)
         | (EDGE e1, EDGE e2)     => Graph.edata_eq (Graph.get_edge_data g e1, Graph.get_edge_data h e2)
         | (EDGE _, VERTEX v)     => Graph.is_wire_vertex h v
         | (VERTEX _, EDGE _)     => false
    fun check_bboxes ve =
      case (VE.dest ve, VE.dest (SiMap.get simap ve))
        of (VERTEX v1, VERTEX v2) =>
             B.NSet.eq (BSub.img_of_set bmap (Graph.get_bboxes_containing_vertex g v1))
                       (Graph.get_bboxes_containing_vertex h v2)
         | _ => true
    fun check_face1 ve = VE.name_eq (face1 g ve, face1 h (SiMap.get simap ve))
    fun check_face2 ve = VE.name_eq (face2 g ve, face2 h (SiMap.get simap ve))

    val () = if VE.NSet.forall
                (fn ve => check_data_and_degen ve andalso
                          check_bboxes ve andalso
                          check_face1 ve andalso
                          check_face2 ve)
                (SiMap.get_dom_set simap)
             then ()
             else raise bad_wire_contraction_exp "simap not a simplicial map respecting data and bboxes"
    
    val () = if VE.NSet.forall
                (fn ve => is_subgraph_contactible g (split_set (SiMap.inv_img simap ve)))
                (SiMap.get_cod_set simap)
             then ()
             else raise bad_wire_contraction_exp "simap not a contraction"


  in
    WireCtr { dom = g, cod = h, simap = simap, bmap = bmap }
  end

  fun mk_opt prms = SOME (mk prms) handle bad_wire_contraction_exp _ => NONE

  val empty = mk (Graph.empty, Graph.empty, SiMap.empty, BSub.empty)
end