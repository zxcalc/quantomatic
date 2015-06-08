structure RG_SimpUtil =
struct

structure SU = SimpUtil(
  structure Theory = RG_Theory
  structure IO = RG_ComponentDataIO)
open SU

fun is_red g v = case RG_Theory.Graph.get_vertex_data g v
                   of (RG_Theory.Graph.NVert (RG_Data.Xnd _)) => true | _ => false

fun is_green g v = case RG_Theory.Graph.get_vertex_data g v
                     of (RG_Theory.Graph.NVert (RG_Data.Znd _)) => true | _ => false

fun is_boundary_red g v = (RG_Theory.Graph.is_boundary g v) andalso V.NSet.exists (is_red g) (RG_Theory.Graph.get_adj_vertices g v)

fun num_boundary_red g = let
  val gmin = RG_Theory.Graph.minimise g
in V.NSet.cardinality (V.NSet.filter (is_boundary_red gmin) (RG_Theory.Graph.get_vertices gmin))
end

fun is_interior_green g v =
  (is_green g v) andalso
  V.NSet.forall (not o (RG_Theory.Graph.is_boundary g))
                (RG_Theory.Graph.get_adj_vertices g v)

fun min_green_arity g = let
  fun min v (SOME a) = SOME (Int.min (Arity.get_undir (RG_Theory.Graph.get_arity g v), a))
    | min v NONE = SOME (Arity.get_undir (RG_Theory.Graph.get_arity g v))
in
  case V.NSet.fold min (V.NSet.filter (is_interior_green g) (RG_Theory.Graph.get_vertices g)) NONE
    of SOME a => a
     | NONE => 0
end

val register_simproc = JsonControllerRegistry.RG_Controller.register_simproc

end
