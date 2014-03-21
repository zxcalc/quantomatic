(*PolyML.SaveState.loadState "../../heaps/quanto.heap";
PolyML.Compiler.printDepth:=100;*)

structure RG_Mathematica =
struct

structure G = RG_Theory.Graph

fun to_mathematica name graph = let
  val g = G.minimise graph
  val bnd = E.NSet.filter 
    (fn e => G.is_boundary g (G.get_edge_source g e) orelse
             G.is_boundary g (G.get_edge_target g e))
    (G.get_edges g)
  val interior = E.NSet.subtract (G.get_edges g) bnd
  val pretty_ang = LinratAngleExpr.pretty_math AlgFormat.MATHEMATICA
  fun pretty_int b = Pretty.str ("{" ^ E.dest b ^ ", 0, 1}")
  fun pretty_adj v =
    Pretty.list "{" "}"
    (map (Pretty.str o E.dest) (E.NSet.list_of (G.get_adj_edges g v)))
  fun pretty_nd v =
    case G.get_vertex_data g v 
      of G.NVert (RG_Data.Znd angle) =>
           Pretty.block[Pretty.str "z[", pretty_ang angle,
           Pretty.str ", ", pretty_adj v, Pretty.str "] * "]
       | G.NVert (RG_Data.Xnd angle) =>
           Pretty.block[Pretty.str "x[", pretty_ang angle,
           Pretty.str ", ", pretty_adj v, Pretty.str "] * "]
       | _ => Pretty.str ""
in
  Pretty.string_of (Pretty.block [
    Pretty.str (name ^ "["),
    Pretty.list "{" "}" (map (fn e => Pretty.str (E.dest e ^ "_")) (E.NSet.list_of bnd)),
    Pretty.str "] := ",
    Pretty.str "Sum[(",
    Pretty.block (map pretty_nd (V.NSet.list_of (G.get_node_vertices g))),
    Pretty.str "1), ",
    Pretty.list "{" "}" (map pretty_int (E.NSet.list_of interior)),
    Pretty.str "];"
  ])
end

end