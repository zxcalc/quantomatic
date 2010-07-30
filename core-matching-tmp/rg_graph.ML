signature RG_GRAPH_SHARING = 
sig
  include BANG_GRAPH_SHARING
end


signature RG_GRAPH = 
sig
include BANG_GRAPH 
structure RGVertex : RG_VERTEX
sharing BVertex.SharingWithIData = RGVertex.SharingWithIData
end;

(* IMPROVE provide table holding variable info for each vertices data *)

structure DB_RGGraph
= struct 
  structure RGVertex = RGVertex;
  structure BBGraph = BangGraphFun(
    structure BVertex = RGVertex  
    and Edge = UnitEdge
    type data = unit (* no extra inherrited data for graphs *)
    val empty_data = () )
  open BBGraph;
end;

structure RGGraph : RG_GRAPH = DB_RGGraph;
structure Vertex = RGGraph.RGVertex;
structure Edge = RGGraph.Edge;

structure V = RGGraph.BVertex;
structure E = RGGraph.Edge;
structure G = RGGraph;

structure MkG = 
struct
val bvert = V.BVert;
val zero_angle = AngleExpr.zero;
val var_angle = AngleExpr.mk_var;
fun var_iangle i v = AngleExpr.mk (0,1) [(v,i)];
val add_angles = AngleExpr.add_expr;
val pi_angle = AngleExpr.pi;
fun mkZ a = V.IVert (RGVertex.Znd a);
fun mkX a = V.IVert (RGVertex.Xnd a);
val mkH = V.IVert RGVertex.Hnd;
val edge = ();
end;


(* install pretty printers *)
local
    fun prettyVertex _ _ x =
      PolyML.PrettyString("\"" ^ Vertex.string_of_name x ^ "\"");
    fun prettyEdge _ _ x =
      PolyML.PrettyString("\"" ^ Edge.string_of_name x ^ "\"");
    fun prettyBBox _ _ x =
      PolyML.PrettyString("\"" ^ G.BBox.string_of_name x ^ "\"");
in
    val () = PolyML.addPrettyPrinter prettyVertex;
    val () = PolyML.addPrettyPrinter prettyEdge;
    val () = PolyML.addPrettyPrinter prettyBBox;
end; 



(* incomplete and needs lots more stuff... eg. table

structure RGGraph :> RG_GRAPH 
  where type Vertex.data = RGVertex.data
    and type Vertex.name = RGVertex.name 
    and type Vertex.renaming = RGVertex.renaming
    and type Vertex.NSet.T = RGVertex.NSet.T 
    and type 'a Vertex.NTab.T = 'a RGVertex.NTab.T 
    and type Edge.data = UnitEdge.data
    and type Edge.name = UnitEdge.name
    and type Edge.renaming = UnitEdge.renaming
    and type Edge.NSet.T = UnitEdge.NSet.T 
    and type 'a Edge.NTab.T = 'a UnitEdge.NTab.T 
  = DB_RGGraph;
*)


(* structure Vertex = RGGraph.Vertex;
structure Edge = RGGraph.Edge;
*)
