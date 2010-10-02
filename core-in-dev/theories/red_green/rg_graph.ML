
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure RG_GraphParam
: GRAPH_PARAM
= struct

  structure Vertex = RGVertex;
  structure Edge = UnitEdge;

  structure IName = AngleExpr.VName;
  
  (* vertex, and vertex data variable dependencies *)
  structure VDep = NameBRelFun(structure Dom = Vertex and Cod = IName);
  
  (* internal name data *)
  type T = VDep.T;

  fun get_IName vdep = VDep.get_codset vdep;
  fun get_vnames vdep = VDep.get_domset vdep;

  fun try_rename1_iname n1 n2 vdep = VDep.try_rename1_cod n1 n2 vdep;
  fun try_rename1_vname n1 n2 vdep = VDep.try_rename1_dom n1 n2 vdep;

  fun rename_vname n1 n2 vdep = the (VDep.try_rename1_dom n1 n2 vdep);
  fun rename_ename n1 n2 vdep = vdep;
  
  (* rename internal names in T *)
  (* rename edges in the dependency structure *)
  structure CRnmINames : COMPOUND_RENAMING
  = CompoundRenamingFun(
    struct structure Nm = IName
           type obj = T
           val get_nameset = get_IName
           val try_rename1 = try_rename1_iname
    end);

  (* rename vertices in the dependency structure *)
  structure CRnmVNames : COMPOUND_RENAMING
  = CompoundRenamingFun(
    struct structure Nm = Vertex
           type obj = T
           val get_nameset = get_vnames
           val try_rename1 = try_rename1_vname
    end);

  (* rename edges in the dependency structure *)
  structure CRnmENames : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = Edge type obj = T);

  (* empty name-dependency information *)
  val empty = VDep.empty;

  fun add_vertex vd vn vdep = 
      (case Vertex.angle_of_data vd of NONE => vdep
          | SOME angle => VDep.add_to_dom vn (AngleExpr.get_vars angle) vdep);
  fun add_edge ed en vdep = vdep;

  fun delete_vertex vn vdep = VDep.del_dom vn vdep;
  fun delete_edge en vdep = vdep;

  fun replace_vertex vd vn vdep = 
      vdep |> delete_vertex vn
           |> add_vertex vd vn;
  fun replace_edge ed en vdep = vdep;

  fun of_vertex vd vn = add_vertex vd vn VDep.empty;
  fun of_edge en ed = empty;

  (* renaming internal names within a vertex *)
  structure VDataCRnmINames : COMPOUND_RENAMING
  = struct 
    type obj = Vertex.data
    type renaming = IName.Rnm.T

    val empty_renaming = CRnmINames.empty_renaming

    fun rename rnm vd = 
        (case Vertex.angle_of_data vd of NONE => (rnm,vd)
         | SOME angle => 
           let val (rnm',angle') = AngleExpr.CRnm.rename rnm angle
           in (rnm', Vertex.map_to_angle (K angle') vd) end)

    fun mk_renaming vd rnm = 
        (case Vertex.angle_of_data vd of NONE => rnm
         | SOME angle => AngleExpr.CRnm.mk_renaming angle rnm);

    fun do_renaming rnm vd = 
        (case Vertex.angle_of_data vd of NONE => vd
         | SOME angle => 
           Vertex.map_to_angle (AngleExpr.CRnm.do_renaming rnm) vd);

    val restrict_to_avoiding = IName.Rnm.restrict_to_avoiding;
  end;

  (* rename internal names within an edge data *)
  structure EDataCRnmINames : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = IName type obj = Edge.data);

  (* Data for unification of data: we only have data on vertices *)
  type unif = RGVertex.unif;
    (* = AngleExpr.T IName.NTab.T; *)

  val empty_unif = RGVertex.empty_unif; (* = IName.NTab.empty; *)
  fun compose_unif u1 u2 = RGVertex.compose_unif (u1,u2); 
      (* = Name.NTab.map_all (AngleExpr.subs u2) u1; *)

(*   fun rg_unif_vertex (e1 as Vertex.Hnd) (e2 as Vertex.Hnd) m = SOME m
    | rg_unif_vertex (Vertex.Znd a1) (Vertex.Znd a2) m = 
      AngleExpr.match a1 a2 m
    | rg_unif_vertex (Vertex.Xnd a1) (Vertex.Xnd a2) m = 
      AngleExpr.match a1 a2 m
    | rg_unif_vertex _ _ _ = NONE;

  (Vertex.IVert d1) (Vertex.IVert d2) u = 
      rg_unif_vertex d1 d2 u
    | unif_vertex Vertex.EVert Vertex.EVert u = SOME u
    | unif_vertex _ _ _ = NONE;
*)

  fun unif_vertex d1 d2 u = RGVertex.unify_data (d1,d2) u;
  fun unif_edge e1 e2 u = SOME u;

  fun subst_in_vertex u vd = RGVertex.subst_in_data u vd;
  fun subst_in_edge u ed = ed;



  fun pretty_unif u = 
    Pretty.chunks (map (fn (n,e) => 
        Pretty.block [Pretty.str n, Pretty.str " = ", AngleExpr.pretty e])
      (IName.NTab.list_of u));
  val print_unif = Pretty.writeln o pretty_unif;

  structure Sharing = 
  struct 
    structure Vertex = Vertex.SharingWithData;
    structure Edge = Edge.EdgeSharingWithData;
    type T = T;
    type inames_renaming = CRnmINames.renaming; (* = IName.Rnm.renaming *)
    type unif = unif;
  end;
end;




signature RG_GRAPH_SHARING = 
sig
  include BANG_GRAPH_SHARING
end


signature RG_GRAPH = 
sig
include BANG_GRAPH 
structure RGVertex : RG_VERTEX
sharing OVertex.SharingWithIData = RGVertex.SharingWithIData
end;

(* IMPROVE provide table holding variable info for each vertices data *)

structure DB_RGGraph
= struct 
  structure RGVertex = RGVertex;
  structure BasicBangGraph = BasicBangGraphFun(
    structure OVertex = RGVertex  
    structure Param = RG_GraphParam
    type data = unit (* no extra inherrited data for graphs *)
    val empty_data = ());
  structure BBGraph = BangGraphTools(BasicBangGraph);
  open BBGraph;
end;

structure RGGraph : RG_GRAPH = DB_RGGraph;
structure Vertex = RGGraph.RGVertex;
structure Edge = RGGraph.Edge;

structure V = RGGraph.OVertex;
structure E = RGGraph.Edge;
structure G = RGGraph;

structure MkG = 
struct
val bvert = V.EVert;
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
