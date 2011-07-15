(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Substring graph matching: vertices have string data and matching is by
    substring. *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure SS_GraphParam
: GRAPH_PARAM
= struct
  (* Vertices with string data and prefix-substring matching *)
  structure Vertex = PrefixStringVertex;
  structure Edge = UnitEdge;

  (* substitutions for variables in vertex/edge data 
     (usually, we only have data on vertices) *)
  type subst = unit;
  
  (* structure for internal names; we use string names, but this is a dummy, we don't actually have any internal names, but we have to provide this *)
  structure IName = SStrName;

  val empty_subst = ();
  fun compose_subst u1 u2 = (); 
  fun try_match_vertex d1 d2 u = SSVertex.match_data (d1,d2);
  fun try_match_edge e1 e2 u = SOME u;

  (* IMPROVE: what data do we want here? should it be add_exp *)
  exception match_exp of unit;
  exception unknown_type_exp of string;
  (* raises match_exp *)
  fun match_vertex d1 d2 u = 
      (case try_match_vertex d1 d2 u 
         of NONE    => raise match_exp ()
          | SOME u' => u'); 
  fun match_edge d1 d2 u =
      (case try_match_edge d1 d2 u 
         of NONE    => raise match_exp ()
          | SOME u' => u'); 

  fun subst_in_vertex u vd = vd;
  fun subst_in_edge u ed = ed;

  val pretty_subst = Vertex.pretty_subst;
  val print_subst = Pretty.writeln o pretty_subst;

  (* internal variable-name to vertex-name dependencies; we have no variables 
     in vertex data, so this is unit. *)
  type T = unit;

  (* empty name-dependency information *)
  val empty = ();

  fun add_vertex vd vn vdep = vdep;
  fun add_edge ed en vdep = vdep;

  fun delete_vertex vn vdep = vdep;
  fun delete_edge en vdep = vdep;

  fun vertex_type_name _ = "subst"
  fun vertex_default_data "subst" = ""
    | vertex_default_data s       = raise unknown_type_exp s
  fun edge_type_name () = "unit"
  fun edge_default_data "unit" = ()
    | edge_default_data s = raise unknown_type_exp s

  fun replace_vertex vd vn vdep = vdep; 
  fun replace_edge ed en vdep = vdep;

  fun of_vertex vd vn = empty;
  fun of_edge en ed = empty;

  (* RENAMING *)
  (* [dummy] Renaming internal names within vertex data *)
  structure VDataCRnmINames : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = IName type obj = Vertex.data);
  (* [dummy] Rename internal names within edge data *)
  structure EDataCRnmINames : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = IName type obj = Edge.data);
  (* [dummy] Rename vertex-names in the dependency data *)
  structure CRnmVNames : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = Vertex type obj = T);
  (* [dummy] Rename edge-names in the dependency data *)
  structure CRnmENames : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = Edge type obj = T);
  (* [dummy] Rename internal names in T *)
  structure CRnmINames : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = IName type obj = T);

  (* convenient syntax for sharing types *)
  structure Sharing = 
  struct 
    structure Vertex = Vertex.SharingWithData;
    structure Edge = Edge.EdgeSharingWithData;
    type T = T;
    type inames_renaming = CRnmINames.renaming; (* = IName.Rnm.renaming *)
    type subst = subst;
  end;
end;


signature SS_GRAPH_SHARING = 
sig
  include BANG_GRAPH_SHARING
end


signature SS_GRAPH = 
sig
include BANG_GRAPH 
structure SSVertex : SS_VERTEX
sharing OVertex.SharingWithIData = SSVertex.SharingWithIData
end;

(* IMPROVE provide table holding variable info for each vertices data *)

structure DB_SSGraph
= struct 
  structure SSVertex = SSVertex;
  structure BasicBangGraph = BasicBangGraphFun(
    structure OVertex = SSVertex  
    structure Param = SS_GraphParam
    type data = unit (* no extra inherrited data for graphs *)
    val empty_data = ());
  structure BBGraph = BangGraphTools(BasicBangGraph);
  open BBGraph;
end;

structure SSGraph : SS_GRAPH = DB_SSGraph;

(* install pretty printers *)
local
    fun prettyVertex _ _ x =
      PolyML.PrettyString("\"" ^ SSGraph.Vertex.string_of_name x ^ "\"");
    fun prettyEdge _ _ x =
      PolyML.PrettyString("\"" ^ SSGraph.Edge.string_of_name x ^ "\"");
    fun prettyBBox _ _ x =
      PolyML.PrettyString("\"" ^ SSGraph.BBox.string_of_name x ^ "\"");
in
    val () = PolyML.addPrettyPrinter prettyVertex;
    val () = PolyML.addPrettyPrinter prettyEdge;
    val () = PolyML.addPrettyPrinter prettyBBox;
end; 

(* ML utility code for building SS graphs *)
structure MkG = 
struct
  structure V = SSGraph.RGVertex;
  structure E = SSGraph.Edge;
  structure G = SSGraph;
  val bvert = V.EVert;
  fun mk a = V.IVert a;
  val undir_edge = (G.UnDirected,());
  val dir_edge = (G.Directed,());
  val no_extra_names = (V.NSet.empty, E.NSet.empty, G.BBox.NSet.empty);
end;
