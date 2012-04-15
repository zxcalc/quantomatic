(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Substring graph matching: vertices have string data and matching is by
    substring. *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure Str_GraphParam
: GRAPH_PARAM
= struct
  val pretty_theory_name = Pretty.str "strings";

  (* Vertices with string data and prefix-substring matching *)
  structure VData = PrefixStringVertexData;
  structure EData = IgnoredUnifiableData(structure D = UnitData;
  structure S = UnitSubst; val default_data = ());

  (* substitutions for variables in vertex/edge data 
     (usually, we only have data on vertices) *)
  type subst = unit;

  val empty_subst = ();
  fun compose_subst u1 u2 = (); 
  fun try_match_vertex d1 d2 u = VData.match_data (d1,d2) u;
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

  val pretty_subst = VData.pretty_subst;
  val print_subst = Pretty.writeln o pretty_subst;

  structure NoInternalData = NoInternalDataGraphParam(
    type vdata = VData.data type edata = EData.data);
  open NoInternalData;
  
  (* convenient syntax for sharing types *)
  structure Sharing = 
  struct 
    structure VData = VData.Sharing;
    structure EData = EData.Sharing;
    type T = T;
    type xnames_renaming = CRnmX.renaming; (* = IName.Rnm.renaming *)
    type subst = subst;
  end;
end;


structure Str_OVData = OVertexData(Str_GraphParam.VData);
structure Str_OGraphParam = OGraphParam(
  structure Param = Str_GraphParam 
        and OVData = Str_OVData);

structure Str_OVertexComponentData_Param = OVertexComponentData_Param(
  structure VertexDataParam = Str_VertexComponentData_Param
  structure OVData = Str_OVData);

structure Str_OVertexComponentData = 
  ElementComponentData(Str_OVertexComponentData_Param);

(* ML utility code for building SS graphs *)
(*
structure MkG = 
struct
  structure G = StrGraph;
  val bvert = G.OVData.EVert;
  fun mk a = G.OVData.IVert a;
  val undir_edge = (G.UnDirected,());
  val dir_edge = (G.Directed,());
  val no_extra_names = (V.NSet.empty, E.NSet.empty, B.NSet.empty);
end;
*)
