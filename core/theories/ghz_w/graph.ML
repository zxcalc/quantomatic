(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  GHZ/W graphs, vertices can be black or white and have unit data *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure GHZW_GraphParam
: GRAPH_PARAM
= struct
  val pretty_theory_name = Pretty.str "ghz_w";

  structure VData = GHZW_VertexData;

  (* edges have unit data, and are ignored as we use Vertex subst type *)
  structure EData = 
    IgnoredUnifiableData(structure D = UnitData structure S = UnitSubst
    val default_data = ());
  
  exception match_exp of unit
  
  (* substitutions for variables in vertex/edge data 
     (usually, we only have data on vertices) *)
  type subst = unit;

  val empty_subst = ();
  fun compose_subst u1 u2 = (); 
  fun subst_in_vertex () = I;
  fun subst_in_edge () = I;
  fun try_match_edge e1 e2 u = SOME u;
  fun match_edge e1 e2 u = u;
  fun try_match_vertex d1 d2 u = VData.match_data (d1,d2) u;
  fun match_vertex v1 v2 u = 
      (case try_match_vertex v1 v2 u 
         of NONE    => raise match_exp ()
          | SOME u' => u');
  
  (* pretty printing *)
  fun pretty_subst () = Pretty.str "unit"
  val print_subst = Pretty.writeln o pretty_subst;  
  
  (* INTERNAL NAME DEPENDENCIES *)
  (* internal name data *)
  type T = unit;
  (* empty name-dependency information *)
  val empty = ();
  fun add_vertex vd vn vdep = vdep;
  fun add_edge ed en vdep = vdep;
  fun of_vertex vd vn = empty;
  fun of_edge en ed = empty;
  fun delete_vertex vn vdep = vdep;
  fun delete_edge en vdep = vdep;
  fun rename_vname n1 n2 d = d;
  fun rename_ename n1 n2 d = d;
  fun replace_vertex vd vn vdep = vdep; 
  fun replace_edge ed en vdep = vdep;
  
  (* rename internal names in the dependency structure *)
  (* RENAMING *)
  (* [dummy] Renaming internal names within vertex data *)
  structure VDataCRnmX : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = X type obj = VData.data);
  (* [dummy] Rename internal names within edge data *)
  structure EDataCRnmX : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = X type obj = EData.data);
  (* [dummy] Rename vertex-names in the dependency data *)
  structure CRnmV : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = V type obj = T);
  (* [dummy] Rename edge-names in the dependency data *)
  structure CRnmE : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = E type obj = T);
  (* [dummy] Rename internal names in T *)
  structure CRnmX : COMPOUND_RENAMING
  = EmptyCompoundRenamingFun(structure Nm = X type obj = T);

  
  structure Sharing = 
  struct 
    structure VData = VData.Sharing;
    structure EData = EData.Sharing;
    type T = T;
    type inames_renaming = CRnmX.renaming; (* = IName.Rnm.renaming *)
    type subst = subst;
  end;
end;

structure GHZW_OVertex = OVertexData(GHZW_VertexData);
structure GHZW_OGraphParam = OGraphParam(
  structure Param = GHZW_GraphParam 
        and OVData = GHZW_OVertex);

structure GHZW_OVertexComponentData_Param = OVertexComponentData_Param(
  structure VertexDataParam = GHZW_VertexComponentData_Param
  structure OVData = GHZW_OVertex);

structure GHZW_OVertexComponentData = ElementComponentData(GHZW_OVertexComponentData_Param);

