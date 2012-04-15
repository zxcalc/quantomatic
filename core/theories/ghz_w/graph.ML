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
  
  structure NoInternalData = NoInternalDataGraphParam(
    type vdata = VData.data type edata = EData.data);
  open NoInternalData;
  
  structure Sharing = 
  struct 
    structure VData = VData.Sharing;
    structure EData = EData.Sharing;
    type T = T;
    type xnames_renaming = CRnmX.renaming; (* = IName.Rnm.renaming *)
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

