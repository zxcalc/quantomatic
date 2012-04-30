(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  GHZ/W graphs, vertices can be black or white and have unit data *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure RGB_GraphParam
: GRAPH_PARAM
= struct
  val pretty_theory_name = Pretty.str "red_green_blue";

  structure VData = RGB_VertexData;
  (* edges have unit data, and are ignored as we use Vertex subst type *)
  structure EData = 
    IgnoredUnifiableData(structure D = UnitData structure S = UnitSubst
                         val default_data = ());
  
  type subst = unit; 

  val empty_subst = VData.empty_subst; (* : IName.NTab.empty; *)
  fun compose_subst u1 u2 = VData.compose_subst (u1,u2); 
      (* : Name.NTab.map_all (LinratAngleExpr.subs u2) u1; *)

  fun try_match_vertex d1 d2 u = VData.match_data (d1,d2) u;
  fun try_match_edge e1 e2 u = SOME u;

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

  fun subst_in_vertex u vd = VData.subst_in_data u vd;
  fun subst_in_edge u ed = ed;

  val pretty_subst = VData.pretty_subst;
  val print_subst = Pretty.writeln o pretty_subst;

  structure NoInternalData = NoInternalDataGraphParam(
    type vdata = VData.data type edata = EData.data);
  open NoInternalData;
    
  
  structure Sharing = 
  struct 
    structure VData = VData.Sharing;
    structure EData = EData.Sharing;
    type T = T;
    type subst = subst;
  end;
end;

structure RGB_OVertexData = OVertexData(RGB_VertexData);
structure RGB_OGraphParam = OGraphParam(
  structure Param = RGB_GraphParam 
        and OVData = RGB_OVertexData);

structure RGB_OVertexComponentData_Param = OVertexComponentData_Param(
  structure VertexDataParam = RGB_VertexComponentData_Param
  structure OVData = RGB_OVertexData);

structure RGB_OVertexComponentData = ElementComponentData(RGB_OVertexComponentData_Param);


