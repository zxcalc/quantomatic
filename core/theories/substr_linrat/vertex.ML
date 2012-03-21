

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature SUBSTR_LINRAT_UNIFIABLE_DATA =    
sig
  include UNIFIABLE_DATA

  datatype dtype = S_E of (string * LinratAngleExpr.T);
  sharing type data = dtype;
  val angle_of_data : data -> LinratAngleExpr.T option;
  val map_to_angle : (LinratAngleExpr.T -> LinratAngleExpr.T) -> data -> data; 
end;

structure SUBSTR_LINRAT_InternVData : SUBSTR_LINRAT_UNIFIABLE_DATA
= struct
  exception unimplemented_exp of string;

  datatype dtype = S_E of (string * LinratAngleExpr.T);
  type data = dtype;

  fun data_ord  (S_E (s1, e1), S_E (s2, e2)) =
    case String.compare (s1, s2) of EQUAL => LinratAngleExpr.ord (e1, e2)
      | _ => String.compare (s1, s2);
  fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;
  
  fun pretty_data (S_E (s, e)) =
    Pretty.block [Pretty.str s, Pretty.str "(", LinratAngleExpr.pretty e, Pretty.str ")"]
  val print_data = Pretty.writeln o pretty_data;
 
  datatype u_l_dtype = U_L of (unit * LinratAngleExpr.match); 
  type subst = u_l_dtype;
  val empty_subst = (U_L ((),LinratAngleExpr.empty_match));
  fun compose_subst (U_L (u1, l1), U_L (u2, l2)) =
    (U_L ((), LinratAngleExpr.compose_matches l1 l2));
  (* Matching is done by substring prefix then by linrat *)
  fun match_data (S_E (s1, e1), S_E (s2, e2)) (U_L (u, m)) =
    if String.isPrefix s1 s2 then
      case LinratAngleExpr.match e1 e2 m of NONE => NONE
      | SOME a => SOME (U_L ((), a))
    else NONE;

  fun unify_data (x,y) m = 
      raise unimplemented_exp "SUBSTR_LINRAT_InternVData.unify_data: has not yet been written.";

  fun subst_in_data (U_L ((), u)) (S_E (s,e)) = S_E (s, LinratAngleExpr.subst u e);
    
  fun pretty_subst (U_L ((), e_subst)) = 
    Pretty.block [Pretty.str "Vertex subst () ;", LinratAngleExpr.pretty_match
    e_subst];
  val print_subst = Pretty.writeln o pretty_subst;

  fun map_to_angle f (S_E (s, angle)) = S_E (s, f angle);

  fun angle_of_data (S_E (s, angle)) = SOME angle
    
  val default_data = (S_E ("", LinratAngleExpr.zero));
  structure Sharing = struct type data = data; type subst = subst; end; 
end;

(*  *)
structure SUBSTR_LINRAT_VertexComponentData_Param
: ELEMENT_COMPONENT_DATA_PARAM where type data = SUBSTR_LINRAT_InternVData.data
= struct 
  type data = SUBSTR_LINRAT_InternVData.data;
  exception unknown_type_exp of string;
  exception unexpected_data_exp;

  fun type_of (SUBSTR_LINRAT_InternVData.S_E (s, e)) = s;

  fun cdata_of (SUBSTR_LINRAT_InternVData.S_E (s, e)) = (ComponentData.LinratAngleExpr e);

  fun default_data s = (SUBSTR_LINRAT_InternVData.S_E (s, LinratAngleExpr.zero));

  fun unwrapUpd upd a =
    let
      fun unwrapLinrat (ComponentData.LinratAngleExpr e) = e
        | unwrapLinrat _ = raise unexpected_data_exp
    in
      unwrapLinrat (upd (ComponentData.LinratAngleExpr a))
    end;
  
  fun update upd (SUBSTR_LINRAT_InternVData.S_E (s, e)) =
    (SUBSTR_LINRAT_InternVData.S_E (s, unwrapUpd upd e));
end;

