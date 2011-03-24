(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Vertices string data and prefix substring matching *)
signature SS_VERTEX
= sig
  include OVERTEX
  sharing type IData.data = string;
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
structure SSVertex
: SS_VERTEX
= struct

  structure V = OVertexFun(
    structure IData = 
    struct
      type data = string;
      fun data_ord (s1,s2) = String.compare;
      fun data_eq (n1, n2) = (n1 = n2);
      
      fun pretty_data = Pretty.str;
      val print_data = Pretty.writeln o pretty_data;
      
      (* no variables in our data, so no real substitution *)
      type subst = unit;
      val empty_subst = ();
      fun compose_subst (u1,u2) = ();
      fun match_data (ss as (s1, s2)) m = 
          if String.isPrefix ss then SOME m else NONE; 
      fun unify_data (x,y) m = m;
      fun subst_in_data u x = x;
        
      val pretty_subst = Pretty.str "vertex subst: ()";
      val print_subst = Pretty.writeln o pretty_subst;  
    end;
    structure VertexNames = SStrName
    ); (* internal OVertex structure *)

  open V;
  
  val default_name = mk "a";

end;
