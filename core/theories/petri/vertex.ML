signature Petri_UNIFIABLE_DATA =    
sig
  datatype node = COPY | SWITCH | BUF0 | BUF1;
  include UNIFIABLE_DATA
  sharing type data = node;
end;

structure Petri_VertexData
: Petri_UNIFIABLE_DATA
= struct
  datatype node = BUF0 | BUF1 | COPY | SWITCH;
  type data = node;
  
  fun data_int BUF0   = 0
    | data_int BUF1   = 1
    | data_int COPY   = 2
    | data_int SWITCH = 3
  
  fun data_ord (x,y) = int_ord (data_int x, data_int y)
  fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;
  fun pretty_data BUF0   = Pretty.str "BUF0"
    | pretty_data BUF1   = Pretty.str "BUF1"
    | pretty_data COPY   = Pretty.str "COPY"
    | pretty_data SWITCH = Pretty.str "SWITCH";
  val print_data = Pretty.writeln o pretty_data;
    
  fun match_data (d1,d2) m = if d1 = d2 then SOME m else NONE
  fun unify_data _ _ = SOME ();
    
  type subst = unit;
  val empty_subst = ();
  fun compose_subst _ = ();
  fun subst_in_data _ d = d;  
  
  fun pretty_subst _ = Pretty.str "";
  val print_subst = Pretty.writeln o pretty_subst;
  
  val default_data = COPY; 
  
  structure Sharing = struct type data = data; type subst = subst; end; 
end;


(*  *)
structure Petri_VertexComponentData_Param
: ELEMENT_COMPONENT_DATA_PARAM where type data = Petri_VertexData.data
= struct 
  type data = Petri_VertexData.data;
  exception unknown_type_exp of string;

  fun type_of Petri_VertexData.BUF0   = "BUF0"
    | type_of Petri_VertexData.BUF1   = "BUF1"
    | type_of Petri_VertexData.COPY   = "COPY"
    | type_of Petri_VertexData.SWITCH = "SWITCH"

  fun cdata_of _ = ComponentData.Unit;

  fun default_data "BUF0" = Petri_VertexData.BUF0
    | default_data "BUF1" = Petri_VertexData.BUF1
    | default_data "COPY" = Petri_VertexData.COPY
    | default_data "SWITCH" = Petri_VertexData.SWITCH
    | default_data n = raise unknown_type_exp n;

  fun update _ = I;
end;
