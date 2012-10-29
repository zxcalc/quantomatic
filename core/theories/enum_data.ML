signature ENUM_DATA =
sig
	type data
	val default   : data
	val int_of    : data -> int
	val string_of : data -> string
	val of_string : string -> data option
end

functor EnumUnifiableData(EnumData : ENUM_DATA) : UNIFIABLE_DATA =
struct
  open EnumData
	fun data_ord (d1,d2) = int_ord (int_of d1, int_of d2)
  fun data_eq (d1, d2) = case data_ord (d1, d2) of EQUAL => true | _ => false;
  fun pretty_data d = Pretty.str (string_of d)
  val print_data = Pretty.writeln o pretty_data;
    
  fun match_data (d1, d2) m = if int_of d1 = int_of d2 then SOME m else NONE;
  fun unify_data (d1, d2) _ = if int_of d1 = int_of d2 then SOME () else NONE;
    
  type subst = unit;
  val empty_subst = ();
  fun compose_subst _ = ();
  fun subst_in_data _ d = d;  
  
  fun pretty_subst _ = Pretty.str "";
  val print_subst = Pretty.writeln o pretty_subst;
  
  val default_data = default; 
  
  structure Sharing = struct type data = data; type subst = subst; end; 
end

functor EnumComponentData(EnumData : ENUM_DATA) : ELEMENT_COMPONENT_DATA_PARAM =
struct
	open EnumData
	exception unknown_type_exp of string;

  fun type_of d = string_of d
  fun cdata_of _ = ComponentData.Unit;
  fun default_data s = case of_string s of SOME d => d | NONE => raise unknown_type_exp s;

  fun update _ = I;
end
