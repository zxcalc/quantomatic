signature RGB_UNIFIABLE_DATA =    
sig
  datatype node = Red | Green | Blue;
  include UNIFIABLE_DATA
  sharing type data = node;
end;

structure RGB_VertexData
: RGB_UNIFIABLE_DATA
= struct
  datatype node = Red | Green | Blue;
  type data = node;
  
  fun as_int Red = 0
    | as_int Green = 1
    | as_int Blue = 2
  
  fun data_ord (d1,d2) = int_ord (as_int d1, as_int d2)
  fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;
  fun pretty_data Red   = Pretty.str "Red"
    | pretty_data Green = Pretty.str "Green"
    | pretty_data Blue  = Pretty.str "Blue";
  val print_data = Pretty.writeln o pretty_data;
    
  fun match_data pair m = if data_eq pair then SOME m else NONE
  fun unify_data _ _ = SOME ();
    
  type subst = unit;
  val empty_subst = ();
  fun compose_subst _ = ();
  fun subst_in_data _ d = d;  
  
  fun pretty_subst _ = Pretty.str "";
  val print_subst = Pretty.writeln o pretty_subst;
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
structure RGB_Vertex
: VERTEX
= struct
  
  open RGB_VertexData;
  open SStrName;

  val default_name = mk "a";
  val default_data = RGB_VertexData.Green;

  structure Map = NameMapFun(structure Dom = SStrName and Cod = SStrName);
  structure InjEndo = NameInjEndoFun(SStrName);

  structure SharingWithData = struct 
    open Sharing; 
    type data = RGB_VertexData.data;
    type subst = RGB_VertexData.subst;
    structure Map = Map.Sharing;
    structure InjEndo = InjEndo.Sharing;
  end;
  (* Note signature constraint hides stuff for us *)
  structure SharingWithoutData = SharingWithData;

end;