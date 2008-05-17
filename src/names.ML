structure InterpStrName : STR_INTERP_NAME
= struct
  open StrName;
  val mk = (I : string -> StrName.T);
  val dest = (I : StrName.T -> string);
end;

(* Safely hide string names; cannot confuse NName.T with EName.T etc *)

structure NName :> STR_INTERP_NAME = InterpStrName;
structure EName :> STR_INTERP_NAME = InterpStrName;
structure VName :> STR_INTERP_NAME = InterpStrName;

install_pp (make_pp ["NName", "T"] (Pretty.pprint o NName.pretty));
install_pp (make_pp ["EName", "T"] (Pretty.pprint o EName.pretty));
install_pp (make_pp ["VName", "T"] (Pretty.pprint o VName.pretty));
