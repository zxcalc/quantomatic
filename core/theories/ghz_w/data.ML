(* Generated using ./theories/generate-no-data-theory.py GHZW ghzw GHZ W TICK ZERO *)
structure GHZW_Data =
struct
  val pretty_theory_name = Pretty.str "ghz_w"
  type psubst = unit
  type subst  = psubst
  
  datatype nvdata = GHZ | W | TICK | ZERO
  val default_nvdata = GHZ
  fun nvdata_eq (a,b) = a = b
  
  fun match_nvdata (x,y) () = if nvdata_eq (x,y) then SOME () else NONE
  fun subst_in_nvdata sub d = (sub,d)
  
  fun pretty_nvdata GHZ  = Pretty.str "GHZ"
    | pretty_nvdata W    = Pretty.str "W"
    | pretty_nvdata TICK = Pretty.str "TICK"
    | pretty_nvdata ZERO = Pretty.str "ZERO"
  
  fun default_nvdata_of_typestring "GHZ"  = GHZ
    | default_nvdata_of_typestring "W"    = W
    | default_nvdata_of_typestring "TICK" = TICK
    | default_nvdata_of_typestring "ZERO" = ZERO
    | default_nvdata_of_typestring s = raise unknown_typestring_exp s
  
  open EmptyEdgeData
  
  fun init_psubst_from_data _ _ = ()
  val solve_psubst = Seq.single
end
