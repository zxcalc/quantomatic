(* Generated using ./theories/generate-no-data-theory.py RGB red_green_blue Red Green Blue *)
structure RGB_Data =
struct
  val pretty_theory_name = Pretty.str "red_green_blue"
  type psubst = unit
  type subst  = psubst
  
  datatype nvdata = Red | Green | Blue
  val default_nvdata = Red
  fun nvdata_eq (a,b) = a = b
  
  fun match_nvdata (x,y) () = if nvdata_eq (x,y) then SOME () else NONE
  fun subst_in_nvdata sub d = (sub,d)
  
  fun pretty_nvdata Red   = Pretty.str "Red"
    | pretty_nvdata Green = Pretty.str "Green"
    | pretty_nvdata Blue  = Pretty.str "Blue"
  
  fun default_nvdata_of_typestring "Red"   = Red
    | default_nvdata_of_typestring "Green" = Green
    | default_nvdata_of_typestring "Blue"  = Blue
    | default_nvdata_of_typestring s = raise unknown_typestring_exp s
  
  open EmptyEdgeData
  
  fun init_psubst_from_data _ _ = ()
  val solve_psubst = Seq.single
end
