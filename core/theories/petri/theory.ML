(* Generated using ./theories/generate-no-data-theory.py Petri petri COPY SWITCH BUF0 BUF1 *)
structure Petri_Data =
struct
  val pretty_theory_name = Pretty.str "petri"
  type psubst = unit
  type subst  = psubst
  
  datatype nvdata = COPY | SWITCH | BUF0 | BUF1
  val default_nvdata = COPY
  fun nvdata_eq (a,b) = a = b
  
  fun match_nvdata (x,y) () = if nvdata_eq (x,y) then SOME () else NONE
  fun subst_in_nvdata sub d = (sub,d)
  
  fun pretty_nvdata COPY   = Pretty.str "COPY"
    | pretty_nvdata SWITCH = Pretty.str "SWITCH"
    | pretty_nvdata BUF0   = Pretty.str "BUF0"
    | pretty_nvdata BUF1   = Pretty.str "BUF1"
  
  fun default_nvdata_of_typestring "COPY"   = COPY
    | default_nvdata_of_typestring "SWITCH" = SWITCH
    | default_nvdata_of_typestring "BUF0"   = BUF0
    | default_nvdata_of_typestring "BUF1"   = BUF1
    | default_nvdata_of_typestring s = raise unknown_typestring_exp s
  
  open EmptyEdgeData
  
  fun init_psubst_from_data _ _ = ()
  val solve_psubst = Seq.single
end

structure Petri_Theory = GraphicalTheory(structure Data = Petri_Data)

