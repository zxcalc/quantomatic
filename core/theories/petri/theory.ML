structure Petri_Data =
struct
  val pretty_theory_name = Pretty.str "petri"
  type psubst = unit
  type subst  = psubst

  datatype nvdata = COPY | SWITCH | BUF0 | BUF1
  val default_nvdata = COPY

  val nvdata_typestrings = ["COPY","SWITCH","BUF0","BUF1"]
  fun default_nvdata_of_typestring s =
    case s of "COPY"   => COPY
            | "SWITCH" => SWITCH
            | "BUF0"   => BUF0
            | "BUF1"   => BUF1
            | _ => raise unknown_typestring_exp s
  fun typestring_of_nvdata d =
    case d of COPY   => "COPY"
            | SWITCH => "SWITCH"
            | BUF0   => "BUF0"
            | BUF1   => "BUF1"

  fun nvdata_eq (a,b) = a = b

  val pretty_nvdata = Pretty.str o typestring_of_nvdata
  
  fun match_nvdata (x,y) () = if nvdata_eq (x,y) then SOME () else NONE

  fun subst_in_nvdata sub d = (sub, d)

  open EmptyEdgeData

  fun init_psubst_from_data _ _ = ()
  val solve_psubst = Seq.single
end

structure Petri_Theory = GraphicalTheory(structure Data = Petri_Data)

