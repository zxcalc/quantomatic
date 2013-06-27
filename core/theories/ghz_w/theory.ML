structure GHZW_Data =
struct
  val pretty_theory_name = Pretty.str "ghzw"
  type psubst = unit
  type subst  = psubst

  datatype nvdata = GHZ | W | TICK | ZERO
  val default_nvdata = GHZ

  val nvdata_typestrings = ["GHZ","W","TICK","ZERO"]
  fun default_nvdata_of_typestring s =
    case s of "GHZ"  => GHZ
            | "W"    => W
            | "TICK" => TICK
            | "ZERO" => ZERO
            | _ => raise unknown_typestring_exp s
  fun typestring_of_nvdata d =
    case d of GHZ  => "GHZ"
            | W    => "W"
            | TICK => "TICK"
            | ZERO => "ZERO"
  
  fun nvdata_eq (a,b) = a = b

  val pretty_nvdata = Pretty.str o typestring_of_nvdata
  
  fun match_nvdata (x,y) () = if nvdata_eq (x,y) then SOME () else NONE

  fun subst_in_nvdata _ = I

  open EmptyEdgeData

  fun init_psubst_from_data _ = ()
  val solve_psubst = Seq.single
end

structure GHZW_Theory = GraphicalTheory(structure Data = GHZW_Data)

