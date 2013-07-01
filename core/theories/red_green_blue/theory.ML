structure RGB_Data =
struct
  val pretty_theory_name = Pretty.str "red_green_blue"
  type psubst = unit
  type subst  = psubst

  datatype nvdata = Red | Green | Blue
  val default_nvdata = Red

  val nvdata_typestrings = ["Red","Green","Blue"]
  fun default_nvdata_of_typestring s =
    case s of "Red"   => Red
            | "Green" => Green
            | "Blue"  => Blue
            | _ => raise unknown_typestring_exp s
  fun typestring_of_nvdata d =
    case d of Red   => "Red"
            | Green => "Green"
            | Blue  => "Blue"
  
  fun nvdata_eq (a,b) = a = b

  val pretty_nvdata = Pretty.str o typestring_of_nvdata
  
  fun match_nvdata (x,y) () = if nvdata_eq (x,y) then SOME () else NONE

  fun subst_in_nvdata sub d = (sub, d)

  open EmptyEdgeData

  fun init_psubst_from_data _ _ = ()
  val solve_psubst = Seq.single
end

structure RGB_Theory = GraphicalTheory(structure Data = RGB_Data)

