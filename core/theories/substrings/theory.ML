(* strings on vertices; prefix-matching *)
structure Substrings_Data =
struct
  val pretty_theory_name = Pretty.str "substrings"
  type psubst = unit
  type subst  = psubst

  type nvdata = string
  val default_nvdata = "undefined"

  fun default_nvdata_of_typestring "String" = default_nvdata
    | default_nvdata_of_typestring s = raise unknown_typestring_exp s

  fun nvdata_eq (s1,s2) = s1 = s2

  val pretty_nvdata = Pretty.str

  fun match_nvdata (s1,s2) () =
        if String.isPrefix s1 s2 then SOME () else NONE

  fun subst_in_nvdata sub d = (sub, d)

  open EmptyEdgeData

  fun init_psubst_from_data _ _ = ()
  val solve_psubst = Seq.single
end

structure Substrings_Theory = GraphicalTheory(structure Data = Substrings_Data)
