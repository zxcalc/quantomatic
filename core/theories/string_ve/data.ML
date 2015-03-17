structure StringVE_Data =
struct
  type psubst = unit
  type subst = unit
  type nvdata = string
  type edata = string
  val pretty_theory_name = Pretty.str "string_ve";
  fun init_psubst_from_data _ _ = ()
  fun nvdata_eq (s1,s2) = (s1=s2)
  fun edata_eq (s1,s2) = (s1=s2)
  fun match_nvdata (s1,s2) _ = if nvdata_eq (s1,s2) then SOME () else NONE
  fun match_edata (s1,s2) _ = if edata_eq (s1,s2) then SOME () else NONE
  val default_nvdata = ""
  val default_edata = ""
  val solve_psubst = Seq.single
  fun subst_in_nvdata _ s = ((),s)
  fun subst_in_edata _ s = ((),s)

  fun default_nvdata_of_typestring _ = ""
  fun default_edata_of_typestring _ = ""

  val pretty_nvdata = Pretty.str
  val pretty_edata = Pretty.str

  open NoXNames
end
