structure RTechn_Data =
struct
  val pretty_theory_name = Pretty.str "isaplanner_rtechn"
  type psubst = unit
  type subst  = psubst

  structure RTechnKey : NAME where type name = string = StrName;

  datatype nvdata = RTechnNode of RTechnKey.name
                  | GoalNode   of  StrName.name  (* goal/wire type *)
                                 * StrIntName.NSet.T; (* goal names of this type *)
  val default_nvdata = RTechnNode "?"

  fun default_nvdata_of_typestring s =
    case s of "RT" => RTechnNode "?"
            | "Gs" => GoalNode ("?", StrIntName.NSet.empty)
            | _ => raise unknown_typestring_exp s

  fun nvdata_eq (RTechnNode k1, RTechnNode k2) = RTechnKey.name_eq (k1,k2)
    | nvdata_eq (GoalNode (n1,s1), GoalNode (n2,s2)) =
        StrName.name_eq (n1,n2) andalso StrIntName.NSet.eq s1 s2
    | nvdata_eq _ = false
      
  fun pretty_nvdata (RTechnNode n) = 
      Pretty.block [Pretty.str "RT(", RTechnKey.pretty_name n, Pretty.str ")"]  
    | pretty_nvdata (GoalNode (n,s)) = 
      Pretty.block [Pretty.str "Gs(", StrName.pretty_name n, Pretty.str ",",
                    StrIntName.NSet.pretty s, Pretty.str ")"]  
  
  fun match_nvdata (RTechnNode n1, RTechnNode n2) subst =
        if String.isPrefix n1 n2 then SOME subst else NONE
    | match_nvdata (GoalNode (n1,_), GoalNode (n2,_)) subst =
        if String.isPrefix n1 n2 then SOME subst else NONE
    | match_nvdata _ _ = NONE

  fun subst_in_nvdata sub d = (sub, d)

  type edata = string
  val default_edata = ""

  fun default_edata_of_typestring _ = ""

  fun edata_eq (s1,s2) = s1 = s2

  val pretty_edata = Pretty.str
  
  fun match_edata (s1,s2) u = if s1 = s2 then SOME u else NONE

  fun subst_in_edata sub d = (sub, d)

  fun init_psubst_from_data _ _ = ()
  val solve_psubst = Seq.single
end

structure RTechn_Theory = GraphicalTheory(structure Data = RTechn_Data)

