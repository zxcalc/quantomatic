(*  Title:      Pure/Tools/find_theorems.ML
    Author:     Rafal Kolanski and Gerwin Klein, NICTA
    Author:     Lars Noschinski and Alexander Krauss, TU Muenchen

Retrieve theorems from proof context.
*)

signature FIND_THEOREMS =
sig
  datatype 'term criterion =
    Name of string | Intro | Elim | Dest | Solves | Simp of 'term | Pattern of 'term
  type 'term query = {
    goal: thm option,
    limit: int option,
    rem_dups: bool,
    criteria: (bool * 'term criterion) list
  }
  val query_parser: (bool * string criterion) list parser
  val read_query: Position.T -> string -> (bool * string criterion) list
  val find_theorems: Proof.context -> thm option -> int option -> bool ->
    (bool * term criterion) list -> int option * (Facts.ref * thm) list
  val find_theorems_cmd: Proof.context -> thm option -> int option -> bool ->
    (bool * string criterion) list -> int option * (Facts.ref * thm) list
  val pretty_thm: Proof.context -> Facts.ref * thm -> Pretty.T
  val pretty_theorems: Proof.state ->
    int option -> bool -> (bool * string criterion) list -> Pretty.T
  val proof_state: Toplevel.state -> Proof.state
end;

structure Find_Theorems: FIND_THEOREMS =
struct

(** search criteria **)

datatype 'term criterion =
  Name of string | Intro | Elim | Dest | Solves | Simp of 'term | Pattern of 'term;

fun read_criterion _ (Name name) = Name name
  | read_criterion _ Intro = Intro
  | read_criterion _ Elim = Elim
  | read_criterion _ Dest = Dest
  | read_criterion _ Solves = Solves
  | read_criterion ctxt (Simp str) = Simp (Proof_Context.read_term_pattern ctxt str)
  | read_criterion ctxt (Pattern str) = Pattern (Proof_Context.read_term_pattern ctxt str);

fun pretty_criterion ctxt (b, c) =
  let
    fun prfx s = if b then s else "-" ^ s;
  in
    (case c of
      Name name => Pretty.str (prfx "name: " ^ quote name)
    | Intro => Pretty.str (prfx "intro")
    | Elim => Pretty.str (prfx "elim")
    | Dest => Pretty.str (prfx "dest")
    | Solves => Pretty.str (prfx "solves")
    | Simp pat => Pretty.block [Pretty.str (prfx "simp:"), Pretty.brk 1,
        Pretty.quote (Syntax.pretty_term ctxt (Term.show_dummy_patterns pat))]
    | Pattern pat => Pretty.enclose (prfx "\"") "\""
        [Syntax.pretty_term ctxt (Term.show_dummy_patterns pat)])
  end;



(** queries **)

type 'term query = {
  goal: thm option,
  limit: int option,
  rem_dups: bool,
  criteria: (bool * 'term criterion) list
};

fun map_criteria f {goal, limit, rem_dups, criteria} =
  {goal = goal, limit = limit, rem_dups = rem_dups, criteria = f criteria};


(** search criterion filters **)

(*generated filters are to be of the form
  input: (Facts.ref * thm)
  output: (p:int, s:int, t:int) option, where
    NONE indicates no match
    p is the primary sorting criterion
      (eg. size of term)
    s is the secondary sorting criterion
      (eg. number of assumptions in the theorem)
    t is the tertiary sorting criterion
      (eg. size of the substitution for intro, elim and dest)
  when applying a set of filters to a thm, fold results in:
    (max p, max s, sum of all t)
*)


(* matching theorems *)

fun is_nontrivial ctxt = Term.is_Const o Term.head_of o Object_Logic.drop_judgment ctxt;

(*extract terms from term_src, refine them to the parts that concern us,
  if po try match them against obj else vice versa.
  trivial matches are ignored.
  returns: smallest substitution size*)
fun is_matching_thm (extract_terms, refine_term) ctxt po obj term_src =
  let
    val thy = Proof_Context.theory_of ctxt;

    fun matches pat =
      is_nontrivial ctxt pat andalso
      Pattern.matches thy (if po then (pat, obj) else (obj, pat));

    fun subst_size pat =
      let val (_, subst) =
        Pattern.match thy (if po then (pat, obj) else (obj, pat)) (Vartab.empty, Vartab.empty)
      in Vartab.fold (fn (_, (_, t)) => fn n => size_of_term t + n) subst 0 end;

    fun best_match [] = NONE
      | best_match xs = SOME (foldl1 Int.min xs);

    val match_thm = matches o refine_term;
  in
    map (subst_size o refine_term) (filter match_thm (extract_terms term_src))
    |> best_match
  end;


(* filter_name *)

fun filter_name str_pat (thmref, _) =
  if match_string str_pat (Facts.name_of_ref thmref)
  then SOME (0, 0, 0) else NONE;


(* filter intro/elim/dest/solves rules *)

fun filter_dest ctxt goal (_, thm) =
  let
    val extract_dest =
     (fn thm => if Thm.no_prems thm then [] else [Thm.full_prop_of thm],
      hd o Logic.strip_imp_prems);
    val prems = Logic.prems_of_goal goal 1;

    fun try_subst prem = is_matching_thm extract_dest ctxt true prem thm;
    val successful = prems |> map_filter try_subst;
  in
    (*if possible, keep best substitution (one with smallest size)*)
    (*dest rules always have assumptions, so a dest with one
      assumption is as good as an intro rule with none*)
    if not (null successful) then
      SOME (size_of_term (Thm.prop_of thm), Thm.nprems_of thm - 1, foldl1 Int.min successful)
    else NONE
  end;

fun filter_intro ctxt goal (_, thm) =
  let
    val extract_intro = (single o Thm.full_prop_of, Logic.strip_imp_concl);
    val concl = Logic.concl_of_goal goal 1;
  in
    (case is_matching_thm extract_intro ctxt true concl thm of
      SOME k => SOME (size_of_term (Thm.prop_of thm), Thm.nprems_of thm, k)
    | NONE => NONE)
  end;

fun filter_elim ctxt goal (_, thm) =
  if Thm.nprems_of thm > 0 then
    let
      val rule = Thm.full_prop_of thm;
      val prems = Logic.prems_of_goal goal 1;
      val goal_concl = Logic.concl_of_goal goal 1;
      val rule_mp = hd (Logic.strip_imp_prems rule);
      val rule_concl = Logic.strip_imp_concl rule;
      fun combine t1 t2 = Const ("*combine*", dummyT --> dummyT) $ (t1 $ t2);  (* FIXME ?!? *)
      val rule_tree = combine rule_mp rule_concl;
      fun goal_tree prem = combine prem goal_concl;
      fun try_subst prem = is_matching_thm (single, I) ctxt true (goal_tree prem) rule_tree;
      val successful = prems |> map_filter try_subst;
    in
      (*elim rules always have assumptions, so an elim with one
        assumption is as good as an intro rule with none*)
      if is_nontrivial ctxt (Thm.major_prem_of thm) andalso not (null successful) then
        SOME (size_of_term (Thm.prop_of thm), Thm.nprems_of thm - 1, foldl1 Int.min successful)
      else NONE
    end
  else NONE;

fun filter_solves ctxt goal =
  let
    val thy' =
      Proof_Context.theory_of ctxt
      |> Context_Position.set_visible_global (Context_Position.is_visible ctxt);
    val ctxt' = Proof_Context.transfer thy' ctxt;
    val goal' = Thm.transfer thy' goal;

    fun limited_etac thm i =
      Seq.take (Options.default_int @{system_option find_theorems_tactic_limit}) o
        eresolve_tac ctxt' [thm] i;
    fun try_thm thm =
      if Thm.no_prems thm then resolve_tac ctxt' [thm] 1 goal'
      else
        (limited_etac thm THEN_ALL_NEW (Goal.norm_hhf_tac ctxt' THEN' Method.assm_tac ctxt'))
          1 goal';
  in
    fn (_, thm) =>
      if is_some (Seq.pull (try_thm thm))
      then SOME (size_of_term (Thm.prop_of thm), Thm.nprems_of thm, 0)
      else NONE
  end;


(* filter_simp *)

fun filter_simp ctxt t (_, thm) =
  let
    val mksimps = Simplifier.mksimps ctxt;
    val extract_simp =
      (map Thm.full_prop_of o mksimps, #1 o Logic.dest_equals o Logic.strip_imp_concl);
  in
    (case is_matching_thm extract_simp ctxt false t thm of
      SOME ss => SOME (size_of_term (Thm.prop_of thm), Thm.nprems_of thm, ss)
    | NONE => NONE)
  end;


(* filter_pattern *)

fun expand_abs t =
  let
    val m = Term.maxidx_of_term t + 1;
    val vs = strip_abs_vars t;
    val ts = map_index (fn (k, (_, T)) => Var ((Name.aT, m + k), T)) vs;
  in betapplys (t, ts) end;

fun get_names t = Term.add_const_names t (Term.add_free_names t []);

(* Does pat match a subterm of obj? *)
fun matches_subterm thy (pat, obj) =
  let
    fun msub bounds obj = Pattern.matches thy (pat, obj) orelse
      (case obj of
        Abs (_, T, t) => msub (bounds + 1) (snd (Term.dest_abs (Name.bound bounds, T, t)))
      | t $ u => msub bounds t orelse msub bounds u
      | _ => false)
  in msub 0 obj end;

(*Including all constants and frees is only sound because matching
  uses higher-order patterns. If full matching were used, then
  constants that may be subject to beta-reduction after substitution
  of frees should not be included for LHS set because they could be
  thrown away by the substituted function.  E.g. for (?F 1 2) do not
  include 1 or 2, if it were possible for ?F to be (%x y. 3).  The
  largest possible set should always be included on the RHS.*)

fun filter_pattern ctxt pat =
  let
    val pat' = (expand_abs o Envir.eta_contract) pat;
    val pat_consts = get_names pat';
    fun check ((x, thm), NONE) = check ((x, thm), SOME (get_names (Thm.full_prop_of thm)))
      | check ((_, thm), c as SOME thm_consts) =
         (if subset (op =) (pat_consts, thm_consts) andalso
            matches_subterm (Proof_Context.theory_of ctxt) (pat', Thm.full_prop_of thm)
          then SOME (size_of_term (Thm.prop_of thm), Thm.nprems_of thm, 0) else NONE, c);
  in check end;


(* interpret criteria as filters *)

local

fun err_no_goal c =
  error ("Current goal required for " ^ c ^ " search criterion");

fun filter_crit _ _ (Name name) = apfst (filter_name name)
  | filter_crit _ NONE Intro = err_no_goal "intro"
  | filter_crit _ NONE Elim = err_no_goal "elim"
  | filter_crit _ NONE Dest = err_no_goal "dest"
  | filter_crit _ NONE Solves = err_no_goal "solves"
  | filter_crit ctxt (SOME goal) Intro = apfst (filter_intro ctxt (Thm.prop_of goal))
  | filter_crit ctxt (SOME goal) Elim = apfst (filter_elim ctxt (Thm.prop_of goal))
  | filter_crit ctxt (SOME goal) Dest = apfst (filter_dest ctxt (Thm.prop_of goal))
  | filter_crit ctxt (SOME goal) Solves = apfst (filter_solves ctxt goal)
  | filter_crit ctxt _ (Simp pat) = apfst (filter_simp ctxt pat)
  | filter_crit ctxt _ (Pattern pat) = filter_pattern ctxt pat;

fun opt_not x = if is_some x then NONE else SOME (0, 0, 0);

fun opt_add (SOME (a, c, x)) (SOME (b, d, y)) = SOME (Int.max (a,b), Int.max (c, d), x + y : int)
  | opt_add _ _ = NONE;

fun app_filters thm =
  let
    fun app (NONE, _, _) = NONE
      | app (SOME v, _, []) = SOME (v, thm)
      | app (r, consts, f :: fs) =
          let val (r', consts') = f (thm, consts)
          in app (opt_add r r', consts', fs) end;
  in app end;

in

fun filter_criterion ctxt opt_goal (b, c) =
  (if b then I else (apfst opt_not)) o filter_crit ctxt opt_goal c;

fun sorted_filter filters thms =
  let
    fun eval_filters thm = app_filters thm (SOME (0, 0, 0), NONE, filters);

    (*filters return: (thm size, number of assumptions, substitution size) option, so
      sort according to size of thm first, then number of assumptions,
      then by the substitution size, then by term order *)
    fun result_ord (((p0, s0, t0), (_, thm0)), ((p1, s1, t1), (_, thm1))) =
      prod_ord int_ord (prod_ord int_ord (prod_ord int_ord Term_Ord.term_ord))
         ((p1, (s1, (t1, Thm.full_prop_of thm1))), (p0, (s0, (t0, Thm.full_prop_of thm0))));
  in
    grouped 100 Par_List.map eval_filters thms
    |> map_filter I |> sort result_ord |> map #2
  end;

fun lazy_filter filters =
  let
    fun lazy_match thms = Seq.make (fn () => first_match thms)
    and first_match [] = NONE
      | first_match (thm :: thms) =
          (case app_filters thm (SOME (0, 0, 0), NONE, filters) of
            NONE => first_match thms
          | SOME (_, t) => SOME (t, lazy_match thms));
  in lazy_match end;

end;


(* removing duplicates, preferring nicer names, roughly O(n log n) *)

local

val index_ord = option_ord (K EQUAL);
val hidden_ord = bool_ord o apply2 Long_Name.is_hidden;
val qual_ord = int_ord o apply2 Long_Name.qualification;
val txt_ord = int_ord o apply2 size;

fun nicer_name ((a, x), i) ((b, y), j) =
  (case bool_ord (a, b) of EQUAL =>
    (case hidden_ord (x, y) of EQUAL =>
      (case index_ord (i, j) of EQUAL =>
        (case qual_ord (x, y) of EQUAL => txt_ord (x, y) | ord => ord)
      | ord => ord)
    | ord => ord)
  | ord => ord) <> GREATER;

fun rem_cdups nicer xs =
  let
    fun rem_c rev_seen [] = rev rev_seen
      | rem_c rev_seen [x] = rem_c (x :: rev_seen) []
      | rem_c rev_seen ((x as ((n, thm), _)) :: (y as ((n', thm'), _)) :: rest) =
          if Thm.eq_thm_prop (thm, thm')
          then rem_c rev_seen ((if nicer n n' then x else y) :: rest)
          else rem_c (x :: rev_seen) (y :: rest);
  in rem_c [] xs end;

in

fun nicer_shortest ctxt =
  let
    fun extern_shortest name =
      let
        val facts = Proof_Context.facts_of_fact ctxt name;
        val space = Facts.space_of facts;
      in (Facts.is_dynamic facts name, Name_Space.extern_shortest ctxt space name) end;

    fun nicer (Facts.Named ((x, _), i)) (Facts.Named ((y, _), j)) =
          nicer_name (extern_shortest x, i) (extern_shortest y, j)
      | nicer (Facts.Fact _) (Facts.Named _) = true
      | nicer (Facts.Named _) (Facts.Fact _) = false
      | nicer (Facts.Fact _) (Facts.Fact _) = true;
  in nicer end;

fun rem_thm_dups nicer xs =
  (xs ~~ (1 upto length xs))
  |> sort (Term_Ord.fast_term_ord o apply2 (Thm.full_prop_of o #2 o #1))
  |> rem_cdups nicer
  |> sort (int_ord o apply2 #2)
  |> map #1;

end;



(** main operations **)

(* filter_theorems *)

fun all_facts_of ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;
    val transfer = Global_Theory.transfer_theories thy;
    val local_facts = Proof_Context.facts_of ctxt;
    val global_facts = Global_Theory.facts_of thy;
  in
   (Facts.dest_all (Context.Proof ctxt) false [global_facts] local_facts @
     Facts.dest_all (Context.Proof ctxt) false [] global_facts)
   |> maps Facts.selections
   |> map (apsnd transfer)
  end;

fun filter_theorems ctxt theorems query =
  let
    val {goal = opt_goal, limit = opt_limit, rem_dups, criteria} = query;
    val filters = map (filter_criterion ctxt opt_goal) criteria;

    fun find_all theorems =
      let
        val raw_matches = sorted_filter filters theorems;

        val matches =
          if rem_dups
          then rem_thm_dups (nicer_shortest ctxt) raw_matches
          else raw_matches;

        val len = length matches;
        val lim = the_default (Options.default_int @{system_option find_theorems_limit}) opt_limit;
      in (SOME len, drop (Int.max (len - lim, 0)) matches) end;

    val find =
      if rem_dups orelse is_none opt_limit
      then find_all
      else pair NONE o Seq.list_of o Seq.take (the opt_limit) o lazy_filter filters;

  in find theorems end;

fun filter_theorems_cmd ctxt theorems raw_query =
  filter_theorems ctxt theorems (map_criteria (map (apsnd (read_criterion ctxt))) raw_query);


(* find_theorems *)

local

fun gen_find_theorems filter ctxt opt_goal opt_limit rem_dups raw_criteria =
  let
    val assms =
      Proof_Context.get_fact ctxt (Facts.named "local.assms")
        handle ERROR _ => [];
    val add_prems = Seq.hd o TRY (Method.insert_tac ctxt assms 1);
    val opt_goal' = Option.map add_prems opt_goal;
  in
    filter ctxt (all_facts_of ctxt)
      {goal = opt_goal', limit = opt_limit, rem_dups = rem_dups, criteria = raw_criteria}
  end;

in

val find_theorems = gen_find_theorems filter_theorems;
val find_theorems_cmd = gen_find_theorems filter_theorems_cmd;

end;


(* pretty_theorems *)

local

fun pretty_ref ctxt thmref =
  let
    val (name, sel) =
      (case thmref of
        Facts.Named ((name, _), sel) => (name, sel)
      | Facts.Fact _ => raise Fail "Illegal literal fact");
  in
    [Pretty.marks_str (#1 (Proof_Context.markup_extern_fact ctxt name), name),
      Pretty.str (Facts.string_of_selection sel), Pretty.str ":", Pretty.brk 1]
  end;

in

fun pretty_thm ctxt (thmref, thm) =
  Pretty.block (pretty_ref ctxt thmref @ [Thm.pretty_thm ctxt thm]);

fun pretty_theorems state opt_limit rem_dups raw_criteria =
  let
    val ctxt = Proof.context_of state;
    val opt_goal = try Proof.simple_goal state |> Option.map #goal;
    val criteria = map (apsnd (read_criterion ctxt)) raw_criteria;

    val (opt_found, theorems) =
      filter_theorems ctxt (all_facts_of ctxt)
        {goal = opt_goal, limit = opt_limit, rem_dups = rem_dups, criteria = criteria};
    val returned = length theorems;

    val tally_msg =
      (case opt_found of
        NONE => "displaying " ^ string_of_int returned ^ " theorem(s)"
      | SOME found =>
          "found " ^ string_of_int found ^ " theorem(s)" ^
            (if returned < found
             then " (" ^ string_of_int returned ^ " displayed)"
             else ""));
    val position_markup = Position.markup (Position.thread_data ()) Markup.position;
  in
    Pretty.block
      (Pretty.fbreaks
        (Pretty.mark position_markup (Pretty.keyword1 "find_theorems") ::
          map (pretty_criterion ctxt) criteria)) ::
    Pretty.str "" ::
    (if null theorems then [Pretty.str "found nothing"]
     else
       Pretty.str (tally_msg ^ ":") ::
       grouped 10 Par_List.map (Pretty.item o single o pretty_thm ctxt) (rev theorems))
  end |> Pretty.fbreaks |> curry Pretty.blk 0;

end;



(** Isar command syntax **)

local

val criterion =
  Parse.reserved "name" |-- Parse.!!! (Parse.$$$ ":" |-- Parse.name) >> Name ||
  Parse.reserved "intro" >> K Intro ||
  Parse.reserved "elim" >> K Elim ||
  Parse.reserved "dest" >> K Dest ||
  Parse.reserved "solves" >> K Solves ||
  Parse.reserved "simp" |-- Parse.!!! (Parse.$$$ ":" |-- Parse.term) >> Simp ||
  Parse.term >> Pattern;

val query_keywords =
  Keyword.add_keywords [((":", @{here}), Keyword.no_spec)] Keyword.empty_keywords;

in

val query_parser = Scan.repeat ((Scan.option Parse.minus >> is_none) -- criterion);

fun read_query pos str =
  Token.explode query_keywords pos str
  |> filter Token.is_proper
  |> Scan.error (Scan.finite Token.stopper (Parse.!!! (query_parser --| Scan.ahead Parse.eof)))
  |> #1;

end;



(** PIDE query operation **)

fun proof_state st =
  (case try Toplevel.proof_of st of
    SOME state => state
  | NONE => Proof.init (Toplevel.context_of st));

val _ =
  Query_Operation.register {name = "find_theorems", pri = Task_Queue.urgent_pri}
    (fn {state = st, args, writeln_result, ...} =>
      if can Toplevel.context_of st then
        let
          val [limit_arg, allow_dups_arg, query_arg] = args;
          val state = proof_state st;
          val opt_limit = Int.fromString limit_arg;
          val rem_dups = allow_dups_arg = "false";
          val criteria = read_query Position.none query_arg;
        in writeln_result (Pretty.string_of (pretty_theorems state opt_limit rem_dups criteria)) end
      else error "Unknown context");

end;
