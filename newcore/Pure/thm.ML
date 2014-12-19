(*  Title:      Pure/thm.ML
    Author:     Lawrence C Paulson, Cambridge University Computer Laboratory
    Author:     Makarius

The very core of Isabelle's Meta Logic: certified types and terms,
derivations, theorems, framework rules (including lifting and
resolution), oracles.
*)

signature BASIC_THM =
  sig
  (*certified types*)
  type ctyp
  val rep_ctyp: ctyp -> {thy: theory, T: typ, maxidx: int, sorts: sort Ord_List.T}
  val theory_of_ctyp: ctyp -> theory
  val typ_of: ctyp -> typ
  val ctyp_of: theory -> typ -> ctyp

  (*certified terms*)
  type cterm
  exception CTERM of string * cterm list
  val rep_cterm: cterm -> {thy: theory, t: term, T: typ, maxidx: int, sorts: sort Ord_List.T}
  val crep_cterm: cterm -> {thy: theory, t: term, T: ctyp, maxidx: int, sorts: sort Ord_List.T}
  val theory_of_cterm: cterm -> theory
  val term_of: cterm -> term
  val cterm_of: theory -> term -> cterm
  val ctyp_of_term: cterm -> ctyp

  (*theorems*)
  type thm
  type conv = cterm -> thm
  val rep_thm: thm ->
   {thy: theory,
    tags: Properties.T,
    maxidx: int,
    shyps: sort Ord_List.T,
    hyps: term Ord_List.T,
    tpairs: (term * term) list,
    prop: term}
  val crep_thm: thm ->
   {thy: theory,
    tags: Properties.T,
    maxidx: int,
    shyps: sort Ord_List.T,
    hyps: cterm Ord_List.T,
    tpairs: (cterm * cterm) list,
    prop: cterm}
  exception THM of string * int * thm list
  val theory_of_thm: thm -> theory
  val prop_of: thm -> term
  val concl_of: thm -> term
  val prems_of: thm -> term list
  val nprems_of: thm -> int
  val cprop_of: thm -> cterm
  val cprem_of: thm -> int -> cterm
end;

signature THM =
sig
  include BASIC_THM
  val dest_ctyp: ctyp -> ctyp list
  val dest_comb: cterm -> cterm * cterm
  val dest_fun: cterm -> cterm
  val dest_arg: cterm -> cterm
  val dest_fun2: cterm -> cterm
  val dest_arg1: cterm -> cterm
  val dest_abs: string option -> cterm -> cterm * cterm
  val apply: cterm -> cterm -> cterm
  val lambda_name: string * cterm -> cterm -> cterm
  val lambda: cterm -> cterm -> cterm
  val adjust_maxidx_cterm: int -> cterm -> cterm
  val incr_indexes_cterm: int -> cterm -> cterm
  val match: cterm * cterm -> (ctyp * ctyp) list * (cterm * cterm) list
  val first_order_match: cterm * cterm -> (ctyp * ctyp) list * (cterm * cterm) list
  val fold_terms: (term -> 'a -> 'a) -> thm -> 'a -> 'a
  val terms_of_tpairs: (term * term) list -> term list
  val full_prop_of: thm -> term
  val maxidx_of: thm -> int
  val maxidx_thm: thm -> int -> int
  val hyps_of: thm -> term list
  val tpairs_of: thm -> (term * term) list
  val no_prems: thm -> bool
  val major_prem_of: thm -> term
  val transfer: theory -> thm -> thm
  val weaken: cterm -> thm -> thm
  val weaken_sorts: sort list -> cterm -> cterm
  val extra_shyps: thm -> sort list
  val proof_bodies_of: thm list -> proof_body list
  val proof_body_of: thm -> proof_body
  val proof_of: thm -> proof
  val join_proofs: thm list -> unit
  val peek_status: thm -> {oracle: bool, unfinished: bool, failed: bool}
  val future: thm future -> cterm -> thm
  val derivation_name: thm -> string
  val name_derivation: string -> thm -> thm
  val axiom: theory -> string -> thm
  val axioms_of: theory -> (string * thm) list
  val get_tags: thm -> Properties.T
  val map_tags: (Properties.T -> Properties.T) -> thm -> thm
  val norm_proof: thm -> thm
  val adjust_maxidx_thm: int -> thm -> thm
  (*meta rules*)
  val assume: cterm -> thm
  val implies_intr: cterm -> thm -> thm
  val implies_elim: thm -> thm -> thm
  val forall_intr: cterm -> thm -> thm
  val forall_elim: cterm -> thm -> thm
  val reflexive: cterm -> thm
  val symmetric: thm -> thm
  val transitive: thm -> thm -> thm
  val beta_conversion: bool -> conv
  val eta_conversion: conv
  val eta_long_conversion: conv
  val abstract_rule: string -> cterm -> thm -> thm
  val combination: thm -> thm -> thm
  val equal_intr: thm -> thm -> thm
  val equal_elim: thm -> thm -> thm
  val flexflex_rule: thm -> thm Seq.seq
  val generalize: string list * string list -> int -> thm -> thm
  val instantiate: (ctyp * ctyp) list * (cterm * cterm) list -> thm -> thm
  val instantiate_cterm: (ctyp * ctyp) list * (cterm * cterm) list -> cterm -> cterm
  val trivial: cterm -> thm
  val of_class: ctyp * class -> thm
  val strip_shyps: thm -> thm
  val unconstrainT: thm -> thm
  val varifyT_global': (string * sort) list -> thm -> ((string * sort) * indexname) list * thm
  val varifyT_global: thm -> thm
  val legacy_freezeT: thm -> thm
  val lift_rule: cterm -> thm -> thm
  val incr_indexes: int -> thm -> thm
  val assumption: int -> thm -> thm Seq.seq
  val eq_assumption: int -> thm -> thm
  val rotate_rule: int -> int -> thm -> thm
  val permute_prems: int -> int -> thm -> thm
  val rename_params_rule: string list * int -> thm -> thm
  val rename_boundvars: term -> term -> thm -> thm
  val bicompose: {flatten: bool, match: bool, incremented: bool} ->
    bool * thm * int -> int -> thm -> thm Seq.seq
  val biresolution: bool -> (bool * thm) list -> int -> thm -> thm Seq.seq
  val extern_oracles: Proof.context -> (Markup.T * xstring) list
  val add_oracle: binding * ('a -> cterm) -> theory -> (string * ('a -> thm)) * theory
end;

structure Thm: THM =
struct

(*** Certified terms and types ***)

(** certified types **)

abstype ctyp = Ctyp of {thy: theory, T: typ, maxidx: int, sorts: sort Ord_List.T}
with

fun rep_ctyp (Ctyp args) = args;
fun theory_of_ctyp (Ctyp {thy, ...}) = thy;
fun typ_of (Ctyp {T, ...}) = T;

fun ctyp_of thy raw_T =
  let
    val T = Sign.certify_typ thy raw_T;
    val maxidx = Term.maxidx_of_typ T;
    val sorts = Sorts.insert_typ T [];
  in Ctyp {thy = thy, T = T, maxidx = maxidx, sorts = sorts} end;

fun dest_ctyp (Ctyp {thy, T = Type (_, Ts), maxidx, sorts}) =
      map (fn T => Ctyp {thy = thy, T = T, maxidx = maxidx, sorts = sorts}) Ts
  | dest_ctyp cT = raise TYPE ("dest_ctyp", [typ_of cT], []);



(** certified terms **)

(*certified terms with checked typ, maxidx, and sorts*)
abstype cterm = Cterm of {thy: theory, t: term, T: typ, maxidx: int, sorts: sort Ord_List.T}
with

exception CTERM of string * cterm list;

fun rep_cterm (Cterm args) = args;

fun crep_cterm (Cterm {thy, t, T, maxidx, sorts}) =
  {thy = thy, t = t, maxidx = maxidx, sorts = sorts,
    T = Ctyp {thy = thy, T = T, maxidx = maxidx, sorts = sorts}};

fun theory_of_cterm (Cterm {thy, ...}) = thy;
fun term_of (Cterm {t, ...}) = t;

fun ctyp_of_term (Cterm {thy, T, maxidx, sorts, ...}) =
  Ctyp {thy = thy, T = T, maxidx = maxidx, sorts = sorts};

fun cterm_of thy tm =
  let
    val (t, T, maxidx) = Sign.certify_term thy tm;
    val sorts = Sorts.insert_term t [];
  in Cterm {thy = thy, t = t, T = T, maxidx = maxidx, sorts = sorts} end;

fun merge_thys0 (Cterm {thy = thy1, ...}) (Cterm {thy = thy2, ...}) =
  Theory.merge (thy1, thy2);


(* destructors *)

fun dest_comb (Cterm {t = c $ a, T, thy, maxidx, sorts}) =
      let val A = Term.argument_type_of c 0 in
        (Cterm {t = c, T = A --> T, thy = thy, maxidx = maxidx, sorts = sorts},
         Cterm {t = a, T = A, thy = thy, maxidx = maxidx, sorts = sorts})
      end
  | dest_comb ct = raise CTERM ("dest_comb", [ct]);

fun dest_fun (Cterm {t = c $ _, T, thy, maxidx, sorts}) =
      let val A = Term.argument_type_of c 0
      in Cterm {t = c, T = A --> T, thy = thy, maxidx = maxidx, sorts = sorts} end
  | dest_fun ct = raise CTERM ("dest_fun", [ct]);

fun dest_arg (Cterm {t = c $ a, T = _, thy, maxidx, sorts}) =
      let val A = Term.argument_type_of c 0
      in Cterm {t = a, T = A, thy = thy, maxidx = maxidx, sorts = sorts} end
  | dest_arg ct = raise CTERM ("dest_arg", [ct]);


fun dest_fun2 (Cterm {t = c $ _ $ _, T, thy, maxidx, sorts}) =
      let
        val A = Term.argument_type_of c 0;
        val B = Term.argument_type_of c 1;
      in Cterm {t = c, T = A --> B --> T, thy = thy, maxidx = maxidx, sorts = sorts} end
  | dest_fun2 ct = raise CTERM ("dest_fun2", [ct]);

fun dest_arg1 (Cterm {t = c $ a $ _, T = _, thy, maxidx, sorts}) =
      let val A = Term.argument_type_of c 0
      in Cterm {t = a, T = A, thy = thy, maxidx = maxidx, sorts = sorts} end
  | dest_arg1 ct = raise CTERM ("dest_arg1", [ct]);

fun dest_abs a (Cterm {t = Abs (x, T, t), T = Type ("fun", [_, U]), thy, maxidx, sorts}) =
      let val (y', t') = Term.dest_abs (the_default x a, T, t) in
        (Cterm {t = Free (y', T), T = T, thy = thy, maxidx = maxidx, sorts = sorts},
          Cterm {t = t', T = U, thy = thy, maxidx = maxidx, sorts = sorts})
      end
  | dest_abs _ ct = raise CTERM ("dest_abs", [ct]);


(* constructors *)

fun apply
  (cf as Cterm {t = f, T = Type ("fun", [dty, rty]), maxidx = maxidx1, sorts = sorts1, ...})
  (cx as Cterm {t = x, T, maxidx = maxidx2, sorts = sorts2, ...}) =
    if T = dty then
      Cterm {thy = merge_thys0 cf cx,
        t = f $ x,
        T = rty,
        maxidx = Int.max (maxidx1, maxidx2),
        sorts = Sorts.union sorts1 sorts2}
      else raise CTERM ("apply: types don't agree", [cf, cx])
  | apply cf cx = raise CTERM ("apply: first arg is not a function", [cf, cx]);

fun lambda_name
  (x, ct1 as Cterm {t = t1, T = T1, maxidx = maxidx1, sorts = sorts1, ...})
  (ct2 as Cterm {t = t2, T = T2, maxidx = maxidx2, sorts = sorts2, ...}) =
    let val t = Term.lambda_name (x, t1) t2 in
      Cterm {thy = merge_thys0 ct1 ct2,
        t = t, T = T1 --> T2,
        maxidx = Int.max (maxidx1, maxidx2),
        sorts = Sorts.union sorts1 sorts2}
    end;

fun lambda t u = lambda_name ("", t) u;


(* indexes *)

fun adjust_maxidx_cterm i (ct as Cterm {thy, t, T, maxidx, sorts}) =
  if maxidx = i then ct
  else if maxidx < i then
    Cterm {maxidx = i, thy = thy, t = t, T = T, sorts = sorts}
  else
    Cterm {maxidx = Int.max (maxidx_of_term t, i), thy = thy, t = t, T = T, sorts = sorts};

fun incr_indexes_cterm i (ct as Cterm {thy, t, T, maxidx, sorts}) =
  if i < 0 then raise CTERM ("negative increment", [ct])
  else if i = 0 then ct
  else Cterm {thy = thy, t = Logic.incr_indexes ([], i) t,
    T = Logic.incr_tvar i T, maxidx = maxidx + i, sorts = sorts};


(* matching *)

local

fun gen_match match
    (ct1 as Cterm {t = t1, sorts = sorts1, ...},
     ct2 as Cterm {t = t2, sorts = sorts2, maxidx = maxidx2, ...}) =
  let
    val thy = merge_thys0 ct1 ct2;
    val (Tinsts, tinsts) = match thy (t1, t2) (Vartab.empty, Vartab.empty);
    val sorts = Sorts.union sorts1 sorts2;
    fun mk_cTinst ((a, i), (S, T)) =
      (Ctyp {T = TVar ((a, i), S), thy = thy, maxidx = i, sorts = sorts},
       Ctyp {T = T, thy = thy, maxidx = maxidx2, sorts = sorts});
    fun mk_ctinst ((x, i), (T, t)) =
      let val T = Envir.subst_type Tinsts T in
        (Cterm {t = Var ((x, i), T), T = T, thy = thy, maxidx = i, sorts = sorts},
         Cterm {t = t, T = T, thy = thy, maxidx = maxidx2, sorts = sorts})
      end;
  in (Vartab.fold (cons o mk_cTinst) Tinsts [], Vartab.fold (cons o mk_ctinst) tinsts []) end;

in

val match = gen_match Pattern.match;
val first_order_match = gen_match Pattern.first_order_match;

end;



(*** Derivations and Theorems ***)

abstype thm = Thm of
 deriv *                        (*derivation*)
 {thy: theory,                  (*background theory*)
  tags: Properties.T,           (*additional annotations/comments*)
  maxidx: int,                  (*maximum index of any Var or TVar*)
  shyps: sort Ord_List.T,       (*sort hypotheses*)
  hyps: term Ord_List.T,        (*hypotheses*)
  tpairs: (term * term) list,   (*flex-flex pairs*)
  prop: term}                   (*conclusion*)
and deriv = Deriv of
 {promises: (serial * thm future) Ord_List.T,
  body: Proofterm.proof_body}
with

type conv = cterm -> thm;

(*errors involving theorems*)
exception THM of string * int * thm list;

fun rep_thm (Thm (_, args)) = args;

fun crep_thm (Thm (_, {thy, tags, maxidx, shyps, hyps, tpairs, prop})) =
  let fun cterm max t = Cterm {thy = thy, t = t, T = propT, maxidx = max, sorts = shyps} in
   {thy = thy, tags = tags, maxidx = maxidx, shyps = shyps,
    hyps = map (cterm ~1) hyps,
    tpairs = map (pairself (cterm maxidx)) tpairs,
    prop = cterm maxidx prop}
  end;

fun fold_terms f (Thm (_, {tpairs, prop, hyps, ...})) =
  fold (fn (t, u) => f t #> f u) tpairs #> f prop #> fold f hyps;

fun terms_of_tpairs tpairs = fold_rev (fn (t, u) => cons t o cons u) tpairs [];

fun eq_tpairs ((t, u), (t', u')) = t aconv t' andalso u aconv u';
fun union_tpairs ts us = Library.merge eq_tpairs (ts, us);
val maxidx_tpairs = fold (fn (t, u) => Term.maxidx_term t #> Term.maxidx_term u);

fun attach_tpairs tpairs prop =
  Logic.list_implies (map Logic.mk_equals tpairs, prop);

fun full_prop_of (Thm (_, {tpairs, prop, ...})) = attach_tpairs tpairs prop;

val union_hyps = Ord_List.union Term_Ord.fast_term_ord;
val insert_hyps = Ord_List.insert Term_Ord.fast_term_ord;
val remove_hyps = Ord_List.remove Term_Ord.fast_term_ord;


(* merge theories of cterms/thms -- trivial absorption only *)

fun merge_thys1 (Cterm {thy = thy1, ...}) (Thm (_, {thy = thy2, ...})) =
  Theory.merge (thy1, thy2);

fun merge_thys2 (Thm (_, {thy = thy1, ...})) (Thm (_, {thy = thy2, ...})) =
  Theory.merge (thy1, thy2);


(* basic components *)

val theory_of_thm = #thy o rep_thm;
val maxidx_of = #maxidx o rep_thm;
fun maxidx_thm th i = Int.max (maxidx_of th, i);
val hyps_of = #hyps o rep_thm;
val prop_of = #prop o rep_thm;
val tpairs_of = #tpairs o rep_thm;

val concl_of = Logic.strip_imp_concl o prop_of;
val prems_of = Logic.strip_imp_prems o prop_of;
val nprems_of = Logic.count_prems o prop_of;
fun no_prems th = nprems_of th = 0;

fun major_prem_of th =
  (case prems_of th of
    prem :: _ => Logic.strip_assums_concl prem
  | [] => raise THM ("major_prem_of: rule with no premises", 0, [th]));

(*the statement of any thm is a cterm*)
fun cprop_of (Thm (_, {thy, maxidx, shyps, prop, ...})) =
  Cterm {thy = thy, maxidx = maxidx, T = propT, t = prop, sorts = shyps};

fun cprem_of (th as Thm (_, {thy, maxidx, shyps, prop, ...})) i =
  Cterm {thy = thy, maxidx = maxidx, T = propT, sorts = shyps,
    t = Logic.nth_prem (i, prop) handle TERM _ => raise THM ("cprem_of", i, [th])};

(*explicit transfer to a super theory*)
fun transfer thy' thm =
  let
    val Thm (der, {thy, tags, maxidx, shyps, hyps, tpairs, prop}) = thm;
    val _ = Theory.subthy (thy, thy') orelse raise THM ("transfer: not a super theory", 0, [thm]);
  in
    if Theory.eq_thy (thy, thy') then thm
    else
      Thm (der,
       {thy = thy',
        tags = tags,
        maxidx = maxidx,
        shyps = shyps,
        hyps = hyps,
        tpairs = tpairs,
        prop = prop})
  end;

(*explicit weakening: maps |- B to A |- B*)
fun weaken raw_ct th =
  let
    val ct as Cterm {t = A, T, sorts, maxidx = maxidxA, ...} = adjust_maxidx_cterm ~1 raw_ct;
    val Thm (der, {tags, maxidx, shyps, hyps, tpairs, prop, ...}) = th;
  in
    if T <> propT then
      raise THM ("weaken: assumptions must have type prop", 0, [])
    else if maxidxA <> ~1 then
      raise THM ("weaken: assumptions may not contain schematic variables", maxidxA, [])
    else
      Thm (der,
       {thy = merge_thys1 ct th,
        tags = tags,
        maxidx = maxidx,
        shyps = Sorts.union sorts shyps,
        hyps = insert_hyps A hyps,
        tpairs = tpairs,
        prop = prop})
  end;

fun weaken_sorts raw_sorts ct =
  let
    val Cterm {thy, t, T, maxidx, sorts} = ct;
    val more_sorts = Sorts.make (map (Sign.certify_sort thy) raw_sorts);
    val sorts' = Sorts.union sorts more_sorts;
  in Cterm {thy = thy, t = t, T = T, maxidx = maxidx, sorts = sorts'} end;

(*dangling sort constraints of a thm*)
fun extra_shyps (th as Thm (_, {shyps, ...})) =
  Sorts.subtract (fold_terms Sorts.insert_term th []) shyps;



(** derivations and promised proofs **)

fun make_deriv promises oracles thms proof =
  Deriv {promises = promises, body = PBody {oracles = oracles, thms = thms, proof = proof}};

val empty_deriv = make_deriv [] [] [] Proofterm.MinProof;


(* inference rules *)

fun promise_ord ((i, _), (j, _)) = int_ord (j, i);

fun deriv_rule2 f
    (Deriv {promises = ps1, body = PBody {oracles = oras1, thms = thms1, proof = prf1}})
    (Deriv {promises = ps2, body = PBody {oracles = oras2, thms = thms2, proof = prf2}}) =
  let
    val ps = Ord_List.union promise_ord ps1 ps2;
    val oras = Proofterm.unions_oracles [oras1, oras2];
    val thms = Proofterm.unions_thms [thms1, thms2];
    val prf =
      (case ! Proofterm.proofs of
        2 => f prf1 prf2
      | 1 => MinProof
      | 0 => MinProof
      | i => error ("Illegal level of detail for proof objects: " ^ string_of_int i));
  in make_deriv ps oras thms prf end;

fun deriv_rule1 f = deriv_rule2 (K f) empty_deriv;
fun deriv_rule0 prf = deriv_rule1 I (make_deriv [] [] [] prf);

fun deriv_rule_unconditional f (Deriv {promises, body = PBody {oracles, thms, proof}}) =
  make_deriv promises oracles thms (f proof);


(* fulfilled proofs *)

fun raw_body_of (Thm (Deriv {body, ...}, _)) = body;
fun raw_promises_of (Thm (Deriv {promises, ...}, _)) = promises;

fun join_promises [] = ()
  | join_promises promises = join_promises_of (Future.joins (map snd promises))
and join_promises_of thms = join_promises (Ord_List.make promise_ord (maps raw_promises_of thms));

fun fulfill_body (Thm (Deriv {promises, body}, {thy, ...})) =
  Proofterm.fulfill_norm_proof thy (fulfill_promises promises) body
and fulfill_promises promises =
  map fst promises ~~ map fulfill_body (Future.joins (map snd promises));

fun proof_bodies_of thms =
  let
    val _ = join_promises_of thms;
    val bodies = map fulfill_body thms;
    val _ = Proofterm.join_bodies bodies;
  in bodies end;

val proof_body_of = singleton proof_bodies_of;
val proof_of = Proofterm.proof_of o proof_body_of;

val join_proofs = ignore o proof_bodies_of;


(* derivation status *)

fun peek_status (Thm (Deriv {promises, body}, _)) =
  let
    val ps = map (Future.peek o snd) promises;
    val bodies = body ::
      map_filter (fn SOME (Exn.Res th) => SOME (raw_body_of th) | _ => NONE) ps;
    val {oracle, unfinished, failed} = Proofterm.peek_status bodies;
  in
   {oracle = oracle,
    unfinished = unfinished orelse exists is_none ps,
    failed = failed orelse exists (fn SOME (Exn.Exn _) => true | _ => false) ps}
  end;


(* future rule *)

fun future_result i orig_thy orig_shyps orig_prop thm =
  let
    fun err msg = raise THM ("future_result: " ^ msg, 0, [thm]);
    val Thm (Deriv {promises, ...}, {thy, shyps, hyps, tpairs, prop, ...}) = thm;

    val _ = Theory.eq_thy (thy, orig_thy) orelse err "bad theory";
    val _ = prop aconv orig_prop orelse err "bad prop";
    val _ = null tpairs orelse err "bad tpairs";
    val _ = null hyps orelse err "bad hyps";
    val _ = Sorts.subset (shyps, orig_shyps) orelse err "bad shyps";
    val _ = forall (fn (j, _) => i <> j) promises orelse err "bad dependencies";
    val _ = join_promises promises;
  in thm end;

fun future future_thm ct =
  let
    val Cterm {thy = thy, t = prop, T, maxidx, sorts} = ct;
    val _ = T <> propT andalso raise CTERM ("future: prop expected", [ct]);

    val i = serial ();
    val future = future_thm |> Future.map (future_result i thy sorts prop);
  in
    Thm (make_deriv [(i, future)] [] [] (Proofterm.promise_proof thy i prop),
     {thy = thy,
      tags = [],
      maxidx = maxidx,
      shyps = sorts,
      hyps = [],
      tpairs = [],
      prop = prop})
  end;


(* closed derivations with official name *)

(*non-deterministic, depends on unknown promises*)
fun derivation_name (Thm (Deriv {body, ...}, {shyps, hyps, prop, ...})) =
  Proofterm.get_name shyps hyps prop (Proofterm.proof_of body);

fun name_derivation name (thm as Thm (der, args)) =
  let
    val Deriv {promises, body} = der;
    val {thy, shyps, hyps, prop, tpairs, ...} = args;
    val _ = null tpairs orelse raise THM ("put_name: unsolved flex-flex constraints", 0, [thm]);

    val ps = map (apsnd (Future.map fulfill_body)) promises;
    val (pthm, proof) = Proofterm.thm_proof thy name shyps hyps prop ps body;
    val der' = make_deriv [] [] [pthm] proof;
  in Thm (der', args) end;



(** Axioms **)

fun axiom theory name =
  let
    fun get_ax thy =
      Name_Space.lookup_key (Theory.axiom_table thy) name
      |> Option.map (fn (_, prop) =>
           let
             val der = deriv_rule0 (Proofterm.axm_proof name prop);
             val maxidx = maxidx_of_term prop;
             val shyps = Sorts.insert_term prop [];
           in
             Thm (der, {thy = thy, tags = [],
               maxidx = maxidx, shyps = shyps, hyps = [], tpairs = [], prop = prop})
           end);
  in
    (case get_first get_ax (Theory.nodes_of theory) of
      SOME thm => thm
    | NONE => raise THEORY ("No axiom " ^ quote name, [theory]))
  end;

(*return additional axioms of this theory node*)
fun axioms_of thy =
  map (fn (name, _) => (name, axiom thy name)) (Theory.axioms_of thy);


(* tags *)

val get_tags = #tags o rep_thm;

fun map_tags f (Thm (der, {thy, tags, maxidx, shyps, hyps, tpairs, prop})) =
  Thm (der, {thy = thy, tags = f tags, maxidx = maxidx,
    shyps = shyps, hyps = hyps, tpairs = tpairs, prop = prop});


(* technical adjustments *)

fun norm_proof (Thm (der, args as {thy, ...})) =
  Thm (deriv_rule1 (Proofterm.rew_proof thy) der, args);

fun adjust_maxidx_thm i (th as Thm (der, {thy, tags, maxidx, shyps, hyps, tpairs, prop})) =
  if maxidx = i then th
  else if maxidx < i then
    Thm (der, {maxidx = i, thy = thy, tags = tags, shyps = shyps,
      hyps = hyps, tpairs = tpairs, prop = prop})
  else
    Thm (der, {maxidx = Int.max (maxidx_tpairs tpairs (maxidx_of_term prop), i), thy = thy,
      tags = tags, shyps = shyps, hyps = hyps, tpairs = tpairs, prop = prop});



(*** Meta rules ***)

(** primitive rules **)

(*The assumption rule A |- A*)
fun assume raw_ct =
  let val Cterm {thy, t = prop, T, maxidx, sorts} = adjust_maxidx_cterm ~1 raw_ct in
    if T <> propT then
      raise THM ("assume: prop", 0, [])
    else if maxidx <> ~1 then
      raise THM ("assume: variables", maxidx, [])
    else Thm (deriv_rule0 (Proofterm.Hyp prop),
     {thy = thy,
      tags = [],
      maxidx = ~1,
      shyps = sorts,
      hyps = [prop],
      tpairs = [],
      prop = prop})
  end;

(*Implication introduction
    [A]
     :
     B
  -------
  A ==> B
*)
fun implies_intr
    (ct as Cterm {t = A, T, maxidx = maxidxA, sorts, ...})
    (th as Thm (der, {maxidx, hyps, shyps, tpairs, prop, ...})) =
  if T <> propT then
    raise THM ("implies_intr: assumptions must have type prop", 0, [th])
  else
    Thm (deriv_rule1 (Proofterm.implies_intr_proof A) der,
     {thy = merge_thys1 ct th,
      tags = [],
      maxidx = Int.max (maxidxA, maxidx),
      shyps = Sorts.union sorts shyps,
      hyps = remove_hyps A hyps,
      tpairs = tpairs,
      prop = Logic.mk_implies (A, prop)});


(*Implication elimination
  A ==> B    A
  ------------
        B
*)
fun implies_elim thAB thA =
  let
    val Thm (derA, {maxidx = maxA, hyps = hypsA, shyps = shypsA, tpairs = tpairsA,
      prop = propA, ...}) = thA
    and Thm (der, {maxidx, hyps, shyps, tpairs, prop, ...}) = thAB;
    fun err () = raise THM ("implies_elim: major premise", 0, [thAB, thA]);
  in
    case prop of
      Const ("Pure.imp", _) $ A $ B =>
        if A aconv propA then
          Thm (deriv_rule2 (curry Proofterm.%%) der derA,
           {thy = merge_thys2 thAB thA,
            tags = [],
            maxidx = Int.max (maxA, maxidx),
            shyps = Sorts.union shypsA shyps,
            hyps = union_hyps hypsA hyps,
            tpairs = union_tpairs tpairsA tpairs,
            prop = B})
        else err ()
    | _ => err ()
  end;

(*Forall introduction.  The Free or Var x must not be free in the hypotheses.
    [x]
     :
     A
  ------
  !!x. A
*)
fun forall_intr
    (ct as Cterm {t = x, T, sorts, ...})
    (th as Thm (der, {maxidx, shyps, hyps, tpairs, prop, ...})) =
  let
    fun result a =
      Thm (deriv_rule1 (Proofterm.forall_intr_proof x a) der,
       {thy = merge_thys1 ct th,
        tags = [],
        maxidx = maxidx,
        shyps = Sorts.union sorts shyps,
        hyps = hyps,
        tpairs = tpairs,
        prop = Logic.all_const T $ Abs (a, T, abstract_over (x, prop))});
    fun check_occs a x ts =
      if exists (fn t => Logic.occs (x, t)) ts then
        raise THM ("forall_intr: variable " ^ quote a ^ " free in assumptions", 0, [th])
      else ();
  in
    (case x of
      Free (a, _) => (check_occs a x hyps; check_occs a x (terms_of_tpairs tpairs); result a)
    | Var ((a, _), _) => (check_occs a x (terms_of_tpairs tpairs); result a)
    | _ => raise THM ("forall_intr: not a variable", 0, [th]))
  end;

(*Forall elimination
  !!x. A
  ------
  A[t/x]
*)
fun forall_elim
    (ct as Cterm {t, T, maxidx = maxt, sorts, ...})
    (th as Thm (der, {maxidx, shyps, hyps, tpairs, prop, ...})) =
  (case prop of
    Const ("Pure.all", Type ("fun", [Type ("fun", [qary, _]), _])) $ A =>
      if T <> qary then
        raise THM ("forall_elim: type mismatch", 0, [th])
      else
        Thm (deriv_rule1 (Proofterm.% o rpair (SOME t)) der,
         {thy = merge_thys1 ct th,
          tags = [],
          maxidx = Int.max (maxidx, maxt),
          shyps = Sorts.union sorts shyps,
          hyps = hyps,
          tpairs = tpairs,
          prop = Term.betapply (A, t)})
  | _ => raise THM ("forall_elim: not quantified", 0, [th]));


(* Equality *)

(*Reflexivity
  t == t
*)
fun reflexive (Cterm {thy, t, T = _, maxidx, sorts}) =
  Thm (deriv_rule0 Proofterm.reflexive,
   {thy = thy,
    tags = [],
    maxidx = maxidx,
    shyps = sorts,
    hyps = [],
    tpairs = [],
    prop = Logic.mk_equals (t, t)});

(*Symmetry
  t == u
  ------
  u == t
*)
fun symmetric (th as Thm (der, {thy, maxidx, shyps, hyps, tpairs, prop, ...})) =
  (case prop of
    (eq as Const ("Pure.eq", _)) $ t $ u =>
      Thm (deriv_rule1 Proofterm.symmetric der,
       {thy = thy,
        tags = [],
        maxidx = maxidx,
        shyps = shyps,
        hyps = hyps,
        tpairs = tpairs,
        prop = eq $ u $ t})
    | _ => raise THM ("symmetric", 0, [th]));

(*Transitivity
  t1 == u    u == t2
  ------------------
       t1 == t2
*)
fun transitive th1 th2 =
  let
    val Thm (der1, {maxidx = max1, hyps = hyps1, shyps = shyps1, tpairs = tpairs1,
      prop = prop1, ...}) = th1
    and Thm (der2, {maxidx = max2, hyps = hyps2, shyps = shyps2, tpairs = tpairs2,
      prop = prop2, ...}) = th2;
    fun err msg = raise THM ("transitive: " ^ msg, 0, [th1, th2]);
  in
    case (prop1, prop2) of
      ((eq as Const ("Pure.eq", Type (_, [T, _]))) $ t1 $ u, Const ("Pure.eq", _) $ u' $ t2) =>
        if not (u aconv u') then err "middle term"
        else
          Thm (deriv_rule2 (Proofterm.transitive u T) der1 der2,
           {thy = merge_thys2 th1 th2,
            tags = [],
            maxidx = Int.max (max1, max2),
            shyps = Sorts.union shyps1 shyps2,
            hyps = union_hyps hyps1 hyps2,
            tpairs = union_tpairs tpairs1 tpairs2,
            prop = eq $ t1 $ t2})
     | _ =>  err "premises"
  end;

(*Beta-conversion
  (%x. t)(u) == t[u/x]
  fully beta-reduces the term if full = true
*)
fun beta_conversion full (Cterm {thy, t, T = _, maxidx, sorts}) =
  let val t' =
    if full then Envir.beta_norm t
    else
      (case t of Abs (_, _, bodt) $ u => subst_bound (u, bodt)
      | _ => raise THM ("beta_conversion: not a redex", 0, []));
  in
    Thm (deriv_rule0 Proofterm.reflexive,
     {thy = thy,
      tags = [],
      maxidx = maxidx,
      shyps = sorts,
      hyps = [],
      tpairs = [],
      prop = Logic.mk_equals (t, t')})
  end;

fun eta_conversion (Cterm {thy, t, T = _, maxidx, sorts}) =
  Thm (deriv_rule0 Proofterm.reflexive,
   {thy = thy,
    tags = [],
    maxidx = maxidx,
    shyps = sorts,
    hyps = [],
    tpairs = [],
    prop = Logic.mk_equals (t, Envir.eta_contract t)});

fun eta_long_conversion (Cterm {thy, t, T = _, maxidx, sorts}) =
  Thm (deriv_rule0 Proofterm.reflexive,
   {thy = thy,
    tags = [],
    maxidx = maxidx,
    shyps = sorts,
    hyps = [],
    tpairs = [],
    prop = Logic.mk_equals (t, Envir.eta_long [] t)});

(*The abstraction rule.  The Free or Var x must not be free in the hypotheses.
  The bound variable will be named "a" (since x will be something like x320)
      t == u
  --------------
  %x. t == %x. u
*)
fun abstract_rule a
    (Cterm {t = x, T, sorts, ...})
    (th as Thm (der, {thy, maxidx, hyps, shyps, tpairs, prop, ...})) =
  let
    val (t, u) = Logic.dest_equals prop
      handle TERM _ => raise THM ("abstract_rule: premise not an equality", 0, [th]);
    val result =
      Thm (deriv_rule1 (Proofterm.abstract_rule x a) der,
       {thy = thy,
        tags = [],
        maxidx = maxidx,
        shyps = Sorts.union sorts shyps,
        hyps = hyps,
        tpairs = tpairs,
        prop = Logic.mk_equals
          (Abs (a, T, abstract_over (x, t)), Abs (a, T, abstract_over (x, u)))});
    fun check_occs a x ts =
      if exists (fn t => Logic.occs (x, t)) ts then
        raise THM ("abstract_rule: variable " ^ quote a ^ " free in assumptions", 0, [th])
      else ();
  in
    (case x of
      Free (a, _) => (check_occs a x hyps; check_occs a x (terms_of_tpairs tpairs); result)
    | Var ((a, _), _) => (check_occs a x (terms_of_tpairs tpairs); result)
    | _ => raise THM ("abstract_rule: not a variable", 0, [th]))
  end;

(*The combination rule
  f == g  t == u
  --------------
    f t == g u
*)
fun combination th1 th2 =
  let
    val Thm (der1, {maxidx = max1, shyps = shyps1, hyps = hyps1, tpairs = tpairs1,
      prop = prop1, ...}) = th1
    and Thm (der2, {maxidx = max2, shyps = shyps2, hyps = hyps2, tpairs = tpairs2,
      prop = prop2, ...}) = th2;
    fun chktypes fT tT =
      (case fT of
        Type ("fun", [T1, _]) =>
          if T1 <> tT then
            raise THM ("combination: types", 0, [th1, th2])
          else ()
      | _ => raise THM ("combination: not function type", 0, [th1, th2]));
  in
    (case (prop1, prop2) of
      (Const ("Pure.eq", Type ("fun", [fT, _])) $ f $ g,
       Const ("Pure.eq", Type ("fun", [tT, _])) $ t $ u) =>
        (chktypes fT tT;
          Thm (deriv_rule2 (Proofterm.combination f g t u fT) der1 der2,
           {thy = merge_thys2 th1 th2,
            tags = [],
            maxidx = Int.max (max1, max2),
            shyps = Sorts.union shyps1 shyps2,
            hyps = union_hyps hyps1 hyps2,
            tpairs = union_tpairs tpairs1 tpairs2,
            prop = Logic.mk_equals (f $ t, g $ u)}))
     | _ => raise THM ("combination: premises", 0, [th1, th2]))
  end;

(*Equality introduction
  A ==> B  B ==> A
  ----------------
       A == B
*)
fun equal_intr th1 th2 =
  let
    val Thm (der1, {maxidx = max1, shyps = shyps1, hyps = hyps1, tpairs = tpairs1,
      prop = prop1, ...}) = th1
    and Thm (der2, {maxidx = max2, shyps = shyps2, hyps = hyps2, tpairs = tpairs2,
      prop = prop2, ...}) = th2;
    fun err msg = raise THM ("equal_intr: " ^ msg, 0, [th1, th2]);
  in
    (case (prop1, prop2) of
      (Const("Pure.imp", _) $ A $ B, Const("Pure.imp", _) $ B' $ A') =>
        if A aconv A' andalso B aconv B' then
          Thm (deriv_rule2 (Proofterm.equal_intr A B) der1 der2,
           {thy = merge_thys2 th1 th2,
            tags = [],
            maxidx = Int.max (max1, max2),
            shyps = Sorts.union shyps1 shyps2,
            hyps = union_hyps hyps1 hyps2,
            tpairs = union_tpairs tpairs1 tpairs2,
            prop = Logic.mk_equals (A, B)})
        else err "not equal"
    | _ =>  err "premises")
  end;

(*The equal propositions rule
  A == B  A
  ---------
      B
*)
fun equal_elim th1 th2 =
  let
    val Thm (der1, {maxidx = max1, shyps = shyps1, hyps = hyps1,
      tpairs = tpairs1, prop = prop1, ...}) = th1
    and Thm (der2, {maxidx = max2, shyps = shyps2, hyps = hyps2,
      tpairs = tpairs2, prop = prop2, ...}) = th2;
    fun err msg = raise THM ("equal_elim: " ^ msg, 0, [th1, th2]);
  in
    (case prop1 of
      Const ("Pure.eq", _) $ A $ B =>
        if prop2 aconv A then
          Thm (deriv_rule2 (Proofterm.equal_elim A B) der1 der2,
           {thy = merge_thys2 th1 th2,
            tags = [],
            maxidx = Int.max (max1, max2),
            shyps = Sorts.union shyps1 shyps2,
            hyps = union_hyps hyps1 hyps2,
            tpairs = union_tpairs tpairs1 tpairs2,
            prop = B})
        else err "not equal"
     | _ =>  err "major premise")
  end;



(**** Derived rules ****)

(*Smash unifies the list of term pairs leaving no flex-flex pairs.
  Instantiates the theorem and deletes trivial tpairs.  Resulting
  sequence may contain multiple elements if the tpairs are not all
  flex-flex.*)
fun flexflex_rule (th as Thm (der, {thy, maxidx, shyps, hyps, tpairs, prop, ...})) =
  Unify.smash_unifiers thy tpairs (Envir.empty maxidx)
  |> Seq.map (fn env =>
      if Envir.is_empty env then th
      else
        let
          val tpairs' = tpairs |> map (pairself (Envir.norm_term env))
            (*remove trivial tpairs, of the form t==t*)
            |> filter_out (op aconv);
          val der' = deriv_rule1 (Proofterm.norm_proof' env) der;
          val prop' = Envir.norm_term env prop;
          val maxidx = maxidx_tpairs tpairs' (maxidx_of_term prop');
          val shyps = Envir.insert_sorts env shyps;
        in
          Thm (der', {thy = thy, tags = [], maxidx = maxidx,
            shyps = shyps, hyps = hyps, tpairs = tpairs', prop = prop'})
        end);


(*Generalization of fixed variables
           A
  --------------------
  A[?'a/'a, ?x/x, ...]
*)

fun generalize ([], []) _ th = th
  | generalize (tfrees, frees) idx th =
      let
        val Thm (der, {thy, maxidx, shyps, hyps, tpairs, prop, ...}) = th;
        val _ = idx <= maxidx andalso raise THM ("generalize: bad index", idx, [th]);

        val bad_type =
          if null tfrees then K false
          else Term.exists_subtype (fn TFree (a, _) => member (op =) tfrees a | _ => false);
        fun bad_term (Free (x, T)) = bad_type T orelse member (op =) frees x
          | bad_term (Var (_, T)) = bad_type T
          | bad_term (Const (_, T)) = bad_type T
          | bad_term (Abs (_, T, t)) = bad_type T orelse bad_term t
          | bad_term (t $ u) = bad_term t orelse bad_term u
          | bad_term (Bound _) = false;
        val _ = exists bad_term hyps andalso
          raise THM ("generalize: variable free in assumptions", 0, [th]);

        val gen = Term_Subst.generalize (tfrees, frees) idx;
        val prop' = gen prop;
        val tpairs' = map (pairself gen) tpairs;
        val maxidx' = maxidx_tpairs tpairs' (maxidx_of_term prop');
      in
        Thm (deriv_rule1 (Proofterm.generalize (tfrees, frees) idx) der,
         {thy = thy,
          tags = [],
          maxidx = maxidx',
          shyps = shyps,
          hyps = hyps,
          tpairs = tpairs',
          prop = prop'})
      end;


(*Instantiation of schematic variables
           A
  --------------------
  A[t1/v1, ..., tn/vn]
*)

local

fun pretty_typing thy t T = Pretty.block
  [Syntax.pretty_term_global thy t, Pretty.str " ::", Pretty.brk 1, Syntax.pretty_typ_global thy T];

fun add_inst (ct, cu) (thy, sorts) =
  let
    val Cterm {t = t, T = T, ...} = ct;
    val Cterm {t = u, T = U, sorts = sorts_u, maxidx = maxidx_u, ...} = cu;
    val thy' = Theory.merge (thy, merge_thys0 ct cu);
    val sorts' = Sorts.union sorts_u sorts;
  in
    (case t of Var v =>
      if T = U then ((v, (u, maxidx_u)), (thy', sorts'))
      else raise TYPE (Pretty.string_of (Pretty.block
       [Pretty.str "instantiate: type conflict",
        Pretty.fbrk, pretty_typing thy' t T,
        Pretty.fbrk, pretty_typing thy' u U]), [T, U], [t, u])
    | _ => raise TYPE (Pretty.string_of (Pretty.block
       [Pretty.str "instantiate: not a variable",
        Pretty.fbrk, Syntax.pretty_term_global thy' t]), [], [t]))
  end;

fun add_instT (cT, cU) (thy, sorts) =
  let
    val Ctyp {T, thy = thy1, ...} = cT
    and Ctyp {T = U, thy = thy2, sorts = sorts_U, maxidx = maxidx_U, ...} = cU;
    val thy' = Theory.merge (thy, Theory.merge (thy1, thy2));
    val sorts' = Sorts.union sorts_U sorts;
  in
    (case T of TVar (v as (_, S)) =>
      if Sign.of_sort thy' (U, S) then ((v, (U, maxidx_U)), (thy', sorts'))
      else raise TYPE ("Type not of sort " ^ Syntax.string_of_sort_global thy' S, [U], [])
    | _ => raise TYPE (Pretty.string_of (Pretty.block
        [Pretty.str "instantiate: not a type variable",
         Pretty.fbrk, Syntax.pretty_typ_global thy' T]), [T], []))
  end;

in

(*Left-to-right replacements: ctpairs = [..., (vi, ti), ...].
  Instantiates distinct Vars by terms of same type.
  Does NOT normalize the resulting theorem!*)
fun instantiate ([], []) th = th
  | instantiate (instT, inst) th =
      let
        val Thm (der, {thy, hyps, shyps, tpairs, prop, ...}) = th;
        val (inst', (instT', (thy', shyps'))) =
          (thy, shyps) |> fold_map add_inst inst ||> fold_map add_instT instT;
        val subst = Term_Subst.instantiate_maxidx (instT', inst');
        val (prop', maxidx1) = subst prop ~1;
        val (tpairs', maxidx') =
          fold_map (fn (t, u) => fn i => subst t i ||>> subst u) tpairs maxidx1;
      in
        Thm (deriv_rule1
          (fn d => Proofterm.instantiate (map (apsnd #1) instT', map (apsnd #1) inst') d) der,
         {thy = thy',
          tags = [],
          maxidx = maxidx',
          shyps = shyps',
          hyps = hyps,
          tpairs = tpairs',
          prop = prop'})
      end
      handle TYPE (msg, _, _) => raise THM (msg, 0, [th]);

fun instantiate_cterm ([], []) ct = ct
  | instantiate_cterm (instT, inst) ct =
      let
        val Cterm {thy, t, T, sorts, ...} = ct;
        val (inst', (instT', (thy', sorts'))) =
          (thy, sorts) |> fold_map add_inst inst ||> fold_map add_instT instT;
        val subst = Term_Subst.instantiate_maxidx (instT', inst');
        val substT = Term_Subst.instantiateT_maxidx instT';
        val (t', maxidx1) = subst t ~1;
        val (T', maxidx') = substT T maxidx1;
      in Cterm {thy = thy', t = t', T = T', sorts = sorts', maxidx = maxidx'} end
      handle TYPE (msg, _, _) => raise CTERM (msg, [ct]);

end;


(*The trivial implication A ==> A, justified by assume and forall rules.
  A can contain Vars, not so for assume!*)
fun trivial (Cterm {thy, t = A, T, maxidx, sorts}) =
  if T <> propT then
    raise THM ("trivial: the term must have type prop", 0, [])
  else
    Thm (deriv_rule0 (Proofterm.AbsP ("H", NONE, Proofterm.PBound 0)),
     {thy = thy,
      tags = [],
      maxidx = maxidx,
      shyps = sorts,
      hyps = [],
      tpairs = [],
      prop = Logic.mk_implies (A, A)});

(*Axiom-scheme reflecting signature contents
        T :: c
  -------------------
  OFCLASS(T, c_class)
*)
fun of_class (cT, raw_c) =
  let
    val Ctyp {thy, T, ...} = cT;
    val c = Sign.certify_class thy raw_c;
    val Cterm {t = prop, maxidx, sorts, ...} = cterm_of thy (Logic.mk_of_class (T, c));
  in
    if Sign.of_sort thy (T, [c]) then
      Thm (deriv_rule0 (Proofterm.OfClass (T, c)),
       {thy = thy,
        tags = [],
        maxidx = maxidx,
        shyps = sorts,
        hyps = [],
        tpairs = [],
        prop = prop})
    else raise THM ("of_class: type not of class " ^ Syntax.string_of_sort_global thy [c], 0, [])
  end;

(*Remove extra sorts that are witnessed by type signature information*)
fun strip_shyps (thm as Thm (_, {shyps = [], ...})) = thm
  | strip_shyps (thm as Thm (der, {thy, tags, maxidx, shyps, hyps, tpairs, prop})) =
      let
        val algebra = Sign.classes_of thy;

        val present = (fold_terms o fold_types o fold_atyps_sorts) (insert (eq_fst op =)) thm [];
        val extra = fold (Sorts.remove_sort o #2) present shyps;
        val witnessed = Sign.witness_sorts thy present extra;
        val extra' = fold (Sorts.remove_sort o #2) witnessed extra
          |> Sorts.minimal_sorts algebra;
        val shyps' = fold (Sorts.insert_sort o #2) present extra';
      in
        Thm (deriv_rule_unconditional
          (Proofterm.strip_shyps_proof algebra present witnessed extra') der,
         {thy = thy, tags = tags, maxidx = maxidx,
          shyps = shyps', hyps = hyps, tpairs = tpairs, prop = prop})
      end;

(*Internalize sort constraints of type variables*)
fun unconstrainT (thm as Thm (der, args)) =
  let
    val Deriv {promises, body} = der;
    val {thy, shyps, hyps, tpairs, prop, ...} = args;

    fun err msg = raise THM ("unconstrainT: " ^ msg, 0, [thm]);
    val _ = null hyps orelse err "illegal hyps";
    val _ = null tpairs orelse err "unsolved flex-flex constraints";
    val tfrees = rev (Term.add_tfree_names prop []);
    val _ = null tfrees orelse err ("illegal free type variables " ^ commas_quote tfrees);

    val ps = map (apsnd (Future.map fulfill_body)) promises;
    val (pthm as (_, (_, prop', _)), proof) =
      Proofterm.unconstrain_thm_proof thy shyps prop ps body;
    val der' = make_deriv [] [] [pthm] proof;
  in
    Thm (der',
     {thy = thy,
      tags = [],
      maxidx = maxidx_of_term prop',
      shyps = [[]],  (*potentially redundant*)
      hyps = [],
      tpairs = [],
      prop = prop'})
  end;

(* Replace all TFrees not fixed or in the hyps by new TVars *)
fun varifyT_global' fixed (Thm (der, {thy, maxidx, shyps, hyps, tpairs, prop, ...})) =
  let
    val tfrees = fold Term.add_tfrees hyps fixed;
    val prop1 = attach_tpairs tpairs prop;
    val (al, prop2) = Type.varify_global tfrees prop1;
    val (ts, prop3) = Logic.strip_prems (length tpairs, [], prop2);
  in
    (al, Thm (deriv_rule1 (Proofterm.varify_proof prop tfrees) der,
     {thy = thy,
      tags = [],
      maxidx = Int.max (0, maxidx),
      shyps = shyps,
      hyps = hyps,
      tpairs = rev (map Logic.dest_equals ts),
      prop = prop3}))
  end;

val varifyT_global = #2 o varifyT_global' [];

(* Replace all TVars by TFrees that are often new *)
fun legacy_freezeT (Thm (der, {thy, shyps, hyps, tpairs, prop, ...})) =
  let
    val prop1 = attach_tpairs tpairs prop;
    val prop2 = Type.legacy_freeze prop1;
    val (ts, prop3) = Logic.strip_prems (length tpairs, [], prop2);
  in
    Thm (deriv_rule1 (Proofterm.legacy_freezeT prop1) der,
     {thy = thy,
      tags = [],
      maxidx = maxidx_of_term prop2,
      shyps = shyps,
      hyps = hyps,
      tpairs = rev (map Logic.dest_equals ts),
      prop = prop3})
  end;


(*** Inference rules for tactics ***)

(*Destruct proof state into constraints, other goals, goal(i), rest *)
fun dest_state (state as Thm (_, {prop,tpairs,...}), i) =
  (case  Logic.strip_prems(i, [], prop) of
      (B::rBs, C) => (tpairs, rev rBs, B, C)
    | _ => raise THM("dest_state", i, [state]))
  handle TERM _ => raise THM("dest_state", i, [state]);

(*Prepare orule for resolution by lifting it over the parameters and
assumptions of goal.*)
fun lift_rule goal orule =
  let
    val Cterm {t = gprop, T, maxidx = gmax, sorts, ...} = goal;
    val inc = gmax + 1;
    val lift_abs = Logic.lift_abs inc gprop;
    val lift_all = Logic.lift_all inc gprop;
    val Thm (der, {maxidx, shyps, hyps, tpairs, prop, ...}) = orule;
    val (As, B) = Logic.strip_horn prop;
  in
    if T <> propT then raise THM ("lift_rule: the term must have type prop", 0, [])
    else
      Thm (deriv_rule1 (Proofterm.lift_proof gprop inc prop) der,
       {thy = merge_thys1 goal orule,
        tags = [],
        maxidx = maxidx + inc,
        shyps = Sorts.union shyps sorts,  (*sic!*)
        hyps = hyps,
        tpairs = map (pairself lift_abs) tpairs,
        prop = Logic.list_implies (map lift_all As, lift_all B)})
  end;

fun incr_indexes i (thm as Thm (der, {thy, maxidx, shyps, hyps, tpairs, prop, ...})) =
  if i < 0 then raise THM ("negative increment", 0, [thm])
  else if i = 0 then thm
  else
    Thm (deriv_rule1 (Proofterm.incr_indexes i) der,
     {thy = thy,
      tags = [],
      maxidx = maxidx + i,
      shyps = shyps,
      hyps = hyps,
      tpairs = map (pairself (Logic.incr_indexes ([], i))) tpairs,
      prop = Logic.incr_indexes ([], i) prop});

(*Solve subgoal Bi of proof state B1...Bn/C by assumption. *)
fun assumption i state =
  let
    val Thm (der, {thy, maxidx, shyps, hyps, ...}) = state;
    val (tpairs, Bs, Bi, C) = dest_state (state, i);
    fun newth n (env, tpairs) =
      Thm (deriv_rule1
          ((if Envir.is_empty env then I else (Proofterm.norm_proof' env)) o
            Proofterm.assumption_proof Bs Bi n) der,
       {tags = [],
        maxidx = Envir.maxidx_of env,
        shyps = Envir.insert_sorts env shyps,
        hyps = hyps,
        tpairs =
          if Envir.is_empty env then tpairs
          else map (pairself (Envir.norm_term env)) tpairs,
        prop =
          if Envir.is_empty env then (*avoid wasted normalizations*)
            Logic.list_implies (Bs, C)
          else (*normalize the new rule fully*)
            Envir.norm_term env (Logic.list_implies (Bs, C)),
        thy = thy});

    val (close, asms, concl) = Logic.assum_problems (~1, Bi);
    val concl' = close concl;
    fun addprfs [] _ = Seq.empty
      | addprfs (asm :: rest) n = Seq.make (fn () => Seq.pull
          (Seq.mapp (newth n)
            (if Term.could_unify (asm, concl) then
              (Unify.unifiers (thy, Envir.empty maxidx, (close asm, concl') :: tpairs))
             else Seq.empty)
            (addprfs rest (n + 1))))
  in addprfs asms 1 end;

(*Solve subgoal Bi of proof state B1...Bn/C by assumption.
  Checks if Bi's conclusion is alpha/eta-convertible to one of its assumptions*)
fun eq_assumption i state =
  let
    val Thm (der, {thy, maxidx, shyps, hyps, ...}) = state;
    val (tpairs, Bs, Bi, C) = dest_state (state, i);
    val (_, asms, concl) = Logic.assum_problems (~1, Bi);
  in
    (case find_index (fn asm => Envir.aeconv (asm, concl)) asms of
      ~1 => raise THM ("eq_assumption", 0, [state])
    | n =>
        Thm (deriv_rule1 (Proofterm.assumption_proof Bs Bi (n + 1)) der,
         {thy = thy,
          tags = [],
          maxidx = maxidx,
          shyps = shyps,
          hyps = hyps,
          tpairs = tpairs,
          prop = Logic.list_implies (Bs, C)}))
  end;


(*For rotate_tac: fast rotation of assumptions of subgoal i*)
fun rotate_rule k i state =
  let
    val Thm (der, {thy, maxidx, shyps, hyps, ...}) = state;
    val (tpairs, Bs, Bi, C) = dest_state (state, i);
    val params = Term.strip_all_vars Bi;
    val rest = Term.strip_all_body Bi;
    val asms = Logic.strip_imp_prems rest
    val concl = Logic.strip_imp_concl rest;
    val n = length asms;
    val m = if k < 0 then n + k else k;
    val Bi' =
      if 0 = m orelse m = n then Bi
      else if 0 < m andalso m < n then
        let val (ps, qs) = chop m asms
        in Logic.list_all (params, Logic.list_implies (qs @ ps, concl)) end
      else raise THM ("rotate_rule", k, [state]);
  in
    Thm (deriv_rule1 (Proofterm.rotate_proof Bs Bi m) der,
     {thy = thy,
      tags = [],
      maxidx = maxidx,
      shyps = shyps,
      hyps = hyps,
      tpairs = tpairs,
      prop = Logic.list_implies (Bs @ [Bi'], C)})
  end;


(*Rotates a rule's premises to the left by k, leaving the first j premises
  unchanged.  Does nothing if k=0 or if k equals n-j, where n is the
  number of premises.  Useful with etac and underlies defer_tac*)
fun permute_prems j k rl =
  let
    val Thm (der, {thy, maxidx, shyps, hyps, tpairs, prop, ...}) = rl;
    val prems = Logic.strip_imp_prems prop
    and concl = Logic.strip_imp_concl prop;
    val moved_prems = List.drop (prems, j)
    and fixed_prems = List.take (prems, j)
      handle General.Subscript => raise THM ("permute_prems: j", j, [rl]);
    val n_j = length moved_prems;
    val m = if k < 0 then n_j + k else k;
    val prop' =
      if 0 = m orelse m = n_j then prop
      else if 0 < m andalso m < n_j then
        let val (ps, qs) = chop m moved_prems
        in Logic.list_implies (fixed_prems @ qs @ ps, concl) end
      else raise THM ("permute_prems: k", k, [rl]);
  in
    Thm (deriv_rule1 (Proofterm.permute_prems_proof prems j m) der,
     {thy = thy,
      tags = [],
      maxidx = maxidx,
      shyps = shyps,
      hyps = hyps,
      tpairs = tpairs,
      prop = prop'})
  end;


(** User renaming of parameters in a subgoal **)

(*Calls error rather than raising an exception because it is intended
  for top-level use -- exception handling would not make sense here.
  The names in cs, if distinct, are used for the innermost parameters;
  preceding parameters may be renamed to make all params distinct.*)
fun rename_params_rule (cs, i) state =
  let
    val Thm (der, {thy, tags, maxidx, shyps, hyps, ...}) = state;
    val (tpairs, Bs, Bi, C) = dest_state (state, i);
    val iparams = map #1 (Logic.strip_params Bi);
    val short = length iparams - length cs;
    val newnames =
      if short < 0 then error "More names than abstractions!"
      else Name.variant_list cs (take short iparams) @ cs;
    val freenames = Term.fold_aterms (fn Free (x, _) => insert (op =) x | _ => I) Bi [];
    val newBi = Logic.list_rename_params newnames Bi;
  in
    (case duplicates (op =) cs of
      a :: _ => (warning ("Can't rename.  Bound variables not distinct: " ^ a); state)
    | [] =>
      (case inter (op =) cs freenames of
        a :: _ => (warning ("Can't rename.  Bound/Free variable clash: " ^ a); state)
      | [] =>
        Thm (der,
         {thy = thy,
          tags = tags,
          maxidx = maxidx,
          shyps = shyps,
          hyps = hyps,
          tpairs = tpairs,
          prop = Logic.list_implies (Bs @ [newBi], C)})))
  end;


(*** Preservation of bound variable names ***)

fun rename_boundvars pat obj (thm as Thm (der, {thy, tags, maxidx, shyps, hyps, tpairs, prop})) =
  (case Term.rename_abs pat obj prop of
    NONE => thm
  | SOME prop' => Thm (der,
      {thy = thy,
       tags = tags,
       maxidx = maxidx,
       hyps = hyps,
       shyps = shyps,
       tpairs = tpairs,
       prop = prop'}));


(* strip_apply f B A strips off all assumptions/parameters from A
   introduced by lifting over B, and applies f to remaining part of A*)
fun strip_apply f =
  let fun strip (Const ("Pure.imp", _) $ _  $ B1)
                (Const ("Pure.imp", _) $ A2 $ B2) = Logic.mk_implies (A2, strip B1 B2)
        | strip ((c as Const ("Pure.all", _)) $ Abs (_, _, t1))
                (      Const ("Pure.all", _)  $ Abs (a, T, t2)) = c $ Abs (a, T, strip t1 t2)
        | strip _ A = f A
  in strip end;

fun strip_lifted (Const ("Pure.imp", _) $ _ $ B1)
                 (Const ("Pure.imp", _) $ _ $ B2) = strip_lifted B1 B2
  | strip_lifted (Const ("Pure.all", _) $ Abs (_, _, t1))
                 (Const ("Pure.all", _) $ Abs (_, _, t2)) = strip_lifted t1 t2
  | strip_lifted _ A = A;

(*Use the alist to rename all bound variables and some unknowns in a term
  dpairs = current disagreement pairs;  tpairs = permanent ones (flexflex);
  Preserves unknowns in tpairs and on lhs of dpairs. *)
fun rename_bvs [] _ _ _ _ = K I
  | rename_bvs al dpairs tpairs B As =
      let
        val add_var = fold_aterms (fn Var ((x, _), _) => insert (op =) x | _ => I);
        val vids = []
          |> fold (add_var o fst) dpairs
          |> fold (add_var o fst) tpairs
          |> fold (add_var o snd) tpairs;
        val vids' = fold (add_var o strip_lifted B) As [];
        (*unknowns appearing elsewhere be preserved!*)
        val al' = distinct ((op =) o pairself fst)
          (filter_out (fn (x, y) =>
             not (member (op =) vids' x) orelse
             member (op =) vids x orelse member (op =) vids y) al);
        val unchanged = filter_out (AList.defined (op =) al') vids';
        fun del_clashing clash xs _ [] qs =
              if clash then del_clashing false xs xs qs [] else qs
          | del_clashing clash xs ys ((p as (x, y)) :: ps) qs =
              if member (op =) ys y
              then del_clashing true (x :: xs) (x :: ys) ps qs
              else del_clashing clash xs (y :: ys) ps (p :: qs);
        val al'' = del_clashing false unchanged unchanged al' [];
        fun rename (t as Var ((x, i), T)) =
              (case AList.lookup (op =) al'' x of
                 SOME y => Var ((y, i), T)
               | NONE => t)
          | rename (Abs (x, T, t)) =
              Abs (the_default x (AList.lookup (op =) al x), T, rename t)
          | rename (f $ t) = rename f $ rename t
          | rename t = t;
        fun strip_ren f Ai = f rename B Ai
      in strip_ren end;

(*Function to rename bounds/unknowns in the argument, lifted over B*)
fun rename_bvars dpairs =
  rename_bvs (fold_rev Term.match_bvars dpairs []) dpairs;


(*** RESOLUTION ***)

(** Lifting optimizations **)

(*strip off pairs of assumptions/parameters in parallel -- they are
  identical because of lifting*)
fun strip_assums2 (Const("Pure.imp", _) $ _ $ B1,
                   Const("Pure.imp", _) $ _ $ B2) = strip_assums2 (B1,B2)
  | strip_assums2 (Const("Pure.all",_)$Abs(a,T,t1),
                   Const("Pure.all",_)$Abs(_,_,t2)) =
      let val (B1,B2) = strip_assums2 (t1,t2)
      in  (Abs(a,T,B1), Abs(a,T,B2))  end
  | strip_assums2 BB = BB;


(*Faster normalization: skip assumptions that were lifted over*)
fun norm_term_skip env 0 t = Envir.norm_term env t
  | norm_term_skip env n (Const ("Pure.all", _) $ Abs (a, T, t)) =
      let
        val T' = Envir.subst_type (Envir.type_env env) T
        (*Must instantiate types of parameters because they are flattened;
          this could be a NEW parameter*)
      in Logic.all_const T' $ Abs (a, T', norm_term_skip env n t) end
  | norm_term_skip env n (Const ("Pure.imp", _) $ A $ B) =
      Logic.mk_implies (A, norm_term_skip env (n - 1) B)
  | norm_term_skip _ _ _ = error "norm_term_skip: too few assumptions??";


(*unify types of schematic variables (non-lifted case)*)
fun unify_var_types thy (th1, th2) env =
  let
    fun unify_vars (T :: Us) = fold (fn U => Pattern.unify_types thy (T, U)) Us
      | unify_vars _ = I;
    val add_vars =
      full_prop_of #>
      fold_aterms (fn Var v => Vartab.insert_list (op =) v | _ => I);
    val vars = Vartab.empty |> add_vars th1 |> add_vars th2;
  in SOME (Vartab.fold (unify_vars o #2) vars env) end
  handle Pattern.Unif => NONE;

(*Composition of object rule r=(A1...Am/B) with proof state s=(B1...Bn/C)
  Unifies B with Bi, replacing subgoal i    (1 <= i <= n)
  If match then forbid instantiations in proof state
  If lifted then shorten the dpair using strip_assums2.
  If eres_flg then simultaneously proves A1 by assumption.
  nsubgoal is the number of new subgoals (written m above).
  Curried so that resolution calls dest_state only once.
*)
local exception COMPOSE
in
fun bicompose_aux {flatten, match, incremented} (state, (stpairs, Bs, Bi, C), lifted)
                        (eres_flg, orule, nsubgoal) =
 let val Thm (sder, {maxidx=smax, shyps=sshyps, hyps=shyps, ...}) = state
     and Thm (rder, {maxidx=rmax, shyps=rshyps, hyps=rhyps,
             tpairs=rtpairs, prop=rprop,...}) = orule
         (*How many hyps to skip over during normalization*)
     and nlift = Logic.count_prems (strip_all_body Bi) + (if eres_flg then ~1 else 0)
     val thy = merge_thys2 state orule;
     (** Add new theorem with prop = '[| Bs; As |] ==> C' to thq **)
     fun addth A (As, oldAs, rder', n) ((env, tpairs), thq) =
       let val normt = Envir.norm_term env;
           (*perform minimal copying here by examining env*)
           val (ntpairs, normp) =
             if Envir.is_empty env then (tpairs, (Bs @ As, C))
             else
             let val ntps = map (pairself normt) tpairs
             in if Envir.above env smax then
                  (*no assignments in state; normalize the rule only*)
                  if lifted
                  then (ntps, (Bs @ map (norm_term_skip env nlift) As, C))
                  else (ntps, (Bs @ map normt As, C))
                else if match then raise COMPOSE
                else (*normalize the new rule fully*)
                  (ntps, (map normt (Bs @ As), normt C))
             end
           val th =
             Thm (deriv_rule2
                   ((if Envir.is_empty env then I
                     else if Envir.above env smax then
                       (fn f => fn der => f (Proofterm.norm_proof' env der))
                     else
                       curry op oo (Proofterm.norm_proof' env))
                    (Proofterm.bicompose_proof flatten Bs oldAs As A n (nlift+1))) rder' sder,
                {tags = [],
                 maxidx = Envir.maxidx_of env,
                 shyps = Envir.insert_sorts env (Sorts.union rshyps sshyps),
                 hyps = union_hyps rhyps shyps,
                 tpairs = ntpairs,
                 prop = Logic.list_implies normp,
                 thy = thy})
        in  Seq.cons th thq  end  handle COMPOSE => thq;
     val (rAs,B) = Logic.strip_prems(nsubgoal, [], rprop)
       handle TERM _ => raise THM("bicompose: rule", 0, [orule,state]);
     (*Modify assumptions, deleting n-th if n>0 for e-resolution*)
     fun newAs(As0, n, dpairs, tpairs) =
       let val (As1, rder') =
         if not lifted then (As0, rder)
         else
           let val rename = rename_bvars dpairs tpairs B As0
           in (map (rename strip_apply) As0,
             deriv_rule1 (Proofterm.map_proof_terms (rename K) I) rder)
           end;
       in (map (if flatten then (Logic.flatten_params n) else I) As1, As1, rder', n)
          handle TERM _ =>
          raise THM("bicompose: 1st premise", 0, [orule])
       end;
     val BBi = if lifted then strip_assums2(B,Bi) else (B,Bi);
     val dpairs = BBi :: (rtpairs@stpairs);

     (*elim-resolution: try each assumption in turn*)
     fun eres _ [] = raise THM ("bicompose: no premises", 0, [orule, state])
       | eres env (A1 :: As) =
           let
             val A = SOME A1;
             val (close, asms, concl) = Logic.assum_problems (nlift + 1, A1);
             val concl' = close concl;
             fun tryasms [] _ = Seq.empty
               | tryasms (asm :: rest) n =
                   if Term.could_unify (asm, concl) then
                     let val asm' = close asm in
                       (case Seq.pull (Unify.unifiers (thy, env, (asm', concl') :: dpairs)) of
                         NONE => tryasms rest (n + 1)
                       | cell as SOME ((_, tpairs), _) =>
                           Seq.it_right (addth A (newAs (As, n, [BBi, (concl', asm')], tpairs)))
                             (Seq.make (fn () => cell),
                              Seq.make (fn () => Seq.pull (tryasms rest (n + 1)))))
                     end
                   else tryasms rest (n + 1);
           in tryasms asms 1 end;

     (*ordinary resolution*)
     fun res env =
       (case Seq.pull (Unify.unifiers (thy, env, dpairs)) of
         NONE => Seq.empty
       | cell as SOME ((_, tpairs), _) =>
           Seq.it_right (addth NONE (newAs (rev rAs, 0, [BBi], tpairs)))
             (Seq.make (fn () => cell), Seq.empty));

     val env0 = Envir.empty (Int.max (rmax, smax));
 in
   (case if incremented then SOME env0 else unify_var_types thy (state, orule) env0 of
     NONE => Seq.empty
   | SOME env => if eres_flg then eres env (rev rAs) else res env)
 end;
end;

fun bicompose flags arg i state =
  bicompose_aux flags (state, dest_state (state,i), false) arg;

(*Quick test whether rule is resolvable with the subgoal with hyps Hs
  and conclusion B.  If eres_flg then checks 1st premise of rule also*)
fun could_bires (Hs, B, eres_flg, rule) =
    let fun could_reshyp (A1::_) = exists (fn H => Term.could_unify (A1, H)) Hs
          | could_reshyp [] = false;  (*no premise -- illegal*)
    in  Term.could_unify(concl_of rule, B) andalso
        (not eres_flg  orelse  could_reshyp (prems_of rule))
    end;

(*Bi-resolution of a state with a list of (flag,rule) pairs.
  Puts the rule above:  rule/state.  Renames vars in the rules. *)
fun biresolution match brules i state =
    let val (stpairs, Bs, Bi, C) = dest_state(state,i);
        val lift = lift_rule (cprem_of state i);
        val B = Logic.strip_assums_concl Bi;
        val Hs = Logic.strip_assums_hyp Bi;
        val compose =
          bicompose_aux {flatten = true, match = match, incremented = true}
            (state, (stpairs, Bs, Bi, C), true);
        fun res [] = Seq.empty
          | res ((eres_flg, rule)::brules) =
              if Config.get_global (theory_of_thm state) Pattern.unify_trace_failure orelse
                 could_bires (Hs, B, eres_flg, rule)
              then Seq.make (*delay processing remainder till needed*)
                  (fn()=> SOME(compose (eres_flg, lift rule, nprems_of rule),
                               res brules))
              else res brules
    in  Seq.flat (res brules)  end;



(*** Oracles ***)

(* oracle rule *)

fun invoke_oracle thy1 name oracle arg =
  let val Cterm {thy = thy2, t = prop, T, maxidx, sorts} = oracle arg in
    if T <> propT then
      raise THM ("Oracle's result must have type prop: " ^ name, 0, [])
    else
      let val (ora, prf) = Proofterm.oracle_proof name prop in
        Thm (make_deriv [] [ora] [] prf,
         {thy = Theory.merge (thy1, thy2),
          tags = [],
          maxidx = maxidx,
          shyps = sorts,
          hyps = [],
          tpairs = [],
          prop = prop})
      end
  end;

end;
end;
end;


(* authentic derivation names *)

structure Oracles = Theory_Data
(
  type T = unit Name_Space.table;
  val empty : T = Name_Space.empty_table "oracle";
  val extend = I;
  fun merge data : T = Name_Space.merge_tables data;
);

fun extern_oracles ctxt =
  map #1 (Name_Space.markup_table ctxt (Oracles.get (Proof_Context.theory_of ctxt)));

fun add_oracle (b, oracle) thy =
  let
    val (name, tab') = Name_Space.define (Context.Theory thy) true (b, ()) (Oracles.get thy);
    val thy' = Oracles.put tab' thy;
  in ((name, invoke_oracle thy' name oracle), thy') end;

end;

structure Basic_Thm: BASIC_THM = Thm;
open Basic_Thm;
