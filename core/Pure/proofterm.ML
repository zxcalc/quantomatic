(*  Title:      Pure/proofterm.ML
    Author:     Stefan Berghofer, TU Muenchen

LF style proof terms.
*)

infix 8 % %% %>;

signature BASIC_PROOFTERM =
sig
  val proofs: int Unsynchronized.ref

  datatype proof =
     MinProof
   | PBound of int
   | Abst of string * typ option * proof
   | AbsP of string * term option * proof
   | op % of proof * term option
   | op %% of proof * proof
   | Hyp of term
   | PAxm of string * term * typ list option
   | OfClass of typ * class
   | Oracle of string * term * typ list option
   | Promise of serial * term * typ list
   | PThm of serial * ((string * term * typ list option) * proof_body future)
  and proof_body = PBody of
    {oracles: (string * term) Ord_List.T,
     thms: (serial * (string * term * proof_body future)) Ord_List.T,
     proof: proof}

  val %> : proof * term -> proof
end;

signature PROOFTERM =
sig
  include BASIC_PROOFTERM

  type oracle = string * term
  type pthm = serial * (string * term * proof_body future)
  val proof_of: proof_body -> proof
  val join_proof: proof_body future -> proof
  val fold_proof_atoms: bool -> (proof -> 'a -> 'a) -> proof list -> 'a -> 'a
  val fold_body_thms: (string * term * proof_body -> 'a -> 'a) -> proof_body list -> 'a -> 'a
  val join_bodies: proof_body list -> unit
  val peek_status: proof_body list -> {failed: bool, oracle: bool, unfinished: bool}

  val oracle_ord: oracle * oracle -> order
  val thm_ord: pthm * pthm -> order
  val unions_oracles: oracle Ord_List.T list -> oracle Ord_List.T
  val unions_thms: pthm Ord_List.T list -> pthm Ord_List.T
  val all_oracles_of: proof_body -> oracle Ord_List.T
  val approximate_proof_body: proof -> proof_body
  val no_proof_body: proof_body
  val no_thm_proofs: proof -> proof

  val encode: proof XML.Encode.T
  val encode_body: proof_body XML.Encode.T
  val decode: proof XML.Decode.T
  val decode_body: proof_body XML.Decode.T

  (** primitive operations **)
  val proofs_enabled: unit -> bool
  val proof_combt: proof * term list -> proof
  val proof_combt': proof * term option list -> proof
  val proof_combP: proof * proof list -> proof
  val strip_combt: proof -> proof * term option list
  val strip_combP: proof -> proof * proof list
  val strip_thm: proof_body -> proof_body
  val map_proof_same: term Same.operation -> typ Same.operation
    -> (typ * class -> proof) -> proof Same.operation
  val map_proof_terms_same: term Same.operation -> typ Same.operation -> proof Same.operation
  val map_proof_types_same: typ Same.operation -> proof Same.operation
  val map_proof_terms: (term -> term) -> (typ -> typ) -> proof -> proof
  val map_proof_types: (typ -> typ) -> proof -> proof
  val fold_proof_terms: (term -> 'a -> 'a) -> (typ -> 'a -> 'a) -> proof -> 'a -> 'a
  val maxidx_proof: proof -> int -> int
  val size_of_proof: proof -> int
  val change_type: typ list option -> proof -> proof
  val prf_abstract_over: term -> proof -> proof
  val prf_incr_bv: int -> int -> int -> int -> proof -> proof
  val incr_pboundvars: int -> int -> proof -> proof
  val prf_loose_bvar1: proof -> int -> bool
  val prf_loose_Pbvar1: proof -> int -> bool
  val prf_add_loose_bnos: int -> int -> proof -> int list * int list -> int list * int list
  val norm_proof: Envir.env -> proof -> proof
  val norm_proof': Envir.env -> proof -> proof
  val prf_subst_bounds: term list -> proof -> proof
  val prf_subst_pbounds: proof list -> proof -> proof
  val freeze_thaw_prf: proof -> proof * (proof -> proof)

  (** proof terms for specific inference rules **)
  val implies_intr_proof: term -> proof -> proof
  val implies_intr_proof': term -> proof -> proof
  val forall_intr_proof: term -> string -> proof -> proof
  val forall_intr_proof': term -> proof -> proof
  val varify_proof: term -> (string * sort) list -> proof -> proof
  val legacy_freezeT: term -> proof -> proof
  val rotate_proof: term list -> term -> int -> proof -> proof
  val permute_prems_proof: term list -> int -> int -> proof -> proof
  val generalize: string list * string list -> int -> proof -> proof
  val instantiate: ((indexname * sort) * typ) list * ((indexname * typ) * term) list
    -> proof -> proof
  val lift_proof: term -> int -> term -> proof -> proof
  val incr_indexes: int -> proof -> proof
  val assumption_proof: term list -> term -> int -> proof -> proof
  val bicompose_proof: bool -> term list -> term list -> term list -> term option ->
    int -> int -> proof -> proof -> proof
  val equality_axms: (string * term) list
  val reflexive_axm: proof
  val symmetric_axm: proof
  val transitive_axm: proof
  val equal_intr_axm: proof
  val equal_elim_axm: proof
  val abstract_rule_axm: proof
  val combination_axm: proof
  val reflexive: proof
  val symmetric: proof -> proof
  val transitive: term -> typ -> proof -> proof -> proof
  val abstract_rule: term -> string -> proof -> proof
  val combination: term -> term -> term -> term -> typ -> proof -> proof -> proof
  val equal_intr: term -> term -> proof -> proof -> proof
  val equal_elim: term -> term -> proof -> proof -> proof
  val strip_shyps_proof: Sorts.algebra -> (typ * sort) list -> (typ * sort) list ->
    sort list -> proof -> proof
  val classrel_proof: theory -> class * class -> proof
  val arity_proof: theory -> string * sort list * class -> proof
  val of_sort_proof: theory -> (typ * class -> proof) -> typ * sort -> proof list
  val install_axclass_proofs:
   {classrel_proof: theory -> class * class -> proof,
    arity_proof: theory -> string * sort list * class -> proof} -> unit
  val axm_proof: string -> term -> proof
  val oracle_proof: string -> term -> oracle * proof

  (** rewriting on proof terms **)
  val add_prf_rrule: proof * proof -> theory -> theory
  val add_prf_rproc: (typ list -> term option list -> proof -> (proof * proof) option) -> theory -> theory
  val no_skel: proof
  val normal_skel: proof
  val rewrite_proof: theory -> (proof * proof) list *
    (typ list -> term option list -> proof -> (proof * proof) option) list -> proof -> proof
  val rewrite_proof_notypes: (proof * proof) list *
    (typ list -> term option list -> proof -> (proof * proof) option) list -> proof -> proof
  val rew_proof: theory -> proof -> proof

  val promise_proof: theory -> serial -> term -> proof
  val fulfill_norm_proof: theory -> (serial * proof_body) list -> proof_body -> proof_body
  val thm_proof: theory -> string -> sort list -> term list -> term ->
    (serial * proof_body future) list -> proof_body -> pthm * proof
  val unconstrain_thm_proof: theory -> sort list -> term ->
    (serial * proof_body future) list -> proof_body -> pthm * proof
  val get_name: sort list -> term list -> term -> proof -> string
  val guess_name: proof -> string
end

structure Proofterm : PROOFTERM =
struct

(***** datatype proof *****)

datatype proof =
   MinProof
 | PBound of int
 | Abst of string * typ option * proof
 | AbsP of string * term option * proof
 | op % of proof * term option
 | op %% of proof * proof
 | Hyp of term
 | PAxm of string * term * typ list option
 | OfClass of typ * class
 | Oracle of string * term * typ list option
 | Promise of serial * term * typ list
 | PThm of serial * ((string * term * typ list option) * proof_body future)
and proof_body = PBody of
  {oracles: (string * term) Ord_List.T,
   thms: (serial * (string * term * proof_body future)) Ord_List.T,
   proof: proof};

type oracle = string * term;
type pthm = serial * (string * term * proof_body future);

fun proof_of (PBody {proof, ...}) = proof;
val join_proof = Future.join #> proof_of;

fun join_thms (thms: pthm list) = ignore (Future.joins (map (#3 o #2) thms));


(***** proof atoms *****)

fun fold_proof_atoms all f =
  let
    fun app (Abst (_, _, prf)) = app prf
      | app (AbsP (_, _, prf)) = app prf
      | app (prf % _) = app prf
      | app (prf1 %% prf2) = app prf1 #> app prf2
      | app (prf as PThm (i, (_, body))) = (fn (x, seen) =>
          if Inttab.defined seen i then (x, seen)
          else
            let val (x', seen') =
              (if all then app (join_proof body) else I) (x, Inttab.update (i, ()) seen)
            in (f prf x', seen') end)
      | app prf = (fn (x, seen) => (f prf x, seen));
  in fn prfs => fn x => #1 (fold app prfs (x, Inttab.empty)) end;

fun fold_body_thms f =
  let
    fun app (PBody {thms, ...}) =
      tap join_thms thms |> fold (fn (i, (name, prop, body)) => fn (x, seen) =>
        if Inttab.defined seen i then (x, seen)
        else
          let
            val body' = Future.join body;
            val (x', seen') = app body' (x, Inttab.update (i, ()) seen);
          in (f (name, prop, body') x', seen') end);
  in fn bodies => fn x => #1 (fold app bodies (x, Inttab.empty)) end;

fun join_bodies bodies = fold_body_thms (fn _ => fn () => ()) bodies ();

fun peek_status bodies =
  let
    fun status (PBody {oracles, thms, ...}) x =
      let
        val ((oracle, unfinished, failed), seen) =
          (thms, x) |-> fold (fn (i, (_, _, body)) => fn (st, seen) =>
            if Inttab.defined seen i then (st, seen)
            else
              let val seen' = Inttab.update (i, ()) seen in
                (case Future.peek body of
                  SOME (Exn.Res body') => status body' (st, seen')
                | SOME (Exn.Exn _) =>
                    let val (oracle, unfinished, _) = st
                    in ((oracle, unfinished, true), seen') end
                | NONE =>
                    let val (oracle, _, failed) = st
                    in ((oracle, true, failed), seen') end)
              end);
      in ((oracle orelse not (null oracles), unfinished, failed), seen) end;
    val (oracle, unfinished, failed) =
      #1 (fold status bodies ((false, false, false), Inttab.empty));
  in {oracle = oracle, unfinished = unfinished, failed = failed} end;


(* proof body *)

val oracle_ord = prod_ord fast_string_ord Term_Ord.fast_term_ord;
fun thm_ord ((i, _): pthm, (j, _)) = int_ord (j, i);

val unions_oracles = Ord_List.unions oracle_ord;
val unions_thms = Ord_List.unions thm_ord;

val all_oracles_of =
  let
    fun collect (PBody {oracles, thms, ...}) =
      tap join_thms thms |> fold (fn (i, (_, _, body)) => fn (x, seen) =>
        if Inttab.defined seen i then (x, seen)
        else
          let
            val body' = Future.join body;
            val (x', seen') = collect body' (x, Inttab.update (i, ()) seen);
          in (if null oracles then x' else oracles :: x', seen') end);
  in fn body => unions_oracles (#1 (collect body ([], Inttab.empty))) end;

fun approximate_proof_body prf =
  let
    val (oracles, thms) = fold_proof_atoms false
      (fn Oracle (s, prop, _) => apfst (cons (s, prop))
        | PThm (i, ((name, prop, _), body)) => apsnd (cons (i, (name, prop, body)))
        | _ => I) [prf] ([], []);
  in
    PBody
     {oracles = Ord_List.make oracle_ord oracles,
      thms = Ord_List.make thm_ord thms,
      proof = prf}
  end;

val no_proof_body = PBody {oracles = [], thms = [], proof = MinProof};
val no_body = Future.value no_proof_body;

fun no_thm_proofs (PThm (i, (a, _))) = PThm (i, (a, no_body))
  | no_thm_proofs (Abst (x, T, prf)) = Abst (x, T, no_thm_proofs prf)
  | no_thm_proofs (AbsP (x, t, prf)) = AbsP (x, t, no_thm_proofs prf)
  | no_thm_proofs (prf % t) = no_thm_proofs prf % t
  | no_thm_proofs (prf1 %% prf2) = no_thm_proofs prf1 %% no_thm_proofs prf2
  | no_thm_proofs a = a;


(***** XML data representation *****)

(* encode *)

local

open XML.Encode Term_XML.Encode;

fun proof prf = prf |> variant
 [fn MinProof => ([], []),
  fn PBound a => ([int_atom a], []),
  fn Abst (a, b, c) => ([a], pair (option typ) proof (b, c)),
  fn AbsP (a, b, c) => ([a], pair (option term) proof (b, c)),
  fn a % b => ([], pair proof (option term) (a, b)),
  fn a %% b => ([], pair proof proof (a, b)),
  fn Hyp a => ([], term a),
  fn PAxm (a, b, c) => ([a], pair term (option (list typ)) (b, c)),
  fn OfClass (a, b) => ([b], typ a),
  fn Oracle (a, b, c) => ([a], pair term (option (list typ)) (b, c)),
  fn Promise (a, b, c) => ([int_atom a], pair term (list typ) (b, c)),
  fn PThm (a, ((b, c, d), body)) =>
    ([int_atom a, b], triple term (option (list typ)) proof_body (c, d, Future.join body))]
and proof_body (PBody {oracles, thms, proof = prf}) =
  triple (list (pair string term)) (list pthm) proof (oracles, thms, prf)
and pthm (a, (b, c, body)) =
  pair int (triple string term proof_body) (a, (b, c, Future.join body));

in

val encode = proof;
val encode_body = proof_body;

end;


(* decode *)

local

open XML.Decode Term_XML.Decode;

fun proof prf = prf |> variant
 [fn ([], []) => MinProof,
  fn ([a], []) => PBound (int_atom a),
  fn ([a], b) => let val (c, d) = pair (option typ) proof b in Abst (a, c, d) end,
  fn ([a], b) => let val (c, d) = pair (option term) proof b in AbsP (a, c, d) end,
  fn ([], a) => op % (pair proof (option term) a),
  fn ([], a) => op %% (pair proof proof a),
  fn ([], a) => Hyp (term a),
  fn ([a], b) => let val (c, d) = pair term (option (list typ)) b in PAxm (a, c, d) end,
  fn ([b], a) => OfClass (typ a, b),
  fn ([a], b) => let val (c, d) = pair term (option (list typ)) b in Oracle (a, c, d) end,
  fn ([a], b) => let val (c, d) = pair term (list typ) b in Promise (int_atom a, c, d) end,
  fn ([a, b], c) =>
    let val (d, e, f) = triple term (option (list typ)) proof_body c
    in PThm (int_atom a, ((b, d, e), Future.value f)) end]
and proof_body x =
  let val (a, b, c) = triple (list (pair string term)) (list pthm) proof x
  in PBody {oracles = a, thms = b, proof = c} end
and pthm x =
  let val (a, (b, c, d)) = pair int (triple string term proof_body) x
  in (a, (b, c, Future.value d)) end;

in

val decode = proof;
val decode_body = proof_body;

end;


(***** proof objects with different levels of detail *****)

fun (prf %> t) = prf % SOME t;

val proof_combt = Library.foldl (op %>);
val proof_combt' = Library.foldl (op %);
val proof_combP = Library.foldl (op %%);

fun strip_combt prf =
    let fun stripc (prf % t, ts) = stripc (prf, t::ts)
          | stripc  x =  x
    in  stripc (prf, [])  end;

fun strip_combP prf =
    let fun stripc (prf %% prf', prfs) = stripc (prf, prf'::prfs)
          | stripc  x =  x
    in  stripc (prf, [])  end;

fun strip_thm (body as PBody {proof, ...}) =
  (case strip_combt (fst (strip_combP proof)) of
    (PThm (_, (_, body')), _) => Future.join body'
  | _ => body);

val mk_Abst = fold_rev (fn (s, T:typ) => fn prf => Abst (s, NONE, prf));
fun mk_AbsP (i, prf) = funpow i (fn prf => AbsP ("H", NONE, prf)) prf;

fun map_proof_same term typ ofclass =
  let
    val typs = Same.map typ;

    fun proof (Abst (s, T, prf)) =
          (Abst (s, Same.map_option typ T, Same.commit proof prf)
            handle Same.SAME => Abst (s, T, proof prf))
      | proof (AbsP (s, t, prf)) =
          (AbsP (s, Same.map_option term t, Same.commit proof prf)
            handle Same.SAME => AbsP (s, t, proof prf))
      | proof (prf % t) =
          (proof prf % Same.commit (Same.map_option term) t
            handle Same.SAME => prf % Same.map_option term t)
      | proof (prf1 %% prf2) =
          (proof prf1 %% Same.commit proof prf2
            handle Same.SAME => prf1 %% proof prf2)
      | proof (PAxm (a, prop, SOME Ts)) = PAxm (a, prop, SOME (typs Ts))
      | proof (OfClass T_c) = ofclass T_c
      | proof (Oracle (a, prop, SOME Ts)) = Oracle (a, prop, SOME (typs Ts))
      | proof (Promise (i, prop, Ts)) = Promise (i, prop, typs Ts)
      | proof (PThm (i, ((a, prop, SOME Ts), body))) =
          PThm (i, ((a, prop, SOME (typs Ts)), body))
      | proof _ = raise Same.SAME;
  in proof end;

fun map_proof_terms_same term typ = map_proof_same term typ (fn (T, c) => OfClass (typ T, c));
fun map_proof_types_same typ = map_proof_terms_same (Term_Subst.map_types_same typ) typ;

fun same eq f x =
  let val x' = f x
  in if eq (x, x') then raise Same.SAME else x' end;

fun map_proof_terms f g = Same.commit (map_proof_terms_same (same (op =) f) (same (op =) g));
fun map_proof_types f = Same.commit (map_proof_types_same (same (op =) f));

fun fold_proof_terms f g (Abst (_, SOME T, prf)) = g T #> fold_proof_terms f g prf
  | fold_proof_terms f g (Abst (_, NONE, prf)) = fold_proof_terms f g prf
  | fold_proof_terms f g (AbsP (_, SOME t, prf)) = f t #> fold_proof_terms f g prf
  | fold_proof_terms f g (AbsP (_, NONE, prf)) = fold_proof_terms f g prf
  | fold_proof_terms f g (prf % SOME t) = fold_proof_terms f g prf #> f t
  | fold_proof_terms f g (prf % NONE) = fold_proof_terms f g prf
  | fold_proof_terms f g (prf1 %% prf2) =
      fold_proof_terms f g prf1 #> fold_proof_terms f g prf2
  | fold_proof_terms _ g (PAxm (_, _, SOME Ts)) = fold g Ts
  | fold_proof_terms _ g (OfClass (T, _)) = g T
  | fold_proof_terms _ g (Oracle (_, _, SOME Ts)) = fold g Ts
  | fold_proof_terms _ g (Promise (_, _, Ts)) = fold g Ts
  | fold_proof_terms _ g (PThm (_, ((_, _, SOME Ts), _))) = fold g Ts
  | fold_proof_terms _ _ _ = I;

fun maxidx_proof prf = fold_proof_terms Term.maxidx_term Term.maxidx_typ prf;

fun size_of_proof (Abst (_, _, prf)) = 1 + size_of_proof prf
  | size_of_proof (AbsP (_, t, prf)) = 1 + size_of_proof prf
  | size_of_proof (prf % _) = 1 + size_of_proof prf
  | size_of_proof (prf1 %% prf2) = size_of_proof prf1 + size_of_proof prf2
  | size_of_proof _ = 1;

fun change_type opTs (PAxm (name, prop, _)) = PAxm (name, prop, opTs)
  | change_type (SOME [T]) (OfClass (_, c)) = OfClass (T, c)
  | change_type opTs (Oracle (name, prop, _)) = Oracle (name, prop, opTs)
  | change_type opTs (Promise _) = raise Fail "change_type: unexpected promise"
  | change_type opTs (PThm (i, ((name, prop, _), body))) =
      PThm (i, ((name, prop, opTs), body))
  | change_type _ prf = prf;


(***** utilities *****)

fun strip_abs (_::Ts) (Abs (_, _, t)) = strip_abs Ts t
  | strip_abs _ t = t;

fun mk_abs Ts t = Library.foldl (fn (t', T) => Abs ("", T, t')) (t, Ts);


(*Abstraction of a proof term over its occurrences of v,
    which must contain no loose bound variables.
  The resulting proof term is ready to become the body of an Abst.*)

fun prf_abstract_over v =
  let
    fun abst' lev u = if v aconv u then Bound lev else
      (case u of
         Abs (a, T, t) => Abs (a, T, abst' (lev + 1) t)
       | f $ t => (abst' lev f $ absth' lev t handle Same.SAME => f $ abst' lev t)
       | _ => raise Same.SAME)
    and absth' lev t = (abst' lev t handle Same.SAME => t);

    fun abst lev (AbsP (a, t, prf)) =
          (AbsP (a, Same.map_option (abst' lev) t, absth lev prf)
           handle Same.SAME => AbsP (a, t, abst lev prf))
      | abst lev (Abst (a, T, prf)) = Abst (a, T, abst (lev + 1) prf)
      | abst lev (prf1 %% prf2) = (abst lev prf1 %% absth lev prf2
          handle Same.SAME => prf1 %% abst lev prf2)
      | abst lev (prf % t) = (abst lev prf % Option.map (absth' lev) t
          handle Same.SAME => prf % Same.map_option (abst' lev) t)
      | abst _ _ = raise Same.SAME
    and absth lev prf = (abst lev prf handle Same.SAME => prf);

  in absth 0 end;


(*increments a proof term's non-local bound variables
  required when moving a proof term within abstractions
     inc is  increment for bound variables
     lev is  level at which a bound variable is considered 'loose'*)

fun incr_bv' inct tlev t = incr_bv (inct, tlev, t);

fun prf_incr_bv' incP inct Plev tlev (PBound i) =
      if i >= Plev then PBound (i+incP) else raise Same.SAME
  | prf_incr_bv' incP inct Plev tlev (AbsP (a, t, body)) =
      (AbsP (a, Same.map_option (same (op =) (incr_bv' inct tlev)) t,
         prf_incr_bv incP inct (Plev+1) tlev body) handle Same.SAME =>
           AbsP (a, t, prf_incr_bv' incP inct (Plev+1) tlev body))
  | prf_incr_bv' incP inct Plev tlev (Abst (a, T, body)) =
      Abst (a, T, prf_incr_bv' incP inct Plev (tlev+1) body)
  | prf_incr_bv' incP inct Plev tlev (prf %% prf') =
      (prf_incr_bv' incP inct Plev tlev prf %% prf_incr_bv incP inct Plev tlev prf'
       handle Same.SAME => prf %% prf_incr_bv' incP inct Plev tlev prf')
  | prf_incr_bv' incP inct Plev tlev (prf % t) =
      (prf_incr_bv' incP inct Plev tlev prf % Option.map (incr_bv' inct tlev) t
       handle Same.SAME => prf % Same.map_option (same (op =) (incr_bv' inct tlev)) t)
  | prf_incr_bv' _ _ _ _ _ = raise Same.SAME
and prf_incr_bv incP inct Plev tlev prf =
      (prf_incr_bv' incP inct Plev tlev prf handle Same.SAME => prf);

fun incr_pboundvars  0 0 prf = prf
  | incr_pboundvars incP inct prf = prf_incr_bv incP inct 0 0 prf;


fun prf_loose_bvar1 (prf1 %% prf2) k = prf_loose_bvar1 prf1 k orelse prf_loose_bvar1 prf2 k
  | prf_loose_bvar1 (prf % SOME t) k = prf_loose_bvar1 prf k orelse loose_bvar1 (t, k)
  | prf_loose_bvar1 (_ % NONE) _ = true
  | prf_loose_bvar1 (AbsP (_, SOME t, prf)) k = loose_bvar1 (t, k) orelse prf_loose_bvar1 prf k
  | prf_loose_bvar1 (AbsP (_, NONE, _)) k = true
  | prf_loose_bvar1 (Abst (_, _, prf)) k = prf_loose_bvar1 prf (k+1)
  | prf_loose_bvar1 _ _ = false;

fun prf_loose_Pbvar1 (PBound i) k = i = k
  | prf_loose_Pbvar1 (prf1 %% prf2) k = prf_loose_Pbvar1 prf1 k orelse prf_loose_Pbvar1 prf2 k
  | prf_loose_Pbvar1 (prf % _) k = prf_loose_Pbvar1 prf k
  | prf_loose_Pbvar1 (AbsP (_, _, prf)) k = prf_loose_Pbvar1 prf (k+1)
  | prf_loose_Pbvar1 (Abst (_, _, prf)) k = prf_loose_Pbvar1 prf k
  | prf_loose_Pbvar1 _ _ = false;

fun prf_add_loose_bnos plev tlev (PBound i) (is, js) =
      if i < plev then (is, js) else (insert (op =) (i-plev) is, js)
  | prf_add_loose_bnos plev tlev (prf1 %% prf2) p =
      prf_add_loose_bnos plev tlev prf2
        (prf_add_loose_bnos plev tlev prf1 p)
  | prf_add_loose_bnos plev tlev (prf % opt) (is, js) =
      prf_add_loose_bnos plev tlev prf (case opt of
          NONE => (is, insert (op =) ~1 js)
        | SOME t => (is, add_loose_bnos (t, tlev, js)))
  | prf_add_loose_bnos plev tlev (AbsP (_, opt, prf)) (is, js) =
      prf_add_loose_bnos (plev+1) tlev prf (case opt of
          NONE => (is, insert (op =) ~1 js)
        | SOME t => (is, add_loose_bnos (t, tlev, js)))
  | prf_add_loose_bnos plev tlev (Abst (_, _, prf)) p =
      prf_add_loose_bnos plev (tlev+1) prf p
  | prf_add_loose_bnos _ _ _ _ = ([], []);


(**** substitutions ****)

fun del_conflicting_tvars envT T = Term_Subst.instantiateT
  (map_filter (fn ixnS as (_, S) =>
     (Type.lookup envT ixnS; NONE) handle TYPE _ =>
        SOME (ixnS, TFree ("'dummy", S))) (Term.add_tvarsT T [])) T;

fun del_conflicting_vars env t = Term_Subst.instantiate
  (map_filter (fn ixnS as (_, S) =>
     (Type.lookup (Envir.type_env env) ixnS; NONE) handle TYPE _ =>
        SOME (ixnS, TFree ("'dummy", S))) (Term.add_tvars t []),
   map_filter (fn (ixnT as (_, T)) =>
     (Envir.lookup env ixnT; NONE) handle TYPE _ =>
        SOME (ixnT, Free ("dummy", T))) (Term.add_vars t [])) t;

fun norm_proof env =
  let
    val envT = Envir.type_env env;
    fun msg s = warning ("type conflict in norm_proof:\n" ^ s);
    fun htype f t = f env t handle TYPE (s, _, _) =>
      (msg s; f env (del_conflicting_vars env t));
    fun htypeT f T = f envT T handle TYPE (s, _, _) =>
      (msg s; f envT (del_conflicting_tvars envT T));
    fun htypeTs f Ts = f envT Ts handle TYPE (s, _, _) =>
      (msg s; f envT (map (del_conflicting_tvars envT) Ts));

    fun norm (Abst (s, T, prf)) =
          (Abst (s, Same.map_option (htypeT Envir.norm_type_same) T, Same.commit norm prf)
            handle Same.SAME => Abst (s, T, norm prf))
      | norm (AbsP (s, t, prf)) =
          (AbsP (s, Same.map_option (htype Envir.norm_term_same) t, Same.commit norm prf)
            handle Same.SAME => AbsP (s, t, norm prf))
      | norm (prf % t) =
          (norm prf % Option.map (htype Envir.norm_term) t
            handle Same.SAME => prf % Same.map_option (htype Envir.norm_term_same) t)
      | norm (prf1 %% prf2) =
          (norm prf1 %% Same.commit norm prf2
            handle Same.SAME => prf1 %% norm prf2)
      | norm (PAxm (s, prop, Ts)) =
          PAxm (s, prop, Same.map_option (htypeTs Envir.norm_types_same) Ts)
      | norm (OfClass (T, c)) =
          OfClass (htypeT Envir.norm_type_same T, c)
      | norm (Oracle (s, prop, Ts)) =
          Oracle (s, prop, Same.map_option (htypeTs Envir.norm_types_same) Ts)
      | norm (Promise (i, prop, Ts)) =
          Promise (i, prop, htypeTs Envir.norm_types_same Ts)
      | norm (PThm (i, ((s, t, Ts), body))) =
          PThm (i, ((s, t, Same.map_option (htypeTs Envir.norm_types_same) Ts), body))
      | norm _ = raise Same.SAME;
  in Same.commit norm end;


(***** Remove some types in proof term (to save space) *****)

fun remove_types (Abs (s, _, t)) = Abs (s, dummyT, remove_types t)
  | remove_types (t $ u) = remove_types t $ remove_types u
  | remove_types (Const (s, _)) = Const (s, dummyT)
  | remove_types t = t;

fun remove_types_env (Envir.Envir {maxidx, tenv, tyenv}) =
  Envir.Envir {maxidx = maxidx, tenv = Vartab.map (K (apsnd remove_types)) tenv, tyenv = tyenv};

fun norm_proof' env prf = norm_proof (remove_types_env env) prf;


(**** substitution of bound variables ****)

fun prf_subst_bounds args prf =
  let
    val n = length args;
    fun subst' lev (Bound i) =
         (if i<lev then raise Same.SAME    (*var is locally bound*)
          else  incr_boundvars lev (nth args (i-lev))
                  handle General.Subscript => Bound (i-n))  (*loose: change it*)
      | subst' lev (Abs (a, T, body)) = Abs (a, T,  subst' (lev+1) body)
      | subst' lev (f $ t) = (subst' lev f $ substh' lev t
          handle Same.SAME => f $ subst' lev t)
      | subst' _ _ = raise Same.SAME
    and substh' lev t = (subst' lev t handle Same.SAME => t);

    fun subst lev (AbsP (a, t, body)) =
        (AbsP (a, Same.map_option (subst' lev) t, substh lev body)
          handle Same.SAME => AbsP (a, t, subst lev body))
      | subst lev (Abst (a, T, body)) = Abst (a, T, subst (lev+1) body)
      | subst lev (prf %% prf') = (subst lev prf %% substh lev prf'
          handle Same.SAME => prf %% subst lev prf')
      | subst lev (prf % t) = (subst lev prf % Option.map (substh' lev) t
          handle Same.SAME => prf % Same.map_option (subst' lev) t)
      | subst _ _ = raise Same.SAME
    and substh lev prf = (subst lev prf handle Same.SAME => prf);
  in case args of [] => prf | _ => substh 0 prf end;

fun prf_subst_pbounds args prf =
  let
    val n = length args;
    fun subst (PBound i) Plev tlev =
         (if i < Plev then raise Same.SAME    (*var is locally bound*)
          else incr_pboundvars Plev tlev (nth args (i-Plev))
                 handle General.Subscript => PBound (i-n)  (*loose: change it*))
      | subst (AbsP (a, t, body)) Plev tlev = AbsP (a, t, subst body (Plev+1) tlev)
      | subst (Abst (a, T, body)) Plev tlev = Abst (a, T, subst body Plev (tlev+1))
      | subst (prf %% prf') Plev tlev = (subst prf Plev tlev %% substh prf' Plev tlev
          handle Same.SAME => prf %% subst prf' Plev tlev)
      | subst (prf % t) Plev tlev = subst prf Plev tlev % t
      | subst  prf _ _ = raise Same.SAME
    and substh prf Plev tlev = (subst prf Plev tlev handle Same.SAME => prf)
  in case args of [] => prf | _ => substh prf 0 0 end;


(**** Freezing and thawing of variables in proof terms ****)

local

fun frzT names =
  map_type_tvar (fn (ixn, S) => TFree (the (AList.lookup (op =) names ixn), S));

fun thawT names =
  map_type_tfree (fn (a, S) =>
    (case AList.lookup (op =) names a of
      NONE => TFree (a, S)
    | SOME ixn => TVar (ixn, S)));

fun freeze names names' (t $ u) =
      freeze names names' t $ freeze names names' u
  | freeze names names' (Abs (s, T, t)) =
      Abs (s, frzT names' T, freeze names names' t)
  | freeze names names' (Const (s, T)) = Const (s, frzT names' T)
  | freeze names names' (Free (s, T)) = Free (s, frzT names' T)
  | freeze names names' (Var (ixn, T)) =
      Free (the (AList.lookup (op =) names ixn), frzT names' T)
  | freeze names names' t = t;

fun thaw names names' (t $ u) =
      thaw names names' t $ thaw names names' u
  | thaw names names' (Abs (s, T, t)) =
      Abs (s, thawT names' T, thaw names names' t)
  | thaw names names' (Const (s, T)) = Const (s, thawT names' T)
  | thaw names names' (Free (s, T)) =
      let val T' = thawT names' T in
        (case AList.lookup (op =) names s of
          NONE => Free (s, T')
        | SOME ixn => Var (ixn, T'))
      end
  | thaw names names' (Var (ixn, T)) = Var (ixn, thawT names' T)
  | thaw names names' t = t;

in

fun freeze_thaw_prf prf =
  let
    val (fs, Tfs, vs, Tvs) = fold_proof_terms
      (fn t => fn (fs, Tfs, vs, Tvs) =>
         (Term.add_free_names t fs, Term.add_tfree_names t Tfs,
          Term.add_var_names t vs, Term.add_tvar_names t Tvs))
      (fn T => fn (fs, Tfs, vs, Tvs) =>
         (fs, Term.add_tfree_namesT T Tfs,
          vs, Term.add_tvar_namesT T Tvs))
      prf ([], [], [], []);
    val names = vs ~~ Name.variant_list fs (map fst vs);
    val names' = Tvs ~~ Name.variant_list Tfs (map fst Tvs);
    val rnames = map swap names;
    val rnames' = map swap names';
  in
    (map_proof_terms (freeze names names') (frzT names') prf,
     map_proof_terms (thaw rnames rnames') (thawT rnames'))
  end;

end;


(***** implication introduction *****)

fun gen_implies_intr_proof f h prf =
  let
    fun abshyp i (Hyp t) = if h aconv t then PBound i else raise Same.SAME
      | abshyp i (Abst (s, T, prf)) = Abst (s, T, abshyp i prf)
      | abshyp i (AbsP (s, t, prf)) = AbsP (s, t, abshyp (i + 1) prf)
      | abshyp i (prf % t) = abshyp i prf % t
      | abshyp i (prf1 %% prf2) =
          (abshyp i prf1 %% abshyph i prf2
            handle Same.SAME => prf1 %% abshyp i prf2)
      | abshyp _ _ = raise Same.SAME
    and abshyph i prf = (abshyp i prf handle Same.SAME => prf);
  in
    AbsP ("H", f h, abshyph 0 prf)
  end;

val implies_intr_proof = gen_implies_intr_proof (K NONE);
val implies_intr_proof' = gen_implies_intr_proof SOME;


(***** forall introduction *****)

fun forall_intr_proof x a prf = Abst (a, NONE, prf_abstract_over x prf);

fun forall_intr_proof' t prf =
  let val (a, T) = (case t of Var ((a, _), T) => (a, T) | Free p => p)
  in Abst (a, SOME T, prf_abstract_over t prf) end;


(***** varify *****)

fun varify_proof t fixed prf =
  let
    val fs = Term.fold_types (Term.fold_atyps
      (fn TFree v => if member (op =) fixed v then I else insert (op =) v | _ => I)) t [];
    val used = Name.context
      |> fold_types (fold_atyps (fn TVar ((a, _), _) => Name.declare a | _ => I)) t;
    val fmap = fs ~~ #1 (fold_map Name.variant (map fst fs) used);
    fun thaw (f as (a, S)) =
      (case AList.lookup (op =) fmap f of
        NONE => TFree f
      | SOME b => TVar ((b, 0), S));
  in map_proof_terms (map_types (map_type_tfree thaw)) (map_type_tfree thaw) prf end;


local

fun new_name ix (pairs, used) =
  let val v = singleton (Name.variant_list used) (string_of_indexname ix)
  in ((ix, v) :: pairs, v :: used) end;

fun freeze_one alist (ix, sort) =
  (case AList.lookup (op =) alist ix of
    NONE => TVar (ix, sort)
  | SOME name => TFree (name, sort));

in

fun legacy_freezeT t prf =
  let
    val used = Term.add_tfree_names t [];
    val (alist, _) = fold_rev new_name (map #1 (Term.add_tvars t [])) ([], used);
  in
    (case alist of
      [] => prf (*nothing to do!*)
    | _ =>
        let val frzT = map_type_tvar (freeze_one alist)
        in map_proof_terms (map_types frzT) frzT prf end)
  end;

end;


(***** rotate assumptions *****)

fun rotate_proof Bs Bi m prf =
  let
    val params = Term.strip_all_vars Bi;
    val asms = Logic.strip_imp_prems (Term.strip_all_body Bi);
    val i = length asms;
    val j = length Bs;
  in
    mk_AbsP (j+1, proof_combP (prf, map PBound
      (j downto 1) @ [mk_Abst params (mk_AbsP (i,
        proof_combP (proof_combt (PBound i, map Bound ((length params - 1) downto 0)),
          map PBound (((i-m-1) downto 0) @ ((i-1) downto (i-m))))))]))
  end;


(***** permute premises *****)

fun permute_prems_proof prems j k prf =
  let val n = length prems
  in mk_AbsP (n, proof_combP (prf,
    map PBound ((n-1 downto n-j) @ (k-1 downto 0) @ (n-j-1 downto k))))
  end;


(***** generalization *****)

fun generalize (tfrees, frees) idx =
  Same.commit (map_proof_terms_same
    (Term_Subst.generalize_same (tfrees, frees) idx)
    (Term_Subst.generalizeT_same tfrees idx));


(***** instantiation *****)

fun instantiate (instT, inst) =
  Same.commit (map_proof_terms_same
    (Term_Subst.instantiate_same (instT, map (apsnd remove_types) inst))
    (Term_Subst.instantiateT_same instT));


(***** lifting *****)

fun lift_proof Bi inc prop prf =
  let
    fun lift'' Us Ts t =
      strip_abs Ts (Logic.incr_indexes ([], Us, inc) (mk_abs Ts t));

    fun lift' Us Ts (Abst (s, T, prf)) =
          (Abst (s, Same.map_option (Logic.incr_tvar_same inc) T, lifth' Us (dummyT::Ts) prf)
           handle Same.SAME => Abst (s, T, lift' Us (dummyT::Ts) prf))
      | lift' Us Ts (AbsP (s, t, prf)) =
          (AbsP (s, Same.map_option (same (op =) (lift'' Us Ts)) t, lifth' Us Ts prf)
           handle Same.SAME => AbsP (s, t, lift' Us Ts prf))
      | lift' Us Ts (prf % t) = (lift' Us Ts prf % Option.map (lift'' Us Ts) t
          handle Same.SAME => prf % Same.map_option (same (op =) (lift'' Us Ts)) t)
      | lift' Us Ts (prf1 %% prf2) = (lift' Us Ts prf1 %% lifth' Us Ts prf2
          handle Same.SAME => prf1 %% lift' Us Ts prf2)
      | lift' _ _ (PAxm (s, prop, Ts)) =
          PAxm (s, prop, (Same.map_option o Same.map) (Logic.incr_tvar_same inc) Ts)
      | lift' _ _ (OfClass (T, c)) =
          OfClass (Logic.incr_tvar_same inc T, c)
      | lift' _ _ (Oracle (s, prop, Ts)) =
          Oracle (s, prop, (Same.map_option o Same.map) (Logic.incr_tvar_same inc) Ts)
      | lift' _ _ (Promise (i, prop, Ts)) =
          Promise (i, prop, Same.map (Logic.incr_tvar_same inc) Ts)
      | lift' _ _ (PThm (i, ((s, prop, Ts), body))) =
          PThm (i, ((s, prop, (Same.map_option o Same.map) (Logic.incr_tvar inc) Ts), body))
      | lift' _ _ _ = raise Same.SAME
    and lifth' Us Ts prf = (lift' Us Ts prf handle Same.SAME => prf);

    val ps = map (Logic.lift_all inc Bi) (Logic.strip_imp_prems prop);
    val k = length ps;

    fun mk_app b (i, j, prf) =
          if b then (i-1, j, prf %% PBound i) else (i, j-1, prf %> Bound j);

    fun lift Us bs i j (Const ("Pure.imp", _) $ A $ B) =
            AbsP ("H", NONE (*A*), lift Us (true::bs) (i+1) j B)
      | lift Us bs i j (Const ("Pure.all", _) $ Abs (a, T, t)) =
            Abst (a, NONE (*T*), lift (T::Us) (false::bs) i (j+1) t)
      | lift Us bs i j _ = proof_combP (lifth' (rev Us) [] prf,
            map (fn k => (#3 (fold_rev mk_app bs (i-1, j-1, PBound k))))
              (i + k - 1 downto i));
  in
    mk_AbsP (k, lift [] [] 0 0 Bi)
  end;

fun incr_indexes i =
  Same.commit (map_proof_terms_same
    (Logic.incr_indexes_same ([], [], i)) (Logic.incr_tvar_same i));


(***** proof by assumption *****)

fun mk_asm_prf t i m =
  let
    fun imp_prf _ i 0 = PBound i
      | imp_prf (Const ("Pure.imp", _) $ A $ B) i m = AbsP ("H", NONE (*A*), imp_prf B (i+1) (m-1))
      | imp_prf _ i _ = PBound i;
    fun all_prf (Const ("Pure.all", _) $ Abs (a, T, t)) = Abst (a, NONE (*T*), all_prf t)
      | all_prf t = imp_prf t (~i) m
  in all_prf t end;

fun assumption_proof Bs Bi n prf =
  mk_AbsP (length Bs, proof_combP (prf,
    map PBound (length Bs - 1 downto 0) @ [mk_asm_prf Bi n ~1]));


(***** Composition of object rule with proof state *****)

fun flatten_params_proof i j n (Const ("Pure.imp", _) $ A $ B, k) =
      AbsP ("H", NONE (*A*), flatten_params_proof (i+1) j n (B, k))
  | flatten_params_proof i j n (Const ("Pure.all", _) $ Abs (a, T, t), k) =
      Abst (a, NONE (*T*), flatten_params_proof i (j+1) n (t, k))
  | flatten_params_proof i j n (_, k) = proof_combP (proof_combt (PBound (k+i),
      map Bound (j-1 downto 0)), map PBound (remove (op =) (i-n) (i-1 downto 0)));

fun bicompose_proof flatten Bs oldAs newAs A n m rprf sprf =
  let
    val la = length newAs;
    val lb = length Bs;
  in
    mk_AbsP (lb+la, proof_combP (sprf,
      map PBound (lb + la - 1 downto la)) %%
        proof_combP (rprf, (if n>0 then [mk_asm_prf (the A) n m] else []) @
          map (if flatten then flatten_params_proof 0 0 n else PBound o snd)
            (oldAs ~~ (la - 1 downto 0))))
  end;


(***** axioms for equality *****)

val aT = TFree ("'a", []);
val bT = TFree ("'b", []);
val x = Free ("x", aT);
val y = Free ("y", aT);
val z = Free ("z", aT);
val A = Free ("A", propT);
val B = Free ("B", propT);
val f = Free ("f", aT --> bT);
val g = Free ("g", aT --> bT);

val equality_axms =
 [("reflexive", Logic.mk_equals (x, x)),
  ("symmetric", Logic.mk_implies (Logic.mk_equals (x, y), Logic.mk_equals (y, x))),
  ("transitive",
    Logic.list_implies ([Logic.mk_equals (x, y), Logic.mk_equals (y, z)], Logic.mk_equals (x, z))),
  ("equal_intr",
    Logic.list_implies ([Logic.mk_implies (A, B), Logic.mk_implies (B, A)], Logic.mk_equals (A, B))),
  ("equal_elim", Logic.list_implies ([Logic.mk_equals (A, B), A], B)),
  ("abstract_rule",
    Logic.mk_implies
      (Logic.all x
        (Logic.mk_equals (f $ x, g $ x)), Logic.mk_equals (lambda x (f $ x), lambda x (g $ x)))),
  ("combination", Logic.list_implies
    ([Logic.mk_equals (f, g), Logic.mk_equals (x, y)], Logic.mk_equals (f $ x, g $ y)))];

val [reflexive_axm, symmetric_axm, transitive_axm, equal_intr_axm,
  equal_elim_axm, abstract_rule_axm, combination_axm] =
    map (fn (s, t) => PAxm ("Pure." ^ s, Logic.varify_global t, NONE)) equality_axms;

val reflexive = reflexive_axm % NONE;

fun symmetric (prf as PAxm ("Pure.reflexive", _, _) % _) = prf
  | symmetric prf = symmetric_axm % NONE % NONE %% prf;

fun transitive _ _ (PAxm ("Pure.reflexive", _, _) % _) prf2 = prf2
  | transitive _ _ prf1 (PAxm ("Pure.reflexive", _, _) % _) = prf1
  | transitive u (Type ("prop", [])) prf1 prf2 =
      transitive_axm % NONE % SOME (remove_types u) % NONE %% prf1 %% prf2
  | transitive u T prf1 prf2 =
      transitive_axm % NONE % NONE % NONE %% prf1 %% prf2;

fun abstract_rule x a prf =
  abstract_rule_axm % NONE % NONE %% forall_intr_proof x a prf;

fun check_comb (PAxm ("Pure.combination", _, _) % f % g % _ % _ %% prf %% _) =
      is_some f orelse check_comb prf
  | check_comb (PAxm ("Pure.transitive", _, _) % _ % _ % _ %% prf1 %% prf2) =
      check_comb prf1 andalso check_comb prf2
  | check_comb (PAxm ("Pure.symmetric", _, _) % _ % _ %% prf) = check_comb prf
  | check_comb _ = false;

fun combination f g t u (Type (_, [T, U])) prf1 prf2 =
  let
    val f = Envir.beta_norm f;
    val g = Envir.beta_norm g;
    val prf =
      if check_comb prf1 then
        combination_axm % NONE % NONE
      else
        (case prf1 of
          PAxm ("Pure.reflexive", _, _) % _ =>
            combination_axm %> remove_types f % NONE
        | _ => combination_axm %> remove_types f %> remove_types g)
  in
    (case T of
      Type ("fun", _) => prf %
        (case head_of f of
          Abs _ => SOME (remove_types t)
        | Var _ => SOME (remove_types t)
        | _ => NONE) %
        (case head_of g of
           Abs _ => SOME (remove_types u)
        | Var _ => SOME (remove_types u)
        | _ => NONE) %% prf1 %% prf2
     | _ => prf % NONE % NONE %% prf1 %% prf2)
  end;

fun equal_intr A B prf1 prf2 =
  equal_intr_axm %> remove_types A %> remove_types B %% prf1 %% prf2;

fun equal_elim A B prf1 prf2 =
  equal_elim_axm %> remove_types A %> remove_types B %% prf1 %% prf2;


(**** type classes ****)

fun strip_shyps_proof algebra present witnessed extra_sorts prf =
  let
    fun get S2 (T, S1) = if Sorts.sort_le algebra (S1, S2) then SOME T else NONE;
    val extra = map (fn S => (TFree ("'dummy", S), S)) extra_sorts;
    val replacements = present @ extra @ witnessed;
    fun replace T =
      if exists (fn (T', _) => T' = T) present then raise Same.SAME
      else
        (case get_first (get (Type.sort_of_atyp T)) replacements of
          SOME T' => T'
        | NONE => raise Fail "strip_shyps_proof: bad type variable in proof term");
  in Same.commit (map_proof_types_same (Term_Subst.map_atypsT_same replace)) prf end;


local

type axclass_proofs =
 {classrel_proof: theory -> class * class -> proof,
  arity_proof: theory -> string * sort list * class -> proof};

val axclass_proofs: axclass_proofs Single_Assignment.var =
  Single_Assignment.var "Proofterm.axclass_proofs";

fun axclass_proof which thy x =
  (case Single_Assignment.peek axclass_proofs of
    NONE => raise Fail "Axclass proof operations not installed"
  | SOME prfs => which prfs thy x);

in

val classrel_proof = axclass_proof #classrel_proof;
val arity_proof = axclass_proof #arity_proof;

fun install_axclass_proofs prfs = Single_Assignment.assign axclass_proofs prfs;

end;


local

fun canonical_instance typs =
  let
    val names = Name.invent Name.context Name.aT (length typs);
    val instT = map2 (fn a => fn T => (((a, 0), []), Type.strip_sorts T)) names typs;
  in instantiate (instT, []) end;

in

fun of_sort_proof thy hyps =
  Sorts.of_sort_derivation (Sign.classes_of thy)
   {class_relation = fn typ => fn (prf, c1) => fn c2 =>
      if c1 = c2 then prf
      else canonical_instance [typ] (classrel_proof thy (c1, c2)) %% prf,
    type_constructor = fn (a, typs) => fn dom => fn c =>
      let val Ss = map (map snd) dom and prfs = maps (map fst) dom
      in proof_combP (canonical_instance typs (arity_proof thy (a, Ss, c)), prfs) end,
    type_variable = fn typ => map (fn c => (hyps (typ, c), c)) (Type.sort_of_atyp typ)};

end;


(***** axioms and theorems *****)

val proofs = Unsynchronized.ref 2;
fun proofs_enabled () = ! proofs >= 2;

fun vars_of t = map Var (rev (Term.add_vars t []));
fun frees_of t = map Free (rev (Term.add_frees t []));

fun test_args _ [] = true
  | test_args is (Bound i :: ts) =
      not (member (op =) is i) andalso test_args (i :: is) ts
  | test_args _ _ = false;

fun is_fun (Type ("fun", _)) = true
  | is_fun (TVar _) = true
  | is_fun _ = false;

fun add_funvars Ts (vs, t) =
  if is_fun (fastype_of1 (Ts, t)) then
    union (op =) vs (map_filter (fn Var (ixn, T) =>
      if is_fun T then SOME ixn else NONE | _ => NONE) (vars_of t))
  else vs;

fun add_npvars q p Ts (vs, Const ("Pure.imp", _) $ t $ u) =
      add_npvars q p Ts (add_npvars q (not p) Ts (vs, t), u)
  | add_npvars q p Ts (vs, Const ("Pure.all", Type (_, [Type (_, [T, _]), _])) $ t) =
      add_npvars q p Ts (vs, if p andalso q then betapply (t, Var (("",0), T)) else t)
  | add_npvars q p Ts (vs, Abs (_, T, t)) = add_npvars q p (T::Ts) (vs, t)
  | add_npvars _ _ Ts (vs, t) = add_npvars' Ts (vs, t)
and add_npvars' Ts (vs, t) = (case strip_comb t of
    (Var (ixn, _), ts) => if test_args [] ts then vs
      else Library.foldl (add_npvars' Ts)
        (AList.update (op =) (ixn,
          Library.foldl (add_funvars Ts) ((these ooo AList.lookup) (op =) vs ixn, ts)) vs, ts)
  | (Abs (_, T, u), ts) => Library.foldl (add_npvars' (T::Ts)) (vs, u :: ts)
  | (_, ts) => Library.foldl (add_npvars' Ts) (vs, ts));

fun prop_vars (Const ("Pure.imp", _) $ P $ Q) = union (op =) (prop_vars P) (prop_vars Q)
  | prop_vars (Const ("Pure.all", _) $ Abs (_, _, t)) = prop_vars t
  | prop_vars t = (case strip_comb t of
      (Var (ixn, _), _) => [ixn] | _ => []);

fun is_proj t =
  let
    fun is_p i t = (case strip_comb t of
        (Bound j, []) => false
      | (Bound j, ts) => j >= i orelse exists (is_p i) ts
      | (Abs (_, _, u), _) => is_p (i+1) u
      | (_, ts) => exists (is_p i) ts)
  in (case strip_abs_body t of
        Bound _ => true
      | t' => is_p 0 t')
  end;

fun needed_vars prop =
  union (op =) (Library.foldl (uncurry (union (op =)))
    ([], map (uncurry (insert (op =))) (add_npvars true true [] ([], prop))))
  (prop_vars prop);

fun gen_axm_proof c name prop =
  let
    val nvs = needed_vars prop;
    val args = map (fn (v as Var (ixn, _)) =>
        if member (op =) nvs ixn then SOME v else NONE) (vars_of prop) @
      map SOME (frees_of prop);
  in
    proof_combt' (c (name, prop, NONE), args)
  end;

val axm_proof = gen_axm_proof PAxm;

fun oracle_proof name prop =
  if ! proofs = 0 then ((name, Term.dummy), Oracle (name, Term.dummy, NONE))
  else ((name, prop), gen_axm_proof Oracle name prop);

fun shrink_proof thy =
  let
    fun shrink ls lev (prf as Abst (a, T, body)) =
          let val (b, is, ch, body') = shrink ls (lev+1) body
          in (b, is, ch, if ch then Abst (a, T, body') else prf) end
      | shrink ls lev (prf as AbsP (a, t, body)) =
          let val (b, is, ch, body') = shrink (lev::ls) lev body
          in (b orelse member (op =) is 0, map_filter (fn 0 => NONE | i => SOME (i-1)) is,
            ch, if ch then AbsP (a, t, body') else prf)
          end
      | shrink ls lev prf =
          let val (is, ch, _, prf') = shrink' ls lev [] [] prf
          in (false, is, ch, prf') end
    and shrink' ls lev ts prfs (prf as prf1 %% prf2) =
          let
            val p as (_, is', ch', prf') = shrink ls lev prf2;
            val (is, ch, ts', prf'') = shrink' ls lev ts (p::prfs) prf1
          in (union (op =) is is', ch orelse ch', ts',
              if ch orelse ch' then prf'' %% prf' else prf)
          end
      | shrink' ls lev ts prfs (prf as prf1 % t) =
          let val (is, ch, (ch', t')::ts', prf') = shrink' ls lev (t::ts) prfs prf1
          in (is, ch orelse ch', ts',
              if ch orelse ch' then prf' % t' else prf) end
      | shrink' ls lev ts prfs (prf as PBound i) =
          (if exists (fn SOME (Bound j) => lev-j <= nth ls i | _ => true) ts
             orelse has_duplicates (op =)
               (Library.foldl (fn (js, SOME (Bound j)) => j :: js | (js, _) => js) ([], ts))
             orelse exists #1 prfs then [i] else [], false, map (pair false) ts, prf)
      | shrink' ls lev ts prfs (Hyp t) = ([], false, map (pair false) ts, Hyp t)
      | shrink' ls lev ts prfs (prf as MinProof) = ([], false, map (pair false) ts, prf)
      | shrink' ls lev ts prfs (prf as OfClass _) = ([], false, map (pair false) ts, prf)
      | shrink' ls lev ts prfs prf =
          let
            val prop =
              (case prf of
                PAxm (_, prop, _) => prop
              | Oracle (_, prop, _) => prop
              | Promise (_, prop, _) => prop
              | PThm (_, ((_, prop, _), _)) => prop
              | _ => raise Fail "shrink: proof not in normal form");
            val vs = vars_of prop;
            val (ts', ts'') = chop (length vs) ts;
            val insts = take (length ts') (map (fst o dest_Var) vs) ~~ ts';
            val nvs = Library.foldl (fn (ixns', (ixn, ixns)) =>
              insert (op =) ixn (case AList.lookup (op =) insts ixn of
                  SOME (SOME t) => if is_proj t then union (op =) ixns ixns' else ixns'
                | _ => union (op =) ixns ixns'))
                  (needed prop ts'' prfs, add_npvars false true [] ([], prop));
            val insts' = map
              (fn (ixn, x as SOME _) => if member (op =) nvs ixn then (false, x) else (true, NONE)
                | (_, x) => (false, x)) insts
          in ([], false, insts' @ map (pair false) ts'', prf) end
    and needed (Const ("Pure.imp", _) $ t $ u) ts ((b, _, _, _)::prfs) =
          union (op =) (if b then map (fst o dest_Var) (vars_of t) else []) (needed u ts prfs)
      | needed (Var (ixn, _)) (_::_) _ = [ixn]
      | needed _ _ _ = [];
  in shrink end;


(**** Simple first order matching functions for terms and proofs ****)

exception PMatch;

(** see pattern.ML **)

fun flt (i: int) = filter (fn n => n < i);

fun fomatch Ts tymatch j instsp p =
  let
    fun mtch (instsp as (tyinsts, insts)) = fn
        (Var (ixn, T), t)  =>
          if j>0 andalso not (null (flt j (loose_bnos t)))
          then raise PMatch
          else (tymatch (tyinsts, fn () => (T, fastype_of1 (Ts, t))),
            (ixn, t) :: insts)
      | (Free (a, T), Free (b, U)) =>
          if a=b then (tymatch (tyinsts, K (T, U)), insts) else raise PMatch
      | (Const (a, T), Const (b, U))  =>
          if a=b then (tymatch (tyinsts, K (T, U)), insts) else raise PMatch
      | (f $ t, g $ u) => mtch (mtch instsp (f, g)) (t, u)
      | (Bound i, Bound j) => if i=j then instsp else raise PMatch
      | _ => raise PMatch
  in mtch instsp (apply2 Envir.beta_eta_contract p) end;

fun match_proof Ts tymatch =
  let
    fun optmatch _ inst (NONE, _) = inst
      | optmatch _ _ (SOME _, NONE) = raise PMatch
      | optmatch mtch inst (SOME x, SOME y) = mtch inst (x, y)

    fun matcht Ts j (pinst, tinst) (t, u) =
      (pinst, fomatch Ts tymatch j tinst (t, Envir.beta_norm u));
    fun matchT (pinst, (tyinsts, insts)) p =
      (pinst, (tymatch (tyinsts, K p), insts));
    fun matchTs inst (Ts, Us) = Library.foldl (uncurry matchT) (inst, Ts ~~ Us);

    fun mtch Ts i j (pinst, tinst) (Hyp (Var (ixn, _)), prf) =
          if i = 0 andalso j = 0 then ((ixn, prf) :: pinst, tinst)
          else (case apfst (flt i) (apsnd (flt j)
                  (prf_add_loose_bnos 0 0 prf ([], []))) of
              ([], []) => ((ixn, incr_pboundvars (~i) (~j) prf) :: pinst, tinst)
            | ([], _) => if j = 0 then
                   ((ixn, incr_pboundvars (~i) (~j) prf) :: pinst, tinst)
                 else raise PMatch
            | _ => raise PMatch)
      | mtch Ts i j inst (prf1 % opt1, prf2 % opt2) =
          optmatch (matcht Ts j) (mtch Ts i j inst (prf1, prf2)) (opt1, opt2)
      | mtch Ts i j inst (prf1 %% prf2, prf1' %% prf2') =
          mtch Ts i j (mtch Ts i j inst (prf1, prf1')) (prf2, prf2')
      | mtch Ts i j inst (Abst (_, opT, prf1), Abst (_, opU, prf2)) =
          mtch (the_default dummyT opU :: Ts) i (j+1)
            (optmatch matchT inst (opT, opU)) (prf1, prf2)
      | mtch Ts i j inst (prf1, Abst (_, opU, prf2)) =
          mtch (the_default dummyT opU :: Ts) i (j+1) inst
            (incr_pboundvars 0 1 prf1 %> Bound 0, prf2)
      | mtch Ts i j inst (AbsP (_, opt, prf1), AbsP (_, opu, prf2)) =
          mtch Ts (i+1) j (optmatch (matcht Ts j) inst (opt, opu)) (prf1, prf2)
      | mtch Ts i j inst (prf1, AbsP (_, _, prf2)) =
          mtch Ts (i+1) j inst (incr_pboundvars 1 0 prf1 %% PBound 0, prf2)
      | mtch Ts i j inst (PAxm (s1, _, opTs), PAxm (s2, _, opUs)) =
          if s1 = s2 then optmatch matchTs inst (opTs, opUs)
          else raise PMatch
      | mtch Ts i j inst (OfClass (T1, c1), OfClass (T2, c2)) =
          if c1 = c2 then matchT inst (T1, T2)
          else raise PMatch
      | mtch Ts i j inst (PThm (_, ((name1, prop1, opTs), _)), PThm (_, ((name2, prop2, opUs), _))) =
          if name1 = name2 andalso prop1 = prop2 then
            optmatch matchTs inst (opTs, opUs)
          else raise PMatch
      | mtch _ _ _ inst (PBound i, PBound j) = if i = j then inst else raise PMatch
      | mtch _ _ _ _ _ = raise PMatch
  in mtch Ts 0 0 end;

fun prf_subst (pinst, (tyinsts, insts)) =
  let
    val substT = Envir.subst_type_same tyinsts;
    val substTs = Same.map substT;

    fun subst' lev (Var (xi, _)) =
        (case AList.lookup (op =) insts xi of
          NONE => raise Same.SAME
        | SOME u => incr_boundvars lev u)
      | subst' _ (Const (s, T)) = Const (s, substT T)
      | subst' _ (Free (s, T)) = Free (s, substT T)
      | subst' lev (Abs (a, T, body)) =
          (Abs (a, substT T, Same.commit (subst' (lev + 1)) body)
            handle Same.SAME => Abs (a, T, subst' (lev + 1) body))
      | subst' lev (f $ t) =
          (subst' lev f $ Same.commit (subst' lev) t
            handle Same.SAME => f $ subst' lev t)
      | subst' _ _ = raise Same.SAME;

    fun subst plev tlev (AbsP (a, t, body)) =
          (AbsP (a, Same.map_option (subst' tlev) t, Same.commit (subst (plev + 1) tlev) body)
            handle Same.SAME => AbsP (a, t, subst (plev + 1) tlev body))
      | subst plev tlev (Abst (a, T, body)) =
          (Abst (a, Same.map_option substT T, Same.commit (subst plev (tlev + 1)) body)
            handle Same.SAME => Abst (a, T, subst plev (tlev + 1) body))
      | subst plev tlev (prf %% prf') =
          (subst plev tlev prf %% Same.commit (subst plev tlev) prf'
            handle Same.SAME => prf %% subst plev tlev prf')
      | subst plev tlev (prf % t) =
          (subst plev tlev prf % Same.commit (Same.map_option (subst' tlev)) t
            handle Same.SAME => prf % Same.map_option (subst' tlev) t)
      | subst plev tlev (Hyp (Var (xi, _))) =
          (case AList.lookup (op =) pinst xi of
            NONE => raise Same.SAME
          | SOME prf' => incr_pboundvars plev tlev prf')
      | subst _ _ (PAxm (id, prop, Ts)) = PAxm (id, prop, Same.map_option substTs Ts)
      | subst _ _ (OfClass (T, c)) = OfClass (substT T, c)
      | subst _ _ (Oracle (id, prop, Ts)) = Oracle (id, prop, Same.map_option substTs Ts)
      | subst _ _ (Promise (i, prop, Ts)) = Promise (i, prop, substTs Ts)
      | subst _ _ (PThm (i, ((id, prop, Ts), body))) =
          PThm (i, ((id, prop, Same.map_option substTs Ts), body))
      | subst _ _ _ = raise Same.SAME;
  in fn t => subst 0 0 t handle Same.SAME => t end;

(*A fast unification filter: true unless the two terms cannot be unified.
  Terms must be NORMAL.  Treats all Vars as distinct. *)
fun could_unify prf1 prf2 =
  let
    fun matchrands (prf1 %% prf2) (prf1' %% prf2') =
          could_unify prf2 prf2' andalso matchrands prf1 prf1'
      | matchrands (prf % SOME t) (prf' % SOME t') =
          Term.could_unify (t, t') andalso matchrands prf prf'
      | matchrands (prf % _) (prf' % _) = matchrands prf prf'
      | matchrands _ _ = true

    fun head_of (prf %% _) = head_of prf
      | head_of (prf % _) = head_of prf
      | head_of prf = prf

  in case (head_of prf1, head_of prf2) of
        (_, Hyp (Var _)) => true
      | (Hyp (Var _), _) => true
      | (PAxm (a, _, _), PAxm (b, _, _)) => a = b andalso matchrands prf1 prf2
      | (OfClass (_, c), OfClass (_, d)) => c = d andalso matchrands prf1 prf2
      | (PThm (_, ((a, propa, _), _)), PThm (_, ((b, propb, _), _))) =>
          a = b andalso propa = propb andalso matchrands prf1 prf2
      | (PBound i, PBound j) => i = j andalso matchrands prf1 prf2
      | (AbsP _, _) =>  true   (*because of possible eta equality*)
      | (Abst _, _) =>  true
      | (_, AbsP _) =>  true
      | (_, Abst _) =>  true
      | _ => false
  end;


(**** rewriting on proof terms ****)

val no_skel = PBound 0;
val normal_skel = Hyp (Var ((Name.uu, 0), propT));

fun rewrite_prf tymatch (rules, procs) prf =
  let
    fun rew _ _ (Abst (_, _, body) % SOME t) = SOME (prf_subst_bounds [t] body, no_skel)
      | rew _ _ (AbsP (_, _, body) %% prf) = SOME (prf_subst_pbounds [prf] body, no_skel)
      | rew Ts hs prf =
          (case get_first (fn r => r Ts hs prf) procs of
            NONE => get_first (fn (prf1, prf2) => SOME (prf_subst
              (match_proof Ts tymatch ([], (Vartab.empty, [])) (prf1, prf)) prf2, prf2)
                 handle PMatch => NONE) (filter (could_unify prf o fst) rules)
          | some => some);

    fun rew0 Ts hs (prf as AbsP (_, _, prf' %% PBound 0)) =
          if prf_loose_Pbvar1 prf' 0 then rew Ts hs prf
          else
            let val prf'' = incr_pboundvars (~1) 0 prf'
            in SOME (the_default (prf'', no_skel) (rew Ts hs prf'')) end
      | rew0 Ts hs (prf as Abst (_, _, prf' % SOME (Bound 0))) =
          if prf_loose_bvar1 prf' 0 then rew Ts hs prf
          else
            let val prf'' = incr_pboundvars 0 (~1) prf'
            in SOME (the_default (prf'', no_skel) (rew Ts hs prf'')) end
      | rew0 Ts hs prf = rew Ts hs prf;

    fun rew1 _ _ (Hyp (Var _)) _ = NONE
      | rew1 Ts hs skel prf = (case rew2 Ts hs skel prf of
          SOME prf1 => (case rew0 Ts hs prf1 of
              SOME (prf2, skel') => SOME (the_default prf2 (rew1 Ts hs skel' prf2))
            | NONE => SOME prf1)
        | NONE => (case rew0 Ts hs prf of
              SOME (prf1, skel') => SOME (the_default prf1 (rew1 Ts hs skel' prf1))
            | NONE => NONE))

    and rew2 Ts hs skel (prf % SOME t) = (case prf of
            Abst (_, _, body) =>
              let val prf' = prf_subst_bounds [t] body
              in SOME (the_default prf' (rew2 Ts hs no_skel prf')) end
          | _ => (case rew1 Ts hs (case skel of skel' % _ => skel' | _ => no_skel) prf of
              SOME prf' => SOME (prf' % SOME t)
            | NONE => NONE))
      | rew2 Ts hs skel (prf % NONE) = Option.map (fn prf' => prf' % NONE)
          (rew1 Ts hs (case skel of skel' % _ => skel' | _ => no_skel) prf)
      | rew2 Ts hs skel (prf1 %% prf2) = (case prf1 of
            AbsP (_, _, body) =>
              let val prf' = prf_subst_pbounds [prf2] body
              in SOME (the_default prf' (rew2 Ts hs no_skel prf')) end
          | _ =>
            let val (skel1, skel2) = (case skel of
                skel1 %% skel2 => (skel1, skel2)
              | _ => (no_skel, no_skel))
            in case rew1 Ts hs skel1 prf1 of
                SOME prf1' => (case rew1 Ts hs skel2 prf2 of
                    SOME prf2' => SOME (prf1' %% prf2')
                  | NONE => SOME (prf1' %% prf2))
              | NONE => (case rew1 Ts hs skel2 prf2 of
                    SOME prf2' => SOME (prf1 %% prf2')
                  | NONE => NONE)
            end)
      | rew2 Ts hs skel (Abst (s, T, prf)) = (case rew1 (the_default dummyT T :: Ts) hs
              (case skel of Abst (_, _, skel') => skel' | _ => no_skel) prf of
            SOME prf' => SOME (Abst (s, T, prf'))
          | NONE => NONE)
      | rew2 Ts hs skel (AbsP (s, t, prf)) = (case rew1 Ts (t :: hs)
              (case skel of AbsP (_, _, skel') => skel' | _ => no_skel) prf of
            SOME prf' => SOME (AbsP (s, t, prf'))
          | NONE => NONE)
      | rew2 _ _ _ _ = NONE;

  in the_default prf (rew1 [] [] no_skel prf) end;

fun rewrite_proof thy = rewrite_prf (fn (tyenv, f) =>
  Sign.typ_match thy (f ()) tyenv handle Type.TYPE_MATCH => raise PMatch);

fun rewrite_proof_notypes rews = rewrite_prf fst rews;


(**** theory data ****)

structure Data = Theory_Data
(
  type T =
    (stamp * (proof * proof)) list *
    (stamp * (typ list -> term option list -> proof -> (proof * proof) option)) list;

  val empty = ([], []);
  val extend = I;
  fun merge ((rules1, procs1), (rules2, procs2)) : T =
    (AList.merge (op =) (K true) (rules1, rules2),
      AList.merge (op =) (K true) (procs1, procs2));
);

fun get_data thy = let val (rules, procs) = Data.get thy in (map #2 rules, map #2 procs) end;
fun rew_proof thy = rewrite_prf fst (get_data thy);

fun add_prf_rrule r = (Data.map o apfst) (cons (stamp (), r));
fun add_prf_rproc p = (Data.map o apsnd) (cons (stamp (), p));


(***** promises *****)

fun promise_proof thy i prop =
  let
    val _ = prop |> Term.exists_subterm (fn t =>
      (Term.is_Free t orelse Term.is_Var t) andalso
        raise Fail ("promise_proof: illegal variable " ^ Syntax.string_of_term_global thy t));
    val _ = prop |> Term.exists_type (Term.exists_subtype
      (fn TFree (a, _) => raise Fail ("promise_proof: illegal type variable " ^ quote a)
        | _ => false));
  in Promise (i, prop, map TVar (Term.add_tvars prop [])) end;

fun fulfill_norm_proof thy ps body0 =
  let
    val PBody {oracles = oracles0, thms = thms0, proof = proof0} = body0;
    val oracles =
      unions_oracles
        (fold (fn (_, PBody {oracles, ...}) => not (null oracles) ? cons oracles) ps [oracles0]);
    val thms =
      unions_thms (fold (fn (_, PBody {thms, ...}) => not (null thms) ? cons thms) ps [thms0]);
    val proofs = fold (fn (i, PBody {proof, ...}) => Inttab.update (i, proof)) ps Inttab.empty;

    fun fill (Promise (i, prop, Ts)) =
          (case Inttab.lookup proofs i of
            NONE => NONE
          | SOME prf => SOME (instantiate (Term.add_tvars prop [] ~~ Ts, []) prf, normal_skel))
      | fill _ = NONE;
    val (rules, procs) = get_data thy;
    val proof = rewrite_prf fst (rules, K (K fill) :: procs) proof0;
  in PBody {oracles = oracles, thms = thms, proof = proof} end;

fun fulfill_proof_future thy promises postproc body =
  let
    fun fulfill () =
      postproc (fulfill_norm_proof thy (map (apsnd Future.join) promises) (Future.join body));
  in
    if null promises then Future.map postproc body
    else if Future.is_finished body andalso length promises = 1 then
      Future.map (fn _ => fulfill ()) (snd (hd promises))
    else
      (singleton o Future.forks)
        {name = "Proofterm.fulfill_proof_future", group = NONE,
          deps = Future.task_of body :: map (Future.task_of o snd) promises, pri = 0,
          interrupts = true}
        fulfill
  end;


(***** abstraction over sort constraints *****)

fun unconstrainT_prf thy (atyp_map, constraints) =
  let
    fun hyp_map hyp =
      (case AList.lookup (op =) constraints hyp of
        SOME t => Hyp t
      | NONE => raise Fail "unconstrainT_prf: missing constraint");

    val typ = Term_Subst.map_atypsT_same (Type.strip_sorts o atyp_map);
    fun ofclass (ty, c) =
      let val ty' = Term.map_atyps atyp_map ty;
      in the_single (of_sort_proof thy hyp_map (ty', [c])) end;
  in
    Same.commit (map_proof_same (Term_Subst.map_types_same typ) typ ofclass)
    #> fold_rev (implies_intr_proof o snd) constraints
  end;

fun unconstrainT_body thy constrs (PBody {oracles, thms, proof}) =
  PBody
   {oracles = oracles,  (* FIXME merge (!), unconstrain (!?!) *)
    thms = thms,  (* FIXME merge (!) *)
    proof = unconstrainT_prf thy constrs proof};


(***** theorems *****)

fun prepare_thm_proof thy name shyps hyps concl promises body =
  let
    val PBody {oracles = oracles0, thms = thms0, proof = prf} = body;
    val prop = Logic.list_implies (hyps, concl);
    val nvs = needed_vars prop;
    val args = map (fn (v as Var (ixn, _)) =>
        if member (op =) nvs ixn then SOME v else NONE) (vars_of prop) @
      map SOME (frees_of prop);

    val ((atyp_map, constraints, outer_constraints), prop1) = Logic.unconstrainT shyps prop;
    val postproc = unconstrainT_body thy (atyp_map, constraints);
    val args1 =
      (map o Option.map o Term.map_types o Term.map_atyps)
        (Type.strip_sorts o atyp_map) args;
    val argsP = map OfClass outer_constraints @ map Hyp hyps;

    fun make_body0 proof0 = PBody {oracles = oracles0, thms = thms0, proof = proof0};
    val body0 =
      if not (proofs_enabled ()) then Future.value (make_body0 MinProof)
      else
        (singleton o Future.cond_forks)
          {name = "Proofterm.prepare_thm_proof", group = NONE,
            deps = [], pri = 0, interrupts = true}
          (fn () =>
            make_body0
              (#4 (shrink_proof thy [] 0 (rew_proof thy (fold_rev implies_intr_proof hyps prf)))));

    fun new_prf () = (serial (), fulfill_proof_future thy promises postproc body0);
    val (i, body') =
      (*non-deterministic, depends on unknown promises*)
      (case strip_combt (fst (strip_combP prf)) of
        (PThm (i, ((old_name, prop', NONE), body')), args') =>
          if (old_name = "" orelse old_name = name) andalso prop1 = prop' andalso args = args'
          then (i, body')
          else new_prf ()
      | _ => new_prf ());
    val head = PThm (i, ((name, prop1, NONE), body'));
  in ((i, (name, prop1, body')), head, args, argsP, args1) end;

fun thm_proof thy name shyps hyps concl promises body =
  let val (pthm, head, args, argsP, _) = prepare_thm_proof thy name shyps hyps concl promises body
  in (pthm, proof_combP (proof_combt' (head, args), argsP)) end;

fun unconstrain_thm_proof thy shyps concl promises body =
  let
    val (pthm, head, _, _, args) = prepare_thm_proof thy "" shyps [] concl promises body
  in (pthm, proof_combt' (head, args)) end;


fun get_name shyps hyps prop prf =
  let val (_, prop) = Logic.unconstrainT shyps (Logic.list_implies (hyps, prop)) in
    (case strip_combt (fst (strip_combP prf)) of
      (PThm (_, ((name, prop', _), _)), _) => if prop = prop' then name else ""
    | _ => "")
  end;

fun guess_name (PThm (_, ((name, _, _), _))) = name
  | guess_name (prf %% Hyp _) = guess_name prf
  | guess_name (prf %% OfClass _) = guess_name prf
  | guess_name (prf % NONE) = guess_name prf
  | guess_name (prf % SOME (Var _)) = guess_name prf
  | guess_name _ = "";

end;

structure Basic_Proofterm : BASIC_PROOFTERM = Proofterm;
open Basic_Proofterm;
