(*  Title:      Pure/more_pattern.ML
    Author:     Tobias Nipkow, TU Muenchen
    Author:     Stefan Berghofer, TU Muenchen

Add-ons to Higher-Order Patterns, outside the inference kernel.
*)

signature PATTERN =
sig
  include PATTERN
  val matches: theory -> term * term -> bool
  val matchess: theory -> term list * term list -> bool
  val equiv: theory -> term * term -> bool
  val first_order: term -> bool
  val pattern: term -> bool
  val match_rew: theory -> term -> term * term -> (term * term) option
  val rewrite_term: theory -> (term * term) list -> (term -> term option) list -> term -> term
  val rewrite_term_top: theory -> (term * term) list -> (term -> term option) list -> term -> term
end;

structure Pattern: PATTERN =
struct

fun matches thy po =
  (Pattern.match thy po (Vartab.empty, Vartab.empty); true) handle Pattern.MATCH => false;

fun matchess thy (ps, os) =
  length ps = length os andalso
    ((fold (Pattern.match thy) (ps ~~ os) (Vartab.empty, Vartab.empty); true)
      handle Pattern.MATCH => false);

fun equiv thy (t, u) = matches thy (t, u) andalso matches thy (u, t);

fun first_order (Abs (_, _, t)) = first_order t
  | first_order (Var _ $ _) = false
  | first_order (t $ u) = first_order t andalso first_order u
  | first_order _ = true;

fun pattern (Abs (_, _, t)) = pattern t
  | pattern t =
      let val (head, args) = strip_comb t in
        if is_Var head then
          forall is_Bound args andalso not (has_duplicates (op aconv) args)
        else forall pattern args
      end;


(* rewriting -- simple but fast *)

fun match_rew thy tm (tm1, tm2) =
  let val rtm = the_default tm2 (Term.rename_abs tm1 tm tm2) in
    SOME (Envir.subst_term (Pattern.match thy (tm1, tm) (Vartab.empty, Vartab.empty)) rtm, rtm)
      handle Pattern.MATCH => NONE
  end;

fun gen_rewrite_term bot thy rules procs tm =
  let
    val skel0 = Bound 0;

    fun variant_absfree bounds (x, T, t) =
      let
        val (x', t') = Term.dest_abs (Name.bound bounds, T, t);
        fun abs u = Abs (x, T, abstract_over (Free (x', T), u));
      in (abs, t') end;

    fun rew (Abs (_, _, body) $ t) = SOME (subst_bound (t, body), skel0)
      | rew tm =
          (case get_first (match_rew thy tm) rules of
            NONE => Option.map (rpair skel0) (get_first (fn p => p tm) procs)
          | x => x);

    fun rew_sub r bounds skel (tm1 $ tm2) = (case tm1 of
            Abs (_, _, body) =>
              let val tm' = subst_bound (tm2, body)
              in SOME (the_default tm' (rew_sub r bounds skel0 tm')) end
          | _ =>
            let val (skel1, skel2) = (case skel of
                skel1 $ skel2 => (skel1, skel2)
              | _ => (skel0, skel0))
            in case r bounds skel1 tm1 of
                SOME tm1' => (case r bounds skel2 tm2 of
                    SOME tm2' => SOME (tm1' $ tm2')
                  | NONE => SOME (tm1' $ tm2))
              | NONE => (case r bounds skel2 tm2 of
                    SOME tm2' => SOME (tm1 $ tm2')
                  | NONE => NONE)
            end)
      | rew_sub r bounds skel (Abs body) =
          let
            val (abs, tm') = variant_absfree bounds body;
            val skel' = (case skel of Abs (_, _, skel') => skel' | _ => skel0)
          in case r (bounds + 1) skel' tm' of
              SOME tm'' => SOME (abs tm'')
            | NONE => NONE
          end
      | rew_sub _ _ _ _ = NONE;

    fun rew_bot bounds (Var _) _ = NONE
      | rew_bot bounds skel tm = (case rew_sub rew_bot bounds skel tm of
          SOME tm1 => (case rew tm1 of
              SOME (tm2, skel') => SOME (the_default tm2 (rew_bot bounds skel' tm2))
            | NONE => SOME tm1)
        | NONE => (case rew tm of
              SOME (tm1, skel') => SOME (the_default tm1 (rew_bot bounds skel' tm1))
            | NONE => NONE));

    fun rew_top bounds _ tm = (case rew tm of
          SOME (tm1, _) => (case rew_sub rew_top bounds skel0 tm1 of
              SOME tm2 => SOME (the_default tm2 (rew_top bounds skel0 tm2))
            | NONE => SOME tm1)
        | NONE => (case rew_sub rew_top bounds skel0 tm of
              SOME tm1 => SOME (the_default tm1 (rew_top bounds skel0 tm1))
            | NONE => NONE));

  in the_default tm ((if bot then rew_bot else rew_top) 0 skel0 tm) end;

val rewrite_term = gen_rewrite_term true;
val rewrite_term_top = gen_rewrite_term false;


open Pattern;

end;

