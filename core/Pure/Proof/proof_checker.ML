(*  Title:      Pure/Proof/proof_checker.ML
    Author:     Stefan Berghofer, TU Muenchen

Simple proof checker based only on the core inference rules
of Isabelle/Pure.
*)

signature PROOF_CHECKER =
sig
  val thm_of_proof : theory -> Proofterm.proof -> thm
end;

structure Proof_Checker : PROOF_CHECKER =
struct

(***** construct a theorem out of a proof term *****)

fun lookup_thm thy =
  let val tab = fold_rev Symtab.update (Global_Theory.all_thms_of thy true) Symtab.empty in
    fn s =>
      (case Symtab.lookup tab s of
        NONE => error ("Unknown theorem " ^ quote s)
      | SOME thm => thm)
  end;

val beta_eta_convert =
  Conv.fconv_rule Drule.beta_eta_conversion;

(* equality modulo renaming of type variables *)
fun is_equal t t' =
  let
    val atoms = fold_types (fold_atyps (insert (op =))) t [];
    val atoms' = fold_types (fold_atyps (insert (op =))) t' []
  in
    length atoms = length atoms' andalso
    map_types (map_atyps (the o AList.lookup (op =) (atoms ~~ atoms'))) t aconv t'
  end;

fun pretty_prf thy vs Hs prf =
  let val prf' = prf |> Proofterm.prf_subst_bounds (map Free vs) |>
    Proofterm.prf_subst_pbounds (map (Hyp o prop_of) Hs)
  in
    (Proof_Syntax.pretty_proof (Syntax.init_pretty_global thy) prf',
     Syntax.pretty_term_global thy (Reconstruct.prop_of prf'))
  end;

fun pretty_term thy vs _ t =
  let val t' = subst_bounds (map Free vs, t)
  in
    (Syntax.pretty_term_global thy t',
     Syntax.pretty_typ_global thy (fastype_of t'))
  end;

fun appl_error thy prt vs Hs s f a =
  let
    val (pp_f, pp_fT) = pretty_prf thy vs Hs f;
    val (pp_a, pp_aT) = prt thy vs Hs a
  in
    error (cat_lines
      [s,
       "",
       Pretty.string_of (Pretty.block
         [Pretty.str "Operator:", Pretty.brk 2, pp_f,
           Pretty.str " ::", Pretty.brk 1, pp_fT]),
       Pretty.string_of (Pretty.block
         [Pretty.str "Operand:", Pretty.brk 3, pp_a,
           Pretty.str " ::", Pretty.brk 1, pp_aT]),
       ""])
  end;

fun thm_of_proof thy =
  let val lookup = lookup_thm thy in
    fn prf =>
      let
        val prf_names = Proofterm.fold_proof_terms Term.declare_term_frees (K I) prf Name.context;

        fun thm_of_atom thm Ts =
          let
            val tvars = Term.add_tvars (Thm.full_prop_of thm) [] |> rev;
            val (fmap, thm') = Thm.varifyT_global' [] thm;
            val ctye = map (pairself (Thm.ctyp_of thy))
              (map TVar tvars @ map (fn ((_, S), ixn) => TVar (ixn, S)) fmap ~~ Ts)
          in
            Thm.instantiate (ctye, []) (forall_intr_vars (Thm.forall_intr_frees thm'))
          end;

        fun thm_of _ _ (PThm (_, ((name, prop', SOME Ts), _))) =
              let
                val thm = Thm.unconstrainT (Drule.implies_intr_hyps (lookup name));
                val prop = Thm.prop_of thm;
                val _ =
                  if is_equal prop prop' then ()
                  else
                    error ("Duplicate use of theorem name " ^ quote name ^ "\n" ^
                      Syntax.string_of_term_global thy prop ^ "\n\n" ^
                      Syntax.string_of_term_global thy prop');
              in thm_of_atom thm Ts end

          | thm_of _ _ (PAxm (name, _, SOME Ts)) =
              thm_of_atom (Thm.axiom thy name) Ts

          | thm_of _ Hs (PBound i) = nth Hs i

          | thm_of (vs, names) Hs (Abst (s, SOME T, prf)) =
              let
                val (x, names') = Name.variant s names;
                val thm = thm_of ((x, T) :: vs, names') Hs prf
              in
                Thm.forall_intr (Thm.cterm_of thy (Free (x, T))) thm
              end

          | thm_of (vs, names) Hs (prf % SOME t) =
              let
                val thm = thm_of (vs, names) Hs prf;
                val ct = Thm.cterm_of thy (Term.subst_bounds (map Free vs, t));
              in
                Thm.forall_elim ct thm
                handle THM (s, _, _) => appl_error thy pretty_term vs Hs s prf t
              end

          | thm_of (vs, names) Hs (AbsP (s, SOME t, prf)) =
              let
                val ct = Thm.cterm_of thy (Term.subst_bounds (map Free vs, t));
                val thm = thm_of (vs, names) (Thm.assume ct :: Hs) prf;
              in
                Thm.implies_intr ct thm
              end

          | thm_of vars Hs (prf %% prf') =
              let
                val thm = beta_eta_convert (thm_of vars Hs prf);
                val thm' = beta_eta_convert (thm_of vars Hs prf');
              in
                Thm.implies_elim thm thm'
                handle THM (s, _, _) => appl_error thy pretty_prf (fst vars) Hs s prf prf'
              end

          | thm_of _ _ (Hyp t) = Thm.assume (Thm.cterm_of thy t)

          | thm_of _ _ _ = error "thm_of_proof: partial proof term";

      in beta_eta_convert (thm_of ([], prf_names) [] prf) end
  end;

end;
