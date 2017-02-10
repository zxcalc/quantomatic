(*  Title:      Pure/ML/ml_pp.ML
    Author:     Makarius

ML toplevel pretty-printing for logical entities.
*)

structure ML_PP: sig end =
struct

val _ =
  ML_system_pp (fn _ => fn _ => Pretty.to_polyml o Proof_Display.pp_context);

val _ =
  ML_system_pp (fn depth => fn _ =>
    ML_Pretty.to_polyml o Pretty.to_ML depth o Proof_Display.pp_thm Thy_Info.pure_theory);

val _ =
  ML_system_pp (fn depth => fn _ =>
    ML_Pretty.to_polyml o Pretty.to_ML depth o Proof_Display.pp_cterm Thy_Info.pure_theory);

val _ =
  ML_system_pp (fn depth => fn _ =>
    ML_Pretty.to_polyml o Pretty.to_ML depth o Proof_Display.pp_ctyp Thy_Info.pure_theory);

val _ =
  ML_system_pp (fn depth => fn _ =>
    ML_Pretty.to_polyml o Pretty.to_ML depth o Proof_Display.pp_typ Thy_Info.pure_theory);


local

fun prt_app name prt = Pretty.block [Pretty.str (name ^ " "), prt];
fun prt_apps name = Pretty.enum "," (name ^ " (") ")";

fun prt_term parens (dp: FixedInt.int) t =
  if dp <= 0 then Pretty.str "..."
  else
    (case t of
      _ $ _ =>
        op :: (strip_comb t)
        |> map_index (fn (i, u) => prt_term true (dp - FixedInt.fromInt i - 1) u)
        |> Pretty.separate " $"
        |> (if parens then Pretty.enclose "(" ")" else Pretty.block)
    | Abs (a, T, b) =>
        prt_apps "Abs"
         [Pretty.from_polyml (ML_system_pretty (a, dp - 1)),
          Pretty.from_polyml (ML_system_pretty (T, dp - 2)),
          prt_term false (dp - 3) b]
    | Const a => prt_app "Const" (Pretty.from_polyml (ML_system_pretty (a, dp - 1)))
    | Free a => prt_app "Free" (Pretty.from_polyml (ML_system_pretty (a, dp - 1)))
    | Var a => prt_app "Var" (Pretty.from_polyml (ML_system_pretty (a, dp - 1)))
    | Bound a => prt_app "Bound" (Pretty.from_polyml (ML_system_pretty (a, dp - 1))));

in

val _ = ML_system_pp (fn depth => fn _ => Pretty.to_polyml o prt_term false depth);


local

fun prt_proof parens dp prf =
  if dp <= 0 then Pretty.str "..."
  else
    (case prf of
      _ % _ => prt_proofs parens dp prf
    | _ %% _ => prt_proofs parens dp prf
    | Abst (a, T, b) =>
        prt_apps "Abst"
         [Pretty.from_polyml (ML_system_pretty (a, dp - 1)),
          Pretty.from_polyml (ML_system_pretty (T, dp - 2)),
          prt_proof false (dp - 3) b]
    | AbsP (a, t, b) =>
        prt_apps "AbsP"
         [Pretty.from_polyml (ML_system_pretty (a, dp - 1)),
          Pretty.from_polyml (ML_system_pretty (t, dp - 2)),
          prt_proof false (dp - 3) b]
    | Hyp t => prt_app "Hyp" (prt_term true (dp - 1) t)
    | MinProof => Pretty.str "MinProof"
    | PBound a => prt_app "PBound" (Pretty.from_polyml (ML_system_pretty (a, dp - 1)))
    | PAxm a => prt_app "PAxm" (Pretty.from_polyml (ML_system_pretty (a, dp - 1)))
    | OfClass a => prt_app "OfClass" (Pretty.from_polyml (ML_system_pretty (a, dp - 1)))
    | Oracle a => prt_app "Oracle" (Pretty.from_polyml (ML_system_pretty (a, dp - 1)))
    | Promise a => prt_app "Promise" (Pretty.from_polyml (ML_system_pretty (a, dp - 1)))
    | PThm a => prt_app "PThm" (Pretty.from_polyml (ML_system_pretty (a, dp - 1))))

and prt_proofs parens dp prf =
  let
    val (head, args) = strip_proof prf [];
    val prts =
      head (dp - 1) :: flat (map_index (fn (i, prt) => prt (dp - FixedInt.fromInt i - 2)) args);
  in if parens then Pretty.enclose "(" ")" prts else Pretty.block prts end

and strip_proof (p % t) res =
      strip_proof p
        ((fn d =>
          [Pretty.str " %", Pretty.brk 1,
            Pretty.from_polyml (ML_system_pretty (t, d))]) :: res)
  | strip_proof (p %% q) res =
      strip_proof p ((fn d => [Pretty.str " %%", Pretty.brk 1, prt_proof true d q]) :: res)
  | strip_proof p res = (fn d => prt_proof true d p, res);

in

val _ =
  ML_system_pp (fn depth => fn _ => fn prf =>
    ML_Pretty.to_polyml (Pretty.to_ML ~1 (prt_proof false depth prf)));

end;

end;

end;
