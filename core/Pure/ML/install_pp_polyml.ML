(*  Title:      Pure/ML/install_pp_polyml.ML
    Author:     Makarius

Extra toplevel pretty-printing for Poly/ML.
*)

PolyML.addPrettyPrinter (fn depth => fn _ => fn str =>
  ml_pretty (Pretty.to_ML (ML_Syntax.pretty_string (depth * 100) str)));

PolyML.addPrettyPrinter (fn depth => fn _ => fn tree =>
  ml_pretty (Pretty.to_ML (XML.pretty depth tree)));

PolyML.addPrettyPrinter (fn depth => fn pretty => fn var =>
  pretty (Synchronized.value var, depth));

PolyML.addPrettyPrinter (fn depth => fn pretty => fn x =>
  (case Future.peek x of
    NONE => PolyML.PrettyString "<future>"
  | SOME (Exn.Exn _) => PolyML.PrettyString "<failed>"
  | SOME (Exn.Res y) => pretty (y, depth)));

PolyML.addPrettyPrinter (fn depth => fn pretty => fn x =>
  (case Lazy.peek x of
    NONE => PolyML.PrettyString "<lazy>"
  | SOME (Exn.Exn _) => PolyML.PrettyString "<failed>"
  | SOME (Exn.Res y) => pretty (y, depth)));


local

open PolyML;
val from_ML = Pretty.from_ML o pretty_ml;
fun prt_app name prt = Pretty.block [Pretty.str (name ^ " "), prt];
fun prt_apps name = Pretty.enum "," (name ^ " (") ")";

fun prt_term parens dp t =
  if dp <= 0 then Pretty.str "..."
  else
    (case t of
      _ $ _ =>
        op :: (strip_comb t)
        |> map_index (fn (i, u) => prt_term true (dp - i - 1) u)
        |> Pretty.separate " $"
        |> (if parens then Pretty.enclose "(" ")" else Pretty.block)
    | Abs (a, T, b) =>
        prt_apps "Abs"
         [from_ML (prettyRepresentation (a, dp - 1)),
          from_ML (prettyRepresentation (T, dp - 2)),
          prt_term false (dp - 3) b]
    | Const a => prt_app "Const" (from_ML (prettyRepresentation (a, dp - 1)))
    | Free a => prt_app "Free" (from_ML (prettyRepresentation (a, dp - 1)))
    | Var a => prt_app "Var" (from_ML (prettyRepresentation (a, dp - 1)))
    | Bound a => prt_app "Bound" (from_ML (prettyRepresentation (a, dp - 1))));

in

val _ =
  PolyML.addPrettyPrinter (fn depth => fn _ => fn t =>
    ml_pretty (Pretty.to_ML (prt_term false depth t)));

local

fun prt_proof parens dp prf =
  if dp <= 0 then Pretty.str "..."
  else
    (case prf of
      _ % _ => prt_proofs parens dp prf
    | _ %% _ => prt_proofs parens dp prf
    | Abst (a, T, b) =>
        prt_apps "Abst"
         [from_ML (prettyRepresentation (a, dp - 1)),
          from_ML (prettyRepresentation (T, dp - 2)),
          prt_proof false (dp - 3) b]
    | AbsP (a, t, b) =>
        prt_apps "AbsP"
         [from_ML (prettyRepresentation (a, dp - 1)),
          from_ML (prettyRepresentation (t, dp - 2)),
          prt_proof false (dp - 3) b]
    | Hyp t => prt_app "Hyp" (prt_term true (dp - 1) t)
    | MinProof => Pretty.str "MinProof"
    | PBound a => prt_app "PBound" (from_ML (prettyRepresentation (a, dp - 1)))
    | PAxm a => prt_app "PAxm" (from_ML (prettyRepresentation (a, dp - 1)))
    | OfClass a => prt_app "OfClass" (from_ML (prettyRepresentation (a, dp - 1)))
    | Oracle a => prt_app "Oracle" (from_ML (prettyRepresentation (a, dp - 1)))
    | Promise a => prt_app "Promise" (from_ML (prettyRepresentation (a, dp - 1)))
    | PThm a => prt_app "PThm" (from_ML (prettyRepresentation (a, dp - 1))))

and prt_proofs parens dp prf =
  let
    val (head, args) = strip_proof prf [];
    val prts = head (dp - 1) :: flat (map_index (fn (i, prt) => prt (dp - i - 2)) args);
  in if parens then Pretty.enclose "(" ")" prts else Pretty.block prts end

and strip_proof (p % t) res =
      strip_proof p
        ((fn d => [Pretty.str " %", Pretty.brk 1, from_ML (prettyRepresentation (t, d))]) :: res)
  | strip_proof (p %% q) res =
      strip_proof p ((fn d => [Pretty.str " %%", Pretty.brk 1, prt_proof true d q]) :: res)
  | strip_proof p res = (fn d => prt_proof true d p, res);

in

val _ =
  PolyML.addPrettyPrinter (fn depth => fn _ => fn prf =>
    ml_pretty (Pretty.to_ML (prt_proof false depth prf)));

end;

end;

