(*  Title:      Pure/ML/install_pp_polyml-5.3.ML
    Author:     Makarius

Extra toplevel pretty-printing for Poly/ML 5.3.0.
*)

PolyML.addPrettyPrinter (fn depth => fn _ => fn str =>
  ml_pretty (Pretty.to_ML (ML_Syntax.pretty_string (depth * 100) str)));

PolyML.addPrettyPrinter (fn depth => fn pretty => fn var =>
  pretty (Synchronized.value var, depth));

PolyML.addPrettyPrinter (fn depth => fn pretty => fn x =>
  (case Future.peek x of
    NONE => PolyML.PrettyString "<future>"
  | SOME (Exn.Exn _) => PolyML.PrettyString "<failed>"
  | SOME (Exn.Result y) => pretty (y, depth)));

PolyML.addPrettyPrinter (fn depth => fn pretty => fn x =>
  (case Lazy.peek x of
    NONE => PolyML.PrettyString "<lazy>"
  | SOME (Exn.Exn _) => PolyML.PrettyString "<failed>"
  | SOME (Exn.Result y) => pretty (y, depth)));

PolyML.addPrettyPrinter (fn depth => fn _ => fn tm =>
  let
    open PolyML;
    val from_ML = Pretty.from_ML o pretty_ml;
    fun prt_app name prt = Pretty.block [Pretty.str (name ^ " "), prt];
    fun prt_apps name = Pretty.enum "," (name ^ " (") ")";
    fun prt_term parens dp (t as _ $ _) =
          op :: (strip_comb t)
          |> map_index (fn (i, u) => prt_term true (dp - i - 1) u)
          |> Pretty.separate " $"
          |> (if parens then Pretty.enclose "(" ")" else Pretty.block)
      | prt_term _ dp (Abs (x, T, body)) =
          prt_apps "Abs"
           [from_ML (prettyRepresentation (x, dp - 1)),
            from_ML (prettyRepresentation (T, dp - 2)),
            prt_term false (dp - 3) body]
      | prt_term _ dp (Const const) =
          prt_app "Const" (from_ML (prettyRepresentation (const, dp - 1)))
      | prt_term _ dp (Free free) =
          prt_app "Free" (from_ML (prettyRepresentation (free, dp - 1)))
      | prt_term _ dp (Var var) =
          prt_app "Var" (from_ML (prettyRepresentation (var, dp - 1)))
      | prt_term _ dp (Bound i) =
          prt_app "Bound" (from_ML (prettyRepresentation (i, dp - 1)));
  in ml_pretty (Pretty.to_ML (prt_term false depth tm)) end);

