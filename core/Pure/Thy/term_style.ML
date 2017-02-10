(*  Title:      Pure/Thy/term_style.ML
    Author:     Florian Haftmann, TU Muenchen

Styles for term printing.
*)

signature TERM_STYLE =
sig
  val setup: binding -> (Proof.context -> term -> term) parser -> theory -> theory
  val parse: (term -> term) context_parser
end;

structure Term_Style: TERM_STYLE =
struct

(* theory data *)

structure Data = Theory_Data
(
  type T = (Proof.context -> term -> term) parser Name_Space.table;
  val empty : T = Name_Space.empty_table "antiquotation_style";
  val extend = I;
  fun merge data : T = Name_Space.merge_tables data;
);

val get_data = Data.get o Proof_Context.theory_of;
val get_style = Name_Space.get o get_data;

fun setup binding style thy =
  Data.map (#2 o Name_Space.define (Context.Theory thy) true (binding, style)) thy;


(* style parsing *)

fun parse_single ctxt =
  Parse.position Parse.xname -- Parse.args >> (fn (name, args) =>
    let
      val (src, parse) = Args.check_src ctxt (get_data ctxt) (Args.src name args);
      val (f, _) = Args.syntax (Scan.lift parse) src ctxt;
    in f ctxt end);

val parse = Args.context :|-- (fn ctxt => Scan.lift
  (Args.parens (parse_single ctxt ::: Scan.repeat (Args.$$$ "," |-- parse_single ctxt))
      >> fold I
  || Scan.succeed I));


(* predefined styles *)

fun style_lhs_rhs proj = Scan.succeed (fn ctxt => fn t =>
  let
    val concl =
      Object_Logic.drop_judgment (Proof_Context.theory_of ctxt) (Logic.strip_imp_concl t)
  in
    (case concl of
      _ $ l $ r => proj (l, r)
    | _ => error ("Binary operator expected in term: " ^ Syntax.string_of_term ctxt concl))
  end);

val style_prem = Parse.nat >> (fn i => fn ctxt => fn t =>
  let
    val prems = Logic.strip_imp_prems t;
  in
    if i <= length prems then nth prems (i - 1)
    else
      error ("Not enough premises for prem " ^ string_of_int i ^
        " in propositon: " ^ Syntax.string_of_term ctxt t)
  end);

fun sub_symbols (d :: s :: ss) =
      if Symbol.is_ascii_digit d andalso not (String.isPrefix ("\<^") s)
      then d :: "\<^sub>" :: sub_symbols (s :: ss)
      else d :: s :: ss
  | sub_symbols cs = cs;

val sub_name = implode o rev o sub_symbols o rev o Symbol.explode;

fun sub_term (Free (n, T)) = Free (sub_name n, T)
  | sub_term (Var ((n, idx), T)) =
      if idx <> 0 then Var ((sub_name (n ^ string_of_int idx), 0), T)
      else Var ((sub_name n, 0), T)
  | sub_term (t $ u) = sub_term t $ sub_term u
  | sub_term (Abs (n, T, b)) = Abs (sub_name n, T, sub_term b)
  | sub_term t = t;

val _ = Theory.setup
 (setup @{binding lhs} (style_lhs_rhs fst) #>
  setup @{binding rhs} (style_lhs_rhs snd) #>
  setup @{binding prem} style_prem #>
  setup @{binding concl} (Scan.succeed (K Logic.strip_imp_concl)) #>
  setup @{binding sub} (Scan.succeed (K sub_term)));

end;
