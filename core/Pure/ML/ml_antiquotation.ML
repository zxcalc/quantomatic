(*  Title:      Pure/ML/ml_antiquotation.ML
    Author:     Makarius

ML antiquotations.
*)

signature ML_ANTIQUOTATION =
sig
  val variant: string -> Proof.context -> string * Proof.context
  val declaration: binding -> 'a context_parser ->
    (Args.src -> 'a -> Proof.context -> ML_Context.decl * Proof.context) ->
    theory -> theory
  val inline: binding -> string context_parser -> theory -> theory
  val value: binding -> string context_parser -> theory -> theory
end;

structure ML_Antiquotation: ML_ANTIQUOTATION =
struct

(* unique names *)

val init_context = ML_Syntax.reserved |> fold Name.declare ["ML_context", "ML_print_depth"];

structure Names = Proof_Data
(
  type T = Name.context;
  fun init _ = init_context;
);

fun variant a ctxt =
  let
    val names = Names.get ctxt;
    val (b, names') = Name.variant (Name.desymbolize (SOME false) a) names;
    val ctxt' = Names.put names' ctxt;
  in (b, ctxt') end;


(* define antiquotations *)

fun declaration name scan body =
  ML_Context.add_antiquotation name
    (fn src => fn orig_ctxt =>
      let val (x, _) = Args.syntax scan src orig_ctxt
      in body src x orig_ctxt end);

fun inline name scan =
  declaration name scan (fn _ => fn s => fn ctxt => (K ("", s), ctxt));

fun value name scan =
  declaration name scan (fn _ => fn s => fn ctxt =>
    let
      val (a, ctxt') = variant (Binding.name_of name) ctxt;
      val env = "val " ^ a ^ " = " ^ s ^ ";\n";
      val body = "Isabelle." ^ a;
    in (K (env, body), ctxt') end);


(* basic antiquotations *)

val _ = Theory.setup
 (declaration (Binding.make ("here", @{here})) (Scan.succeed ())
    (fn src => fn () => fn ctxt =>
      let
        val (a, ctxt') = variant "position" ctxt;
        val (_, pos) = Args.name_of_src src;
        val env = "val " ^ a ^ " = " ^ ML_Syntax.print_position pos ^ ";\n";
        val body = "Isabelle." ^ a;
      in (K (env, body), ctxt') end) #>

  value (Binding.make ("binding", @{here}))
    (Scan.lift (Parse.position Args.name) >> ML_Syntax.make_binding));

end;

