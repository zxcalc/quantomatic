(*  Title:      Pure/Thy/thy_syntax.ML
    Author:     Makarius

Theory syntax elements.
*)

signature THY_SYNTAX =
sig
  datatype 'a element = Element of 'a * ('a element list * 'a) option
  val atom: 'a -> 'a element
  val map_element: ('a -> 'b) -> 'a element -> 'b element
  val flat_element: 'a element -> 'a list
  val last_element: 'a element -> 'a
  val parse_elements: Keyword.keywords -> Command_Span.span list -> Command_Span.span element list
end;

structure Thy_Syntax: THY_SYNTAX =
struct

(* datatype element: command with optional proof *)

datatype 'a element = Element of 'a * ('a element list * 'a) option;

fun element (a, b) = Element (a, SOME b);
fun atom a = Element (a, NONE);

fun map_element f (Element (a, NONE)) = Element (f a, NONE)
  | map_element f (Element (a, SOME (elems, b))) =
      Element (f a, SOME ((map o map_element) f elems, f b));

fun flat_element (Element (a, NONE)) = [a]
  | flat_element (Element (a, SOME (elems, b))) = a :: maps flat_element elems @ [b];

fun last_element (Element (a, NONE)) = a
  | last_element (Element (_, SOME (_, b))) = b;


(* scanning spans *)

val eof = Command_Span.Span (Command_Span.Command_Span ("", Position.none), []);

fun is_eof (Command_Span.Span (Command_Span.Command_Span ("", _), _)) = true
  | is_eof _ = false;

val not_eof = not o is_eof;

val stopper = Scan.stopper (K eof) is_eof;


(* parse *)

local

fun command_with pred =
  Scan.one
    (fn (Command_Span.Span (Command_Span.Command_Span (name, _), _)) => pred name | _ => false);

fun parse_element keywords =
  let
    val proof_atom =
      Scan.one
        (fn (Command_Span.Span (Command_Span.Command_Span (name, _), _)) =>
              Keyword.is_proof_body keywords name
          | _ => true) >> atom;
    fun proof_element x =
      (command_with (Keyword.is_proof_goal keywords) -- proof_rest >> element || proof_atom) x
    and proof_rest x =
      (Scan.repeat proof_element -- command_with (Keyword.is_qed keywords)) x;
  in
    command_with (Keyword.is_theory_goal keywords) -- proof_rest >> element ||
    Scan.one not_eof >> atom
  end;

in

fun parse_elements keywords =
  Source.of_list #>
  Source.source stopper (Scan.bulk (parse_element keywords)) #>
  Source.exhaust;

end;

end;
