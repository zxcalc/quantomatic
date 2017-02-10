(*  Title:      Pure/Syntax/term_position.ML
    Author:     Makarius

Encoded position within term syntax trees.
*)

signature TERM_POSITION =
sig
  val pretty: Position.T -> Pretty.T
  val encode: Position.T -> string
  val decode: string -> Position.T option
  val decode_position: term -> (Position.T * typ) option
  val decode_positionT: typ -> Position.T option
  val decode_positionS: sort -> Position.T list * sort
  val is_position: term -> bool
  val is_positionT: typ -> bool
  val markers: string list
  val strip_positions: term -> term
end;

structure Term_Position: TERM_POSITION =
struct

(* markup *)

val position_dummy = "<position>";
val position_text = XML.Text position_dummy;

fun pretty pos =
  Pretty.markup (Position.markup pos Markup.position) [Pretty.str position_dummy];

fun encode pos =
  YXML.string_of (XML.Elem (Position.markup pos Markup.position, [position_text]));

fun decode str =
  (case YXML.parse_body str handle Fail msg => error msg of
    [XML.Elem ((name, props), [arg])] =>
      if name = Markup.positionN andalso arg = position_text
      then SOME (Position.of_properties props)
      else NONE
  | _ => NONE);


(* positions within parse trees *)

fun decode_position (Free (x, _)) =
      (case decode x of
        SOME pos => SOME (pos, TFree (x, dummyS))
      | NONE => NONE)
  | decode_position _ = NONE;

fun decode_positionT (TFree (x, _)) = decode x
  | decode_positionT _ = NONE;

fun decode_positionS cs =
  let val (ps, sort) = List.partition (is_some o decode) cs
  in (map (the o decode) ps, sort) end;

val is_position = is_some o decode_position;
val is_positionT = is_some o decode_positionT;

val markers = ["_constrain", "_constrainAbs", "_ofsort"];

fun strip_positions ((t as Const (c, _)) $ u $ v) =
      if member (op =) markers c andalso is_position v
      then strip_positions u
      else t $ strip_positions u $ strip_positions v
  | strip_positions (t $ u) = strip_positions t $ strip_positions u
  | strip_positions (Abs (x, T, t)) = Abs (x, T, strip_positions t)
  | strip_positions t = t;

end;
