(*  Title:      Pure/ML/exn_properties_polyml.ML
    Author:     Makarius

Exception properties for Poly/ML.
*)

signature EXN_PROPERTIES =
sig
  val position_of: PolyML.location -> Position.T
  val get: exn -> Properties.T
  val update: Properties.entry list -> exn -> exn
end;

structure Exn_Properties: EXN_PROPERTIES =
struct

(* source locations *)

fun props_of (loc: PolyML.location) =
  (case YXML.parse_body (#file loc) of
    [] => []
  | [XML.Text file] => [(Markup.fileN, file)]
  | body => XML.Decode.properties body);

fun position_of loc =
  Position.make
   {line = #startLine loc,
    offset = #startPosition loc,
    end_offset = #endPosition loc,
    props = props_of loc};


(* exception properties *)

fun get exn =
  (case PolyML.exceptionLocation exn of
    NONE => []
  | SOME loc => props_of loc);

fun update entries exn =
  let
    val loc =
      the_default {file = "", startLine = 0, endLine = 0, startPosition = 0, endPosition = 0}
        (PolyML.exceptionLocation exn);
    val props = props_of loc;
    val props' = fold Properties.put entries props;
  in
    if props = props' then exn
    else
      let
        val loc' =
          {file = YXML.string_of_body (XML.Encode.properties props'),
            startLine = #startLine loc, endLine = #endLine loc,
            startPosition = #startPosition loc, endPosition = #endPosition loc};
      in
        uninterruptible (fn _ => fn () => PolyML.raiseWithLocation (exn, loc')) ()
          handle exn' => exn'
      end
  end;

end;

