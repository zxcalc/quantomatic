(*  Title:      Pure/ML/exn_properties_polyml.ML
    Author:     Makarius

Exception properties for Poly/ML.
*)

signature EXN_PROPERTIES =
sig
  val of_location: PolyML.location -> Properties.T
  val get: exn -> Properties.T
  val update: Properties.entry list -> exn -> exn
end;

structure Exn_Properties: EXN_PROPERTIES =
struct

fun of_location (loc: PolyML.location) =
  (case YXML.parse_body (#file loc) of
    [] => []
  | [XML.Text file] => [(Markup.fileN, file)]
  | body => XML.Decode.properties body);

fun get exn =
  (case PolyML.exceptionLocation exn of
    NONE => []
  | SOME loc => of_location loc);

fun update entries exn =
  let
    val loc =
      the_default {file = "", startLine = 0, endLine = 0, startPosition = 0, endPosition = 0}
        (PolyML.exceptionLocation exn);
    val props = of_location loc;
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

