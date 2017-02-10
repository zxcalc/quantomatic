(*  Title:      Pure/ML/exn_properties.ML
    Author:     Makarius

Exception properties.
*)

signature EXN_PROPERTIES =
sig
  val position_of_polyml_location: ML_Compiler0.polyml_location -> Position.T
  val position: exn -> Position.T
  val get: exn -> Properties.T
  val update: Properties.entry list -> exn -> exn
end;

structure Exn_Properties: EXN_PROPERTIES =
struct

(* source locations *)

fun props_of_polyml_location (loc: ML_Compiler0.polyml_location) =
  (case YXML.parse_body (#file loc) of
    [] => []
  | [XML.Text file] =>
      if file = "Standard Basis" then []
      else [(Markup.fileN, ML_System.standard_path file)]
  | body => XML.Decode.properties body);

fun position_of_polyml_location loc =
  Position.make
   {line = FixedInt.toInt (#startLine loc),
    offset = FixedInt.toInt (#startPosition loc),
    end_offset = FixedInt.toInt (#endPosition loc),
    props = props_of_polyml_location loc};

fun position exn =
  (case Exn.polyml_location exn of
    NONE => Position.none
  | SOME loc => position_of_polyml_location loc);


(* exception properties *)

fun get exn =
  (case Exn.polyml_location exn of
    NONE => []
  | SOME loc => props_of_polyml_location loc);

fun update entries exn =
  if Exn.is_interrupt exn then exn
  else
    let
      val loc =
        the_default {file = "", startLine = 0, endLine = 0, startPosition = 0, endPosition = 0}
          (Exn.polyml_location exn);
      val props = props_of_polyml_location loc;
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
          Thread_Attributes.uninterruptible (fn _ => fn () =>
            PolyML.raiseWithLocation (exn, loc') handle exn' => exn') ()
        end
    end;

end;
