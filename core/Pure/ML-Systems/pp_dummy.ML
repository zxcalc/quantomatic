(*  Title:      Pure/ML-Systems/pp_dummy.ML

Dummy setup for toplevel pretty printing.
*)

fun ml_pretty _ = raise Fail "ml_pretty dummy";
fun pretty_ml _ = raise Fail "pretty_ml dummy";

structure PolyML =
struct
  fun addPrettyPrinter _ = ();
  fun prettyRepresentation _ =
    raise Fail "PolyML.prettyRepresentation dummy";
  open PolyML;
end;

