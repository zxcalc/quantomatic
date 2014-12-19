theory scratch
imports lib
begin

ML_file "isabelle/use_thy.ML";

ML {*

val lex = Scan.make_lexicon (map Symbol.explode
    ["theory", "begin", "end", "imports", "ML_file", ";"]);

fun ws s = s |> Scan.many (Token.is_improper o fst);
fun kw k s = s |> Scan.one (Token.keyword_with (fn k' => k' = k));
fun name s = s |> Scan.one (Token.is_name);

val parseme = "(* comment (* nest *) *) theory   jub\n imports \"and such\" \n begin \n (* comment *) ML_file \"test/foo.ML\"; \n end \n";

(*val ts = Token.source {do_recover=NONE} (K (lex,Scan.empty_lexicon)) (Position.file "foo.thy") src;*)


fun header s =  s |> kw "theory" -- name -- kw "imports" -- Scan.repeat name -- kw "begin";
fun useline s = s |> kw "ML_file" |-- name >>
  (fn t => (use (Token.content_of t); t));

fun thy_file s = s |> header |-- Scan.repeat (useline || kw ";") --| kw "end";

fun token_source pos str =
  str
  |> Source.of_string
  |> Symbol.source
  |> Token.source {do_recover = NONE} (K (lex,Scan.empty_lexicon)) pos;

fun read_source pos source =
  let val res =
    source
    |> Token.source_proper
    |> Source.source Token.stopper (Scan.single (Scan.error thy_file)) NONE
    |> Source.get_single;
  in
    (case res of
      SOME (h, _) => h
    | NONE => error ("Unexpected end of input" ^ Position.here pos))
  end;
*}

ML {*
val p = Path.explode "~/git/quantomatic/newcore/lib.thy";
val str = File.read p;
val pos = Position.file "foo.thy";
val ts = token_source pos str |> Token.source_proper;
val res = read_source pos ts;

Token.content_of (hd res);
*}


end
