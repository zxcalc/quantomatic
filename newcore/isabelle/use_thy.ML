local
  val lex = Scan.make_lexicon (map Symbol.explode
    ["theory", "begin", "end", "imports", "ML_file"]);
  fun of_str str = map (fn s => (s,())) (Symbol.explode str);
  fun str_of syms = String.concat (map fst syms);
  val eof = [(Symbol.eof,())];

  fun ch c = Scan.one (fn (c',_) => c' = c);
  fun opt_ch c = Scan.optional (ch c) ("",());
  fun until c = Scan.many (fn (c',_) => c' <> c andalso c' <> Symbol.eof);
  fun ws s = s |> Scan.repeat (ch " " || ch "\t" || ch "\n" || ch "\r");
  fun lit str s = s|> ws |-- Scan.literal lex --| ws >>
    (fn l => if of_str str = l then Scan.succeed str [] else Scan.fail l);

  fun useline s = s |>
    lit "ML_file" |-- ch "\"" |-- (until "\"" >> str_of) --|
    ch "\"" --| ws --| opt_ch ";" >>
      (fn f => (use f; f));

  fun pull_files ln =
    SOME (fst (Scan.catch (Scan.repeat useline) (of_str ln @ eof)))
    handle Fail _ => NONE;

  fun readlines f = case TextIO.inputLine f
                      of SOME ln => (pull_files ln; readlines f)
                       | NONE => ()

in
  fun use_thy file = let
    val stream = (TextIO.openIn file);
  in (readlines stream; TextIO.closeIn stream)
  end
end

