local

  val lexicon = Scan.make_lexicon (map Symbol.explode
      ["theory", "begin", "end", "imports", "ML_file", ";"]);

  fun kw k s = s |> Scan.one (Token.keyword_with (fn k' => k' = k));
  fun name s = s |> Scan.one (Token.is_name);

  fun header s =  s |> kw "theory" -- name -- kw "imports" -- Scan.repeat name -- kw "begin";
  fun useline s = s |> kw "ML_file" |-- name >>
    (fn t => (use (Token.content_of t); t));

  fun thy_file s = s |> header |-- Scan.repeat (useline || kw ";") --| kw "end";
    
  fun read_thy pos str =
    let val res =
      str
      |> Source.of_string
      |> Symbol.source
      |> Token.source {do_recover = NONE} (K (lexicon,Scan.empty_lexicon)) pos
      |> Token.source_proper
      |> Source.source Token.stopper (Scan.single (Scan.error thy_file)) NONE
      |> Source.get_single;
    in
      (case res of
        SOME (h, _) => h
      | NONE => error ("Unexpected end of input" ^ Position.here pos))
    end;

in
  fun use_thy file = let
    val thy_str = File.read (Path.explode file)
    val _ = read_thy (Position.file file) thy_str
  in ()
  end
end

