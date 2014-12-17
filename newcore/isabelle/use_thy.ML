local
  fun ch c = Scan.one (fn c' => c = c');
  fun ws s = s |> Scan.repeat (ch #" " || ch #"\t");
  fun mlfile s = s |> ch #"M" --| ch #"L" --| ch #"_" --|
                      ch #"f" --| ch #"i" --| ch #"l" --| ch #"e"
  fun useline s = s |> ws |-- mlfile |-- ws |-- ch #"\"" |--
                       Scan.many (fn c => c <> #"\"") --| ch #"\"";
  fun pull_file ln = SOME(String.implode (fst (useline (String.explode ln)))) handle _ => NONE;
  fun readlines f = case TextIO.inputLine f
                      of SOME ln => (case pull_file ln of SOME f => use f | NONE => () ;
                                     readlines f)
                       | NONE => ()

in
  fun use_thy file = let
    val stream = (TextIO.openIn file);
  in (readlines stream; TextIO.closeIn stream)
  end 
end

