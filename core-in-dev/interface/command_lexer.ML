structure CommandLexer =
struct
  local open SimpleLexer in

  (* a command is the command name and its arguments *)
  type T = string * string list;
  
  exception bug_exp of ();

  fun unwrap (ID s) = s
    | unwrap (INT n) = Int.toString n
    | unwrap (LIT s) = s
    | unwrap _ = raise bug_exp ();
  
  (*fun unescape (LIT str) = str String.translate
                 (fn c => case c of #"\\" => ""
                          | _ => Char.toString c)
                 str
    | unescape _ = raise Match
  *)
  
  (*
  a term grammar with no nesting
  
  arg   ::= LITERAL | ID
  args  ::= arg args | E
  expr  ::= ID args
  exprs ::= expr ; exprs | E
  *)
  
  
  fun arg x = x |> ((literal >> unwrap)
                    || ((ident || num) >> unwrap))
  fun args x = x|>(((arg -- args) >> op::)
               || (noop >> K []))
  fun expr x = x|>((ident >> unwrap) -- args)
  fun exprs x = x|>(
            ((expr --| sym ";" -- exprs) >> op::)
            || (expr >> single)
            || (noop >> K []))
  
  fun parse str = parse_with exprs str
  fun pretty (f,args) = Pretty.block[Pretty.str f,
                     Pretty.str_list "(" ")" args]
  val print = Pretty.writeln o pretty
  val toString = Pretty.string_of o pretty

end; (* local Lexer *)
end; (* structure CommandLexer *)


