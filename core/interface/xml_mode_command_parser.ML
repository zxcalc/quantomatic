signature COMMAND_PARSER
= sig

  (* an argument to a command *)
  datatype arg = STR of string
               | INT of int
               | XML of XML.tree;
  (* a command *)
  type T

  (* a command is the command name and its arguments *)
  val get_args : T -> arg list
  val get_cmdstr : T -> string 

  val string_of : T -> string 
  val string_of_arg : arg -> string 

  (* *)
  val parse : string (* in string *) 
              -> T list (* commands *)

end;


structure DB_XML_Mode_CommandParser
= struct

  (* a command is the command name and its arguments *)
  datatype T = Command of string * arg list;
  fun get_cmdstr (Command x) = fst x;
  fun get_args (Command x) = snd x;


  datatype state = XML_MODE of string * string list | COMMAND_MODE
  

  exception bug_unsupported_lex_kind_exp of unit;

  fun string_of_ident (SimpleLexer.ID s) = s
    | string_of_ident _ = raise bug_unsupported_lex_kind_exp ();

  fun unwrap (SimpleLexer.ID s) = STR s
    | unwrap (SimpleLexer.INT n) = INT n
    | unwrap (SimpleLexer.LIT s) = STR s
    | unwrap _ = raise bug_unsupported_lex_kind_exp ();
  
  val arg = ((num || string_literal || ident) >> unwrap) || (XML.parse_document >> XML);
  fun args ts = ts |> (((arg -- args) >> op::) || (noop >> K []));
  val expr = ((ident >> string_of_ident) -- args) >> Command;
  fun exprs ts = ts |> (((expr --| (sym ";") -- exprs) >> op::)
               || (expr >> single)
               || (noop >> K []))
  
  fun parse str = Scan.catch exprs (Symbol.explode str);

  (* pretty stuff *)
  fun string_of_arg (STR s) = s
    | string_of_arg (INT i) = Int.toString i
    | string_of_arg (XML xml) = XML.string_of xml;
  fun pretty (Command (cmd_name,args)) = Pretty.block[Pretty.str cmd_name,
                     Pretty.str_list "(" ")" (map string_of_arg args)]
  val print = Pretty.writeln o pretty
  val string_of = Pretty.string_of o pretty

end; (* local Lexer *)
end; (* structure CommandParser *)


structure CommandParser : COMMAND_PARSER = DB_CommandParser;
