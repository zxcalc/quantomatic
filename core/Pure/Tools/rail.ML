(*  Title:      Pure/Tools/rail.ML
    Author:     Michael Kerscher, TU München
    Author:     Makarius

Railroad diagrams in LaTeX.
*)

structure Rail: sig end =
struct

(** lexical syntax **)

(* singleton keywords *)

val keywords =
  Symtab.make [
    ("|", Markup.keyword3),
    ("*", Markup.keyword3),
    ("+", Markup.keyword3),
    ("?", Markup.keyword3),
    ("(", Markup.empty),
    (")", Markup.empty),
    ("\<newline>", Markup.keyword2),
    (";", Markup.keyword2),
    (":", Markup.keyword2),
    ("@", Markup.keyword1)];


(* datatype token *)

datatype kind =
  Keyword | Ident | String | Antiq of Antiquote.antiq | EOF;

datatype token = Token of Position.range * (kind * string);

fun pos_of (Token ((pos, _), _)) = pos;
fun end_pos_of (Token ((_, pos), _)) = pos;

fun range_of (toks as tok :: _) =
      let val pos' = end_pos_of (List.last toks)
      in Position.range (pos_of tok) pos' end
  | range_of [] = Position.no_range;

fun kind_of (Token (_, (k, _))) = k;
fun content_of (Token (_, (_, x))) = x;


(* diagnostics *)

val print_kind =
 fn Keyword => "rail keyword"
  | Ident => "identifier"
  | String => "single-quoted string"
  | Antiq _ => "antiquotation"
  | EOF => "end-of-input";

fun print (Token ((pos, _), (k, x))) =
  (if k = EOF then print_kind k else print_kind k ^ " " ^ quote x) ^
  Position.here pos;

fun print_keyword x = print_kind Keyword ^ " " ^ quote x;

fun reports_of_token (Token ((pos, _), (String, _))) = [(pos, Markup.inner_string)]
  | reports_of_token (Token ((pos, _), (Keyword, x))) =
      map (pair pos) (the_list (Symtab.lookup keywords x) @ Completion.suppress_abbrevs x)
  | reports_of_token (Token (_, (Antiq antiq, _))) = Antiquote.antiq_reports antiq
  | reports_of_token _ = [];


(* stopper *)

fun mk_eof pos = Token ((pos, Position.none), (EOF, ""));
val eof = mk_eof Position.none;

fun is_eof (Token (_, (EOF, _))) = true
  | is_eof _ = false;

val stopper =
  Scan.stopper (fn [] => eof | toks => mk_eof (end_pos_of (List.last toks))) is_eof;


(* tokenize *)

local

fun token k ss = [Token (Symbol_Pos.range ss, (k, Symbol_Pos.content ss))];

fun antiq_token (antiq as (ss, {range, ...})) =
  [Token (range, (Antiq antiq, Symbol_Pos.content ss))];

val scan_space = Scan.many1 (Symbol.is_blank o Symbol_Pos.symbol);

val scan_keyword =
  Scan.one (Symtab.defined keywords o Symbol_Pos.symbol);

val err_prefix = "Rail lexical error: ";

val scan_token =
  scan_space >> K [] ||
  Antiquote.scan_antiq >> antiq_token ||
  scan_keyword >> (token Keyword o single) ||
  Lexicon.scan_id >> token Ident ||
  Symbol_Pos.scan_string_q err_prefix >> (fn (pos1, (ss, pos2)) =>
    [Token (Position.range pos1 pos2, (String, Symbol_Pos.content ss))]);

val scan =
  (Scan.repeat scan_token >> flat) --|
    Symbol_Pos.!!! (fn () => err_prefix ^ "bad input")
      (Scan.ahead (Scan.one Symbol_Pos.is_eof));

in

val tokenize = #1 o Scan.error (Scan.finite Symbol_Pos.stopper scan);

end;



(** parsing **)

(* parser combinators *)

fun !!! scan =
  let
    val prefix = "Rail syntax error";

    fun get_pos [] = " (end-of-input)"
      | get_pos (tok :: _) = Position.here (pos_of tok);

    fun err (toks, NONE) = (fn () => prefix ^ get_pos toks)
      | err (toks, SOME msg) =
          (fn () =>
            let val s = msg () in
              if String.isPrefix prefix s then s
              else prefix ^ get_pos toks ^ ": " ^ s
            end);
  in Scan.!! err scan end;

fun $$$ x =
  Scan.one (fn tok => kind_of tok = Keyword andalso content_of tok = x) ||
  Scan.fail_with
    (fn [] => (fn () => print_keyword x ^ " expected,\nbut end-of-input was found")
      | tok :: _ => (fn () => print_keyword x ^ " expected,\nbut " ^ print tok ^ " was found"));

fun enum1 sep scan = scan ::: Scan.repeat ($$$ sep |-- !!! scan);
fun enum sep scan = enum1 sep scan || Scan.succeed [];

val ident = Scan.some (fn tok => if kind_of tok = Ident then SOME (content_of tok) else NONE);
val string = Scan.some (fn tok => if kind_of tok = String then SOME (content_of tok) else NONE);

val antiq = Scan.some (fn tok => (case kind_of tok of Antiq a => SOME a | _ => NONE));

fun RANGE scan = Scan.trace scan >> apsnd range_of;
fun RANGE_APP scan = RANGE scan >> (fn (f, r) => f r);


(* parse trees *)

datatype trees =
  CAT of tree list * Position.range
and tree =
  BAR of trees list * Position.range |
  STAR of (trees * trees) * Position.range |
  PLUS of (trees * trees) * Position.range |
  MAYBE of tree * Position.range |
  NEWLINE of Position.range |
  NONTERMINAL of string * Position.range |
  TERMINAL of (bool * string) * Position.range |
  ANTIQUOTE of (bool * Antiquote.antiq) * Position.range;

fun reports_of_tree ctxt =
  if Context_Position.is_visible ctxt then
    let
      fun reports r =
        if r = Position.no_range then []
        else [(Position.set_range r, Markup.expression)];
      fun trees (CAT (ts, r)) = reports r @ maps tree ts
      and tree (BAR (Ts, r)) = reports r @ maps trees Ts
        | tree (STAR ((T1, T2), r)) = reports r @ trees T1 @ trees T2
        | tree (PLUS ((T1, T2), r)) = reports r @ trees T1 @ trees T2
        | tree (MAYBE (t, r)) = reports r @ tree t
        | tree (NEWLINE r) = reports r
        | tree (NONTERMINAL (_, r)) = reports r
        | tree (TERMINAL (_, r)) = reports r
        | tree (ANTIQUOTE (_, r)) = reports r;
    in distinct (op =) o tree end
  else K [];

local

val at_mode = Scan.option ($$$ "@") >> (fn NONE => false | _ => true);

fun body x = (RANGE (enum1 "|" body1) >> BAR) x
and body0 x = (RANGE (enum "|" body1) >> BAR) x
and body1 x =
 (RANGE_APP (body2 :|-- (fn a =>
   $$$ "*" |-- !!! body4e >> (fn b => fn r => CAT ([STAR ((a, b), r)], r)) ||
   $$$ "+" |-- !!! body4e >> (fn b => fn r => CAT ([PLUS ((a, b), r)], r)) ||
   Scan.succeed (K a)))) x
and body2 x = (RANGE (Scan.repeat1 body3) >> CAT) x
and body3 x =
 (RANGE_APP (body4 :|-- (fn a =>
   $$$ "?" >> K (curry MAYBE a) ||
   Scan.succeed (K a)))) x
and body4 x =
 ($$$ "(" |-- !!! (body0 --| $$$ ")") ||
  RANGE_APP
   ($$$ "\<newline>" >> K NEWLINE ||
    ident >> curry NONTERMINAL ||
    at_mode -- string >> curry TERMINAL ||
    at_mode -- antiq >> curry ANTIQUOTE)) x
and body4e x =
  (RANGE (Scan.option body4) >> (fn (a, r) => CAT (the_list a, r))) x;

val rule_name = ident >> Antiquote.Text || antiq >> Antiquote.Antiq;
val rule = rule_name -- ($$$ ":" |-- !!! body) || body >> pair (Antiquote.Text "");
val rules = enum1 ";" (Scan.option rule) >> map_filter I;

in

fun parse_rules toks =
  #1 (Scan.error (Scan.finite stopper (rules --| !!! (Scan.ahead (Scan.one is_eof)))) toks);

end;


(** rail expressions **)

(* datatype *)

datatype rails =
  Cat of int * rail list
and rail =
  Bar of rails list |
  Plus of rails * rails |
  Newline of int |
  Nonterminal of string |
  Terminal of bool * string |
  Antiquote of bool * Antiquote.antiq;

fun is_newline (Newline _) = true | is_newline _ = false;


(* prepare *)

local

fun cat rails = Cat (0, rails);

val empty = cat [];
fun is_empty (Cat (_, [])) = true | is_empty _ = false;

fun bar [Cat (_, [rail])] = rail
  | bar cats = Bar cats;

fun reverse_cat (Cat (y, rails)) = Cat (y, rev (map reverse rails))
and reverse (Bar cats) = Bar (map reverse_cat cats)
  | reverse (Plus (cat1, cat2)) = Plus (reverse_cat cat1, reverse_cat cat2)
  | reverse x = x;

fun plus (cat1, cat2) = Plus (cat1, reverse_cat cat2);

in

fun prepare_trees (CAT (ts, _)) = Cat (0, map prepare_tree ts)
and prepare_tree (BAR (Ts, _)) = bar (map prepare_trees Ts)
  | prepare_tree (STAR (Ts, _)) =
      let val (cat1, cat2) = apply2 prepare_trees Ts in
        if is_empty cat2 then plus (empty, cat1)
        else bar [empty, cat [plus (cat1, cat2)]]
      end
  | prepare_tree (PLUS (Ts, _)) = plus (apply2 prepare_trees Ts)
  | prepare_tree (MAYBE (t, _)) = bar [empty, cat [prepare_tree t]]
  | prepare_tree (NEWLINE _) = Newline 0
  | prepare_tree (NONTERMINAL (a, _)) = Nonterminal a
  | prepare_tree (TERMINAL (a, _)) = Terminal a
  | prepare_tree (ANTIQUOTE (a, _)) = Antiquote a;

end;


(* read *)

fun read ctxt source =
  let
    val _ = Context_Position.report ctxt (Input.pos_of source) Markup.language_rail;
    val toks = tokenize (Input.source_explode source);
    val _ = Context_Position.reports ctxt (maps reports_of_token toks);
    val rules = parse_rules toks;
    val _ = Position.reports (maps (reports_of_tree ctxt o #2) rules);
  in map (apsnd prepare_tree) rules end;


(* latex output *)

local

fun vertical_range_cat (Cat (_, rails)) y =
  let val (rails', (_, y')) =
    fold_map (fn rail => fn (y0, y') =>
      if is_newline rail then (Newline (y' + 1), (y' + 1, y' + 2))
      else
        let val (rail', y0') = vertical_range rail y0;
        in (rail', (y0, Int.max (y0', y'))) end) rails (y, y + 1)
  in (Cat (y, rails'), y') end

and vertical_range (Bar cats) y =
      let val (cats', y') = fold_map vertical_range_cat cats y
      in (Bar cats', Int.max (y + 1, y')) end
  | vertical_range (Plus (cat1, cat2)) y =
      let val ([cat1', cat2'], y') = fold_map vertical_range_cat [cat1, cat2] y;
      in (Plus (cat1', cat2'), Int.max (y + 1, y')) end
  | vertical_range (Newline _) y = (Newline (y + 2), y + 3)
  | vertical_range atom y = (atom, y + 1);

fun output_rules state rules =
  let
    val output_antiq = Thy_Output.eval_antiq state;
    fun output_text b s =
      Output.output s
      |> b ? enclose "\\isakeyword{" "}"
      |> enclose "\\isa{" "}";

    fun output_cat c (Cat (_, rails)) = outputs c rails
    and outputs c [rail] = output c rail
      | outputs _ rails = implode (map (output "") rails)
    and output _ (Bar []) = ""
      | output c (Bar [cat]) = output_cat c cat
      | output _ (Bar (cat :: cats)) =
          "\\rail@bar\n" ^ output_cat "" cat ^
          implode (map (fn Cat (y, rails) =>
              "\\rail@nextbar{" ^ string_of_int y ^ "}\n" ^ outputs "" rails) cats) ^
          "\\rail@endbar\n"
      | output c (Plus (cat, Cat (y, rails))) =
          "\\rail@plus\n" ^ output_cat c cat ^
          "\\rail@nextplus{" ^ string_of_int y ^ "}\n" ^ outputs "c" rails ^
          "\\rail@endplus\n"
      | output _ (Newline y) = "\\rail@cr{" ^ string_of_int y ^ "}\n"
      | output c (Nonterminal s) = "\\rail@" ^ c ^ "nont{" ^ output_text false s ^ "}[]\n"
      | output c (Terminal (b, s)) = "\\rail@" ^ c ^ "term{" ^ output_text b s ^ "}[]\n"
      | output c (Antiquote (b, a)) =
          "\\rail@" ^ c ^ (if b then "term{" else "nont{") ^ output_antiq a ^ "}[]\n";

    fun output_rule (name, rail) =
      let
        val (rail', y') = vertical_range rail 0;
        val out_name =
          (case name of
            Antiquote.Text "" => ""
          | Antiquote.Text s => output_text false s
          | Antiquote.Antiq a => output_antiq a);
      in
        "\\rail@begin{" ^ string_of_int y' ^ "}{" ^ out_name ^ "}\n" ^
        output "" rail' ^
        "\\rail@end\n"
      end;
  in
    "\\begin{railoutput}\n" ^
    implode (map output_rule rules) ^
    "\\end{railoutput}\n"
  end;

in

val _ = Theory.setup
  (Thy_Output.antiquotation @{binding rail} (Scan.lift Args.text_input)
    (fn {state, context, ...} => output_rules state o read context));

end;

end;

