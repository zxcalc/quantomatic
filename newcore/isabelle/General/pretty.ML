(*  Title:      Pure/General/pretty.ML
    Author:     Lawrence C Paulson, Cambridge University Computer Laboratory
    Author:     Markus Wenzel, TU Munich

Generic pretty printing module.

Loosely based on
  D. C. Oppen, "Pretty Printing",
  ACM Transactions on Programming Languages and Systems (1980), 465-483.

The object to be printed is given as a tree with indentation and line
breaking information.  A "break" inserts a newline if the text until
the next break is too long to fit on the current line.  After the newline,
text is indented to the level of the enclosing block.  Normally, if a block
is broken then all enclosing blocks will also be broken.  Only "inconsistent
breaks" are provided.

The stored length of a block is used in breakdist (to treat each inner block as
a unit for breaking).
*)

signature PRETTY =
sig
  val spaces: int -> string
  val default_indent: string -> int -> Output.output
  val add_mode: string -> (string -> int -> Output.output) -> unit
  type T
  val str: string -> T
  val brk: int -> T
  val fbrk: T
  val breaks: T list -> T list
  val fbreaks: T list -> T list
  val blk: int * T list -> T
  val block: T list -> T
  val strs: string list -> T
  val raw_markup: Output.output * Output.output -> int * T list -> T
  val markup: Markup.T -> T list -> T
  val mark: Markup.T -> T -> T
  val mark_str: Markup.T * string -> T
  val marks_str: Markup.T list * string -> T
  val item: T list -> T
  val text_fold: T list -> T
  val keyword1: string -> T
  val keyword2: string -> T
  val text: string -> T list
  val paragraph: T list -> T
  val para: string -> T
  val quote: T -> T
  val backquote: T -> T
  val cartouche: T -> T
  val separate: string -> T list -> T list
  val commas: T list -> T list
  val enclose: string -> string -> T list -> T
  val enum: string -> string -> string -> T list -> T
  val position: Position.T -> T
  val list: string -> string -> T list -> T
  val str_list: string -> string -> string list -> T
  val big_list: string -> T list -> T
  val indent: int -> T -> T
  val unbreakable: T -> T
  val margin_default: int Unsynchronized.ref
  val symbolicN: string
  val output_buffer: int option -> T -> Buffer.T
  val output: int option -> T -> Output.output
  val string_of_margin: int -> T -> string
  val string_of: T -> string
  val writeln: T -> unit
  val symbolic_output: T -> Output.output
  val symbolic_string_of: T -> string
  val str_of: T -> string
  val markup_chunks: Markup.T -> T list -> T
  val chunks: T list -> T
  val chunks2: T list -> T
  val block_enclose: T * T -> T list -> T
  val writeln_chunks: T list -> unit
  val writeln_chunks2: T list -> unit
  val to_ML: T -> ML_Pretty.pretty
  val from_ML: ML_Pretty.pretty -> T
end;

structure Pretty: PRETTY =
struct

(** spaces **)

local
  val small_spaces = Vector.tabulate (65, fn i => replicate_string i Symbol.space);
in
  fun spaces k =
    if k < 64 then Vector.sub (small_spaces, k)
    else
      replicate_string (k div 64) (Vector.sub (small_spaces, 64)) ^
        Vector.sub (small_spaces, k mod 64);
end;



(** print mode operations **)

fun default_indent (_: string) = spaces;

local
  val default = {indent = default_indent};
  val modes = Synchronized.var "Pretty.modes" (Symtab.make [("", default)]);
in
  fun add_mode name indent =
    Synchronized.change modes (fn tab =>
      (if not (Symtab.defined tab name) then ()
       else warning ("Redefining pretty mode " ^ quote name);
       Symtab.update (name, {indent = indent}) tab));
  fun get_mode () =
    the_default default
      (Library.get_first (Symtab.lookup (Synchronized.value modes)) (print_mode_value ()));
end;

fun mode_indent x y = #indent (get_mode ()) x y;

val output_spaces = Output.output o spaces;
val add_indent = Buffer.add o output_spaces;



(** printing items: compound phrases, strings, and breaks **)

abstype T =
    Block of (Output.output * Output.output) * T list * int * int
      (*markup output, body, indentation, length*)
  | String of Output.output * int  (*text, length*)
  | Break of bool * int  (*mandatory flag, width if not taken*)
with

fun length (Block (_, _, _, len)) = len
  | length (String (_, len)) = len
  | length (Break (_, wd)) = wd;



(** derived operations to create formatting expressions **)

val str = String o Output.output_width;

fun brk wd = Break (false, wd);
val fbrk = Break (true, 1);

fun breaks prts = Library.separate (brk 1) prts;
fun fbreaks prts = Library.separate fbrk prts;

fun raw_markup m (indent, es) =
  let
    fun sum [] k = k
      | sum (e :: es) k = sum es (length e + k);
  in Block (m, es, indent, sum es 0) end;

fun markup_block m arg = raw_markup (Markup.output m) arg;

val blk = markup_block Markup.empty;
fun block prts = blk (2, prts);
val strs = block o breaks o map str;

fun markup m prts = markup_block m (0, prts);
fun mark m prt = if m = Markup.empty then prt else markup m [prt];
fun mark_str (m, s) = mark m (str s);
fun marks_str (ms, s) = fold_rev mark ms (str s);

val item = markup Markup.item;
val text_fold = markup Markup.text_fold;

fun keyword1 name = mark_str (Markup.keyword1, name);
fun keyword2 name = mark_str (Markup.keyword2, name);

val text = breaks o map str o Symbol.explode_words;
val paragraph = markup Markup.paragraph;
val para = paragraph o text;

fun quote prt = blk (1, [str "\"", prt, str "\""]);
fun backquote prt = blk (1, [str "`", prt, str "`"]);
fun cartouche prt = blk (1, [str "\\<open>", prt, str "\\<close>"]);

fun separate sep prts =
  flat (Library.separate [str sep, brk 1] (map single prts));

val commas = separate ",";

fun enclose lpar rpar prts =
  block (str lpar :: (prts @ [str rpar]));

fun enum sep lpar rpar prts = enclose lpar rpar (separate sep prts);

val position =
  enum "," "{" "}" o map (fn (x, y) => str (x ^ "=" ^ y)) o Position.properties_of;

val list = enum ",";
fun str_list lpar rpar strs = list lpar rpar (map str strs);

fun big_list name prts = block (fbreaks (str name :: prts));

fun indent 0 prt = prt
  | indent n prt = blk (0, [str (spaces n), prt]);

fun unbreakable (Break (_, wd)) = String (output_spaces wd, wd)
  | unbreakable (Block (m, es, indent, wd)) = Block (m, map unbreakable es, indent, wd)
  | unbreakable (e as String _) = e;



(** formatting **)

(* formatted output *)

local

type text = {tx: Buffer.T, ind: Buffer.T, pos: int, nl: int};

val empty: text =
 {tx = Buffer.empty,
  ind = Buffer.empty,
  pos = 0,
  nl = 0};

fun newline {tx, ind = _, pos = _, nl} : text =
 {tx = Buffer.add (Output.output "\n") tx,
  ind = Buffer.empty,
  pos = 0,
  nl = nl + 1};

fun control s {tx, ind, pos: int, nl} : text =
 {tx = Buffer.add s tx,
  ind = ind,
  pos = pos,
  nl = nl};

fun string (s, len) {tx, ind, pos: int, nl} : text =
 {tx = Buffer.add s tx,
  ind = Buffer.add s ind,
  pos = pos + len,
  nl = nl};

fun blanks wd = string (output_spaces wd, wd);

fun indentation (buf, len) {tx, ind, pos, nl} : text =
  let val s = Buffer.content buf in
   {tx = Buffer.add (mode_indent s len) tx,
    ind = Buffer.add s ind,
    pos = pos + len,
    nl = nl}
  end;

(*Add the lengths of the expressions until the next Break; if no Break then
  include "after", to account for text following this block.*)
fun breakdist (Break _ :: _, _) = 0
  | breakdist (Block (_, _, _, len) :: es, after) = len + breakdist (es, after)
  | breakdist (String (_, len) :: es, after) = len + breakdist (es, after)
  | breakdist ([], after) = after;

(*Search for the next break (at this or higher levels) and force it to occur.*)
fun forcenext [] = []
  | forcenext (Break _ :: es) = fbrk :: es
  | forcenext (e :: es) = e :: forcenext es;

in

fun formatted margin input =
  let
    val breakgain = margin div 20;     (*minimum added space required of a break*)
    val emergencypos = margin div 2;   (*position too far to right*)

    (*es is list of expressions to print;
      blockin is the indentation of the current block;
      after is the width of the following context until next break.*)
    fun format ([], _, _) text = text
      | format (e :: es, block as (_, blockin), after) (text as {ind, pos, nl, ...}) =
          (case e of
            Block ((bg, en), bes, indent, _) =>
              let
                val pos' = pos + indent;
                val pos'' = pos' mod emergencypos;
                val block' =
                  if pos' < emergencypos then (ind |> add_indent indent, pos')
                  else (add_indent pos'' Buffer.empty, pos'');
                val btext: text = text
                  |> control bg
                  |> format (bes, block', breakdist (es, after))
                  |> control en;
                (*if this block was broken then force the next break*)
                val es' = if nl < #nl btext then forcenext es else es;
              in format (es', block, after) btext end
          | Break (force, wd) =>
              (*no break if text to next break fits on this line
                or if breaking would add only breakgain to space*)
              format (es, block, after)
                (if not force andalso
                    pos + wd <= Int.max (margin - breakdist (es, after), blockin + breakgain)
                  then text |> blanks wd  (*just insert wd blanks*)
                  else text |> newline |> indentation block)
          | String str => format (es, block, after) (string str text));
  in
    #tx (format ([input], (Buffer.empty, 0), 0) empty)
  end;

end;


(* special output *)

(*symbolic markup -- no formatting*)
fun symbolic prt =
  let
    fun out (Block ((bg, en), [], _, _)) = Buffer.add bg #> Buffer.add en
      | out (Block ((bg, en), prts, indent, _)) =
          Buffer.add bg #>
          Buffer.markup (Markup.block indent) (fold out prts) #>
          Buffer.add en
      | out (String (s, _)) = Buffer.add s
      | out (Break (false, wd)) =
          Buffer.markup (Markup.break wd) (Buffer.add (output_spaces wd))
      | out (Break (true, _)) = Buffer.add (Output.output "\n");
  in out prt Buffer.empty end;

(*unformatted output*)
fun unformatted prt =
  let
    fun fmt (Block ((bg, en), prts, _, _)) = Buffer.add bg #> fold fmt prts #> Buffer.add en
      | fmt (String (s, _)) = Buffer.add s
      | fmt (Break (_, wd)) = Buffer.add (output_spaces wd);
  in fmt prt Buffer.empty end;


(* output interfaces *)

val margin_default = Unsynchronized.ref 76;  (*right margin, or page width*)

val symbolicN = "pretty_symbolic";

fun output_buffer margin prt =
  if print_mode_active symbolicN then symbolic prt
  else formatted (the_default (! margin_default) margin) prt;

val output = Buffer.content oo output_buffer;
fun string_of_margin margin = Output.escape o output (SOME margin);
val string_of = Output.escape o output NONE;
val writeln = Output.writeln o string_of;

val symbolic_output = Buffer.content o symbolic;
val symbolic_string_of = Output.escape o symbolic_output;

val str_of = Output.escape o Buffer.content o unformatted;


(* chunks *)

fun markup_chunks m prts = markup m (fbreaks (map (text_fold o single) prts));
val chunks = markup_chunks Markup.empty;

fun chunks2 prts =
  (case try split_last prts of
    NONE => blk (0, [])
  | SOME (prefix, last) =>
      blk (0, maps (fn prt => [text_fold [prt, fbrk], fbrk]) prefix @ [text_fold [last]]));

fun block_enclose (prt1, prt2) prts = chunks [block (fbreaks (prt1 :: prts)), prt2];

fun string_of_text_fold prt = string_of prt |> Markup.markup Markup.text_fold;

fun writeln_chunks prts =
  Output.writelns (Library.separate "\n" (map string_of_text_fold prts));

fun writeln_chunks2 prts =
  (case try split_last prts of
    NONE => ()
  | SOME (prefix, last) =>
      (map (fn prt => Markup.markup Markup.text_fold (string_of prt ^ "\n") ^ "\n") prefix @
        [string_of_text_fold last])
      |> Output.writelns);



(** ML toplevel pretty printing **)

fun to_ML (Block (m, prts, ind, _)) = ML_Pretty.Block (m, map to_ML prts, ind)
  | to_ML (String s) = ML_Pretty.String s
  | to_ML (Break b) = ML_Pretty.Break b;

fun from_ML (ML_Pretty.Block (m, prts, ind)) = raw_markup m (ind, map from_ML prts)
  | from_ML (ML_Pretty.String s) = String s
  | from_ML (ML_Pretty.Break b) = Break b;

end;

end;
