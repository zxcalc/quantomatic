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
is broken then all enclosing blocks will also be broken.

The stored length of a block is used in break_dist (to treat each inner block as
a unit for breaking).
*)

signature PRETTY =
sig
  val default_indent: string -> int -> Output.output
  val add_mode: string -> (string -> int -> Output.output) -> unit
  type T
  val make_block: {markup: Output.output * Output.output, consistent: bool, indent: int} ->
    T list -> T
  val markup_block: {markup: Markup.T, consistent: bool, indent: int} -> T list -> T
  val str: string -> T
  val brk: int -> T
  val brk_indent: int -> int -> T
  val fbrk: T
  val breaks: T list -> T list
  val fbreaks: T list -> T list
  val blk: int * T list -> T
  val block: T list -> T
  val strs: string list -> T
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
  val unformatted_string_of: T -> string
  val markup_chunks: Markup.T -> T list -> T
  val chunks: T list -> T
  val chunks2: T list -> T
  val block_enclose: T * T -> T list -> T
  val writeln_chunks: T list -> unit
  val writeln_chunks2: T list -> unit
  val to_ML: FixedInt.int -> T -> ML_Pretty.pretty
  val from_ML: ML_Pretty.pretty -> T
  val to_polyml: T -> PolyML_Pretty.pretty
  val from_polyml: PolyML_Pretty.pretty -> T
end;

structure Pretty: PRETTY =
struct

(** print mode operations **)

fun default_indent (_: string) = Symbol.spaces;

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

val output_spaces = Output.output o Symbol.spaces;
val add_indent = Buffer.add o output_spaces;



(** printing items: compound phrases, strings, and breaks **)

val force_nat = Integer.max 0;

abstype T =
    Block of (Output.output * Output.output) * bool * int * T list * int
      (*markup output, consistent, indentation, body, length*)
  | Break of bool * int * int  (*mandatory flag, width if not taken, extra indentation if taken*)
  | Str of Output.output * int  (*text, length*)
with

fun length (Block (_, _, _, _, len)) = len
  | length (Break (_, wd, _)) = wd
  | length (Str (_, len)) = len;

fun make_block {markup, consistent, indent} body =
  let
    val indent' = force_nat indent;
    fun body_length prts len =
      let
        val (line, rest) = take_prefix (fn Break (true, _, _) => false | _ => true) prts;
        val len' = Int.max (fold (Integer.add o length) line 0, len);
      in
        (case rest of
          Break (true, _, ind) :: rest' =>
            body_length (Break (false, indent' + ind, 0) :: rest') len'
        | [] => len')
      end;
  in Block (markup, consistent, indent', body, body_length body 0) end;

fun markup_block {markup, consistent, indent} es =
  make_block {markup = Markup.output markup, consistent = consistent, indent = indent} es;



(** derived operations to create formatting expressions **)

val str = Output.output_width ##> force_nat #> Str;

fun brk wd = Break (false, force_nat wd, 0);
fun brk_indent wd ind = Break (false, force_nat wd, ind);
val fbrk = Break (true, 1, 0);

fun breaks prts = Library.separate (brk 1) prts;
fun fbreaks prts = Library.separate fbrk prts;

fun blk (indent, es) =
  markup_block {markup = Markup.empty, consistent = false, indent = indent} es;

fun block prts = blk (2, prts);
val strs = block o breaks o map str;

fun markup m = markup_block {markup = m, consistent = false, indent = 0};
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
fun cartouche prt = blk (1, [str Symbol.open_, prt, str Symbol.close]);

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
  | indent n prt = blk (0, [str (Symbol.spaces n), prt]);

fun unbreakable (Block (m, consistent, indent, es, len)) =
      Block (m, consistent, indent, map unbreakable es, len)
  | unbreakable (Break (_, wd, _)) = Str (output_spaces wd, wd)
  | unbreakable (e as Str _) = e;



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
fun break_dist (Break _ :: _, _) = 0
  | break_dist (e :: es, after) = length e + break_dist (es, after)
  | break_dist ([], after) = after;

val force_break = fn Break (false, wd, ind) => Break (true, wd, ind) | e => e;
val force_all = map force_break;

(*Search for the next break (at this or higher levels) and force it to occur.*)
fun force_next [] = []
  | force_next ((e as Break _) :: es) = force_break e :: es
  | force_next (e :: es) = e :: force_next es;

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
            Block ((bg, en), consistent, indent, bes, blen) =>
              let
                val pos' = pos + indent;
                val pos'' = pos' mod emergencypos;
                val block' =
                  if pos' < emergencypos then (ind |> add_indent indent, pos')
                  else (add_indent pos'' Buffer.empty, pos'');
                val d = break_dist (es, after)
                val bes' = if consistent andalso pos + blen > margin - d then force_all bes else bes;
                val btext: text = text
                  |> control bg
                  |> format (bes', block', d)
                  |> control en;
                (*if this block was broken then force the next break*)
                val es' = if nl < #nl btext then force_next es else es;
              in format (es', block, after) btext end
          | Break (force, wd, ind) =>
              (*no break if text to next break fits on this line
                or if breaking would add only breakgain to space*)
              format (es, block, after)
                (if not force andalso
                    pos + wd <= Int.max (margin - break_dist (es, after), blockin + breakgain)
                 then text |> blanks wd  (*just insert wd blanks*)
                 else text |> newline |> indentation block |> blanks ind)
          | Str str => format (es, block, after) (string str text));
  in
    #tx (format ([input], (Buffer.empty, 0), 0) empty)
  end;

end;


(* special output *)

(*symbolic markup -- no formatting*)
fun symbolic prt =
  let
    fun out (Block ((bg, en), _, _, [], _)) = Buffer.add bg #> Buffer.add en
      | out (Block ((bg, en), consistent, indent, prts, _)) =
          Buffer.add bg #>
          Buffer.markup (Markup.block consistent indent) (fold out prts) #>
          Buffer.add en
      | out (Break (false, wd, ind)) =
          Buffer.markup (Markup.break wd ind) (Buffer.add (output_spaces wd))
      | out (Break (true, _, _)) = Buffer.add (Output.output "\n")
      | out (Str (s, _)) = Buffer.add s;
  in out prt Buffer.empty end;

(*unformatted output*)
fun unformatted prt =
  let
    fun fmt (Block ((bg, en), _, _, prts, _)) = Buffer.add bg #> fold fmt prts #> Buffer.add en
      | fmt (Break (_, wd, _)) = Buffer.add (output_spaces wd)
      | fmt (Str (s, _)) = Buffer.add s;
  in fmt prt Buffer.empty end;


(* output interfaces *)

val margin_default = Unsynchronized.ref ML_Pretty.default_margin;  (*right margin, or page width*)

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

val unformatted_string_of = Output.escape o Buffer.content o unformatted;


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



(** toplevel pretty printing **)

fun to_ML 0 (Block _) = ML_Pretty.str "..."
  | to_ML depth (Block (m, consistent, indent, prts, _)) =
      ML_Pretty.Block (m, consistent, FixedInt.fromInt indent, map (to_ML (depth - 1)) prts)
  | to_ML _ (Break (force, wd, ind)) =
      ML_Pretty.Break (force, FixedInt.fromInt wd, FixedInt.fromInt ind)
  | to_ML _ (Str (s, len)) = ML_Pretty.String (s, FixedInt.fromInt len);

fun from_ML (ML_Pretty.Block (m, consistent, indent, prts)) =
      make_block {markup = m, consistent = consistent, indent = FixedInt.toInt indent}
        (map from_ML prts)
  | from_ML (ML_Pretty.Break (force, wd, ind)) =
      Break (force, FixedInt.toInt wd, FixedInt.toInt ind)
  | from_ML (ML_Pretty.String (s, len)) = Str (s, force_nat (FixedInt.toInt len));

val to_polyml = to_ML ~1 #> ML_Pretty.to_polyml;
val from_polyml = ML_Pretty.from_polyml #> from_ML;

end;

val _ = ML_system_pp (fn d => fn _ => ML_Pretty.to_polyml o to_ML (d + 1) o quote);
val _ = ML_system_pp (fn _ => fn _ => to_polyml o position);

end;

structure ML_Pretty =
struct
  open ML_Pretty;
  val string_of_polyml = Pretty.string_of o Pretty.from_polyml;
end;
