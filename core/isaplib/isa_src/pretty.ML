(*  Title:      Pure/General/pretty.ML
    ID:         $Id: pretty.ML,v 1.36 2008/04/17 14:30:48 wenzelm Exp $
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

type pprint_args = (output -> unit) * (int -> unit) * (int -> unit) *
  (unit -> unit) * (unit -> unit);

signature PRETTY =
sig
  val default_indent: string -> int -> output
  val add_mode: string -> (string -> int -> output) -> unit
  type T
  val str: string -> T
  val brk: int -> T
  val fbrk: T
  val breaks: T list -> T list
  val fbreaks: T list -> T list
  val blk: int * T list -> T
  val block: T list -> T
  val strs: string list -> T
  val markup: Markup.T -> T list -> T
  val mark: Markup.T -> T -> T
  val keyword: string -> T
  val command: string -> T
  val markup_chunks: Markup.T -> T list -> T
  val chunks: T list -> T
  val chunks2: T list -> T
  val block_enclose: T * T -> T list -> T
  val quote: T -> T
  val backquote: T -> T
  val separate: string -> T list -> T list
  val commas: T list -> T list
  val enclose: string -> string -> T list -> T
  val enum: string -> string -> string -> T list -> T
  val list: string -> string -> T list -> T
  val str_list: string -> string -> string list -> T
  val big_list: string -> T list -> T
  val indent: int -> T -> T
  val unbreakable: T -> T
  val setmargin: int -> unit
  val setmp_margin: int -> ('a -> 'b) -> 'a -> 'b
  val setdepth: int -> unit
  val pprint: T -> pprint_args -> unit
  val symbolicN: string
  val output_buffer: T -> Buffer.T
  val output: T -> output
  val string_of: T -> string
  val str_of: T -> string
  val writeln: T -> unit
end;

structure Pretty: PRETTY =
struct

(** print mode operations **)

fun default_indent (_: string) = Symbol.spaces;

local
  val default = {indent = default_indent};
  val modes = ref (Symtab.make [("", default)]);
in
  fun add_mode name indent = CRITICAL (fn () =>
    change modes (Symtab.update_new (name, {indent = indent})));
  fun get_mode () =
    the_default default (Library.get_first (Symtab.lookup (! modes)) (print_mode_value ()));
end;

fun mode_indent x y = #indent (get_mode ()) x y;

val output_spaces = Output.output o Symbol.spaces;
val add_indent = Buffer.add o output_spaces;



(** printing items: compound phrases, strings, and breaks **)

datatype T =
  Block of Markup.T * T list * int * int |  (*markup, body, indentation, length*)
  String of output * int |                  (*text, length*)
  Break of bool * int;                      (*mandatory flag, width if not taken*)

fun length (Block (_, _, _, len)) = len
  | length (String (_, len)) = len
  | length (Break (_, wd)) = wd;



(** derived operations to create formatting expressions **)

val str = String o Output.output_width;

fun brk wd = Break (false, wd);
val fbrk = Break (true, 2);

fun breaks prts = Library.separate (brk 1) prts;
fun fbreaks prts = Library.separate fbrk prts;

fun markup_block m (indent, es) =
  let
    fun sum [] k = k
      | sum (e :: es) k = sum es (length e + k);
  in Block (m, es, indent, sum es 0) end;

val blk = markup_block Markup.none;
fun block prts = blk (2, prts);
val strs = block o breaks o map str;

fun markup m prts = markup_block m (0, prts);
fun mark m prt = markup m [prt];
fun keyword name = mark (Markup.keyword name) (str name);
fun command name = mark (Markup.command name) (str name);

fun markup_chunks m prts = markup m (fbreaks prts);
val chunks = markup_chunks Markup.none;
fun chunks2 prts = blk (0, flat (Library.separate [fbrk, fbrk] (map single prts)));

fun block_enclose (p1, p2) ps = chunks [(block  o fbreaks) (p1 :: ps), p2];

fun quote prt = blk (1, [str "\"", prt, str "\""]);
fun backquote prt = blk (1, [str "`", prt, str "`"]);

fun separate sep prts =
  flat (Library.separate [str sep, brk 1] (map single prts));

val commas = separate ",";

fun enclose lpar rpar prts =
  block (str lpar :: (prts @ [str rpar]));

fun enum sep lpar rpar prts = enclose lpar rpar (separate sep prts);

val list = enum ",";
fun str_list lpar rpar strs = list lpar rpar (map str strs);

fun big_list name prts = block (fbreaks (str name :: prts));

fun indent 0 prt = prt
  | indent n prt = blk (0, [str (Symbol.spaces n), prt]);

fun unbreakable (Break (_, wd)) = String (output_spaces wd, wd)
  | unbreakable (Block (m, es, indent, wd)) = Block (m, map unbreakable es, indent, wd)
  | unbreakable (e as String _) = e;



(** formatting **)

(* margin *)

fun make_margin_info m =
 {margin = m,                   (*right margin, or page width*)
  breakgain = m div 20,         (*minimum added space required of a break*)
  emergencypos = m div 2};      (*position too far to right*)

val margin_info = ref (make_margin_info 76);
fun setmargin m = margin_info := make_margin_info m;
fun setmp_margin m f = setmp margin_info (make_margin_info m) f;


(* depth limitation *)

val depth = ref 0;   (*maximum depth; 0 means no limit*)
fun setdepth dp = (depth := dp);

local
  fun pruning dp (Block (m, bes, indent, wd)) =
        if dp > 0
        then markup_block m (indent, map (pruning (dp - 1)) bes)
        else str "..."
    | pruning dp e = e
in
  fun prune e = if ! depth > 0 then pruning (! depth) e else e;
end;


(* formatted output *)

local

type text = {tx: Buffer.T, ind: Buffer.T, pos: int, nl: int};

val empty: text =
 {tx = Buffer.empty,
  ind = Buffer.empty,
  pos = 0,
  nl = 0};

fun newline {tx, ind, pos, nl} : text =
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
fun breakdist (Block (_, _, _, len) :: es, after) = len + breakdist (es, after)
  | breakdist (String (s, len) :: es, after) = len + breakdist (es, after)
  | breakdist (Break _ :: es, after) = 0
  | breakdist ([], after) = after;

(*Search for the next break (at this or higher levels) and force it to occur.*)
fun forcenext [] = []
  | forcenext (Break (_, wd) :: es) = Break (true, 0) :: es
  | forcenext (e :: es) = e :: forcenext es;

(*es is list of expressions to print;
  blockin is the indentation of the current block;
  after is the width of the following context until next break.*)
fun format ([], _, _) text = text
  | format (e :: es, block as (blockind, blockin), after) (text as {ind, pos, nl, ...}) =
      (case e of
        Block (markup, bes, indent, wd) =>
          let
            val {emergencypos, ...} = ! margin_info;
            val pos' = pos + indent;
            val pos'' = pos' mod emergencypos;
            val block' =
              if pos' < emergencypos then (ind |> add_indent indent, pos')
              else (add_indent pos'' Buffer.empty, pos'');
            val (bg, en) = Markup.output markup;
            val btext: text = text
              |> control bg
              |> format (bes, block', breakdist (es, after))
              |> control en;
            (*if this block was broken then force the next break*)
            val es' = if nl < #nl btext then forcenext es else es;
          in format (es', block, after) btext end
      | String str => format (es, block, after) (string str text)
      | Break (force, wd) =>
          let val {margin, breakgain, ...} = ! margin_info in
            (*no break if text to next break fits on this line
              or if breaking would add only breakgain to space*)
            format (es, block, after)
              (if not force andalso
                  pos + wd <= Int.max (margin - breakdist (es, after), blockin + breakgain)
                then text |> blanks wd  (*just insert wd blanks*)
                else text |> newline |> indentation block)
          end);

in

fun formatted e = #tx (format ([prune e], (Buffer.empty, 0), 0) empty);

end;


(* special output *)

(*symbolic markup -- no formatting*)
fun symbolic prt =
  let
    fun out (Block (m, [], _, _)) = Buffer.markup m I
      | out (Block (m, prts, indent, _)) =
          Buffer.markup m (Buffer.markup (Markup.block indent) (fold out prts))
      | out (String (s, _)) = Buffer.add s
      | out (Break (false, wd)) = Buffer.markup (Markup.break wd) (Buffer.add (output_spaces wd))
      | out (Break (true, _)) = Buffer.markup Markup.fbreak (Buffer.add (output_spaces 1));
  in out prt Buffer.empty end;

(*unformatted output*)
fun unformatted prt =
  let
    fun fmt (Block (m, prts, _, _)) = Buffer.markup m (fold fmt prts)
      | fmt (String (s, _)) = Buffer.add s
      | fmt (Break (false, wd)) = Buffer.add (output_spaces wd)
      | fmt (Break (true, _)) = Buffer.add (output_spaces 1);
  in fmt (prune prt) Buffer.empty end;

(*ML toplevel pretty printing*)
fun pprint prt (put_str, begin_blk, put_brk, put_fbrk, end_blk) =
  let
    fun pp (Block (_, prts, ind, _)) = (begin_blk ind; pp_lst prts; end_blk ())
      | pp (String (s, _)) = put_str s
      | pp (Break (false, wd)) = put_brk wd
      | pp (Break (true, _)) = put_fbrk ()
    and pp_lst [] = ()
      | pp_lst (prt :: prts) = (pp prt; pp_lst prts);
  in pp (prune prt) end;


(* output interfaces *)

val symbolicN = "pretty_symbolic";

fun output_buffer prt =
  if print_mode_active symbolicN then symbolic prt
  else formatted prt;

val output = Buffer.content o output_buffer;
val string_of = Output.escape o output;
val str_of = Output.escape o Buffer.content o unformatted;
val writeln = Output.writeln o string_of;

end;
