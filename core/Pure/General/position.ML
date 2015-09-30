(*  Title:      Pure/General/position.ML
    Author:     Markus Wenzel, TU Muenchen

Source positions: counting Isabelle symbols, starting from 1.
*)

signature POSITION =
sig
  eqtype T
  val make: {line: int, offset: int, end_offset: int, props: Properties.T} -> T
  val dest: T -> {line: int, offset: int, end_offset: int, props: Properties.T}
  val line_of: T -> int option
  val offset_of: T -> int option
  val end_offset_of: T -> int option
  val file_of: T -> string option
  val advance: Symbol.symbol -> T -> T
  val advance_offset: int -> T -> T
  val distance_of: T -> T -> int
  val none: T
  val start: T
  val file_name: string -> Properties.T
  val file_only: string -> T
  val file: string -> T
  val line_file_only: int -> string -> T
  val line_file: int -> string -> T
  val line: int -> T
  val id: string -> T
  val id_only: string -> T
  val get_id: T -> string option
  val put_id: string -> T -> T
  val parse_id: T -> int option
  val of_properties: Properties.T -> T
  val properties_of: T -> Properties.T
  val def_properties_of: T -> Properties.T
  val entity_properties_of: bool -> serial -> T -> Properties.T
  val default_properties: T -> Properties.T -> Properties.T
  val markup: T -> Markup.T -> Markup.T
  val is_reported: T -> bool
  val is_reported_range: T -> bool
  val reported_text: T -> Markup.T -> string -> string
  val report_text: T -> Markup.T -> string -> unit
  val report: T -> Markup.T -> unit
  type report = T * Markup.T
  type report_text = report * string
  val reports_text: report_text list -> unit
  val reports: report list -> unit
  val store_reports: report_text list Unsynchronized.ref ->
    T list -> ('a -> Markup.T list) -> 'a -> unit
  val append_reports: report_text list Unsynchronized.ref -> report list -> unit
  val here: T -> string
  val here_list: T list -> string
  type range = T * T
  val no_range: range
  val set_range: range -> T
  val reset_range: T -> T
  val range: T -> T -> range
  val range_of_properties: Properties.T -> range
  val properties_of_range: range -> Properties.T
  val thread_data: unit -> T
  val setmp_thread_data: T -> ('a -> 'b) -> 'a -> 'b
  val default: T -> bool * T
end;

structure Position: POSITION =
struct

(* datatype position *)

datatype T = Pos of (int * int * int) * Properties.T;

fun norm_props (props: Properties.T) =
  maps (fn a => the_list (find_first (fn (b, _) => a = b) props))
    Markup.position_properties';

fun make {line = i, offset = j, end_offset = k, props} = Pos ((i, j, k), norm_props props);
fun dest (Pos ((i, j, k), props)) = {line = i, offset = j, end_offset = k, props = props};

fun valid (i: int) = i > 0;
fun if_valid i i' = if valid i then i' else i;


(* fields *)

fun line_of (Pos ((i, _, _), _)) = if valid i then SOME i else NONE;
fun offset_of (Pos ((_, j, _), _)) = if valid j then SOME j else NONE;
fun end_offset_of (Pos ((_, _, k), _)) = if valid k then SOME k else NONE;

fun file_of (Pos (_, props)) = Properties.get props Markup.fileN;


(* advance *)

fun advance_count "\n" (i: int, j: int, k: int) =
      (if_valid i (i + 1), if_valid j (j + 1), k)
  | advance_count s (i, j, k) =
      if Symbol.not_eof s then (i, if_valid j (j + 1), k)
      else (i, j, k);

fun invalid_count (i, j, _: int) =
  not (valid i orelse valid j);

fun advance sym (pos as (Pos (count, props))) =
  if invalid_count count then pos else Pos (advance_count sym count, props);

fun advance_offset offset (pos as (Pos (count as (i, j, k), props))) =
  if invalid_count count then pos
  else if valid i then raise Fail "Illegal line position"
  else Pos ((i, if_valid j (j + offset), k), props);


(* distance of adjacent positions *)

fun distance_of (Pos ((_, j, _), _)) (Pos ((_, j', _), _)) =
  if valid j andalso valid j' then j' - j
  else 0;


(* make position *)

val none = Pos ((0, 0, 0), []);
val start = Pos ((1, 1, 0), []);


fun file_name "" = []
  | file_name name = [(Markup.fileN, name)];

fun file_only name = Pos ((0, 0, 0), file_name name);
fun file name = Pos ((1, 1, 0), file_name name);

fun line_file_only i name = Pos ((i, 0, 0), file_name name);
fun line_file i name = Pos ((i, 1, 0), file_name name);
fun line i = line_file i "";

fun id id = Pos ((0, 1, 0), [(Markup.idN, id)]);
fun id_only id = Pos ((0, 0, 0), [(Markup.idN, id)]);

fun get_id (Pos (_, props)) = Properties.get props Markup.idN;
fun put_id id (Pos (count, props)) = Pos (count, Properties.put (Markup.idN, id) props);

fun parse_id pos = Option.map Markup.parse_int (get_id pos);


(* markup properties *)

fun get props name =
  (case Properties.get props name of
    NONE => 0
  | SOME s => Markup.parse_int s);

fun of_properties props =
  make {line = get props Markup.lineN,
    offset = get props Markup.offsetN,
    end_offset = get props Markup.end_offsetN,
    props = props};

fun value k i = if valid i then [(k, Markup.print_int i)] else [];

fun properties_of (Pos ((i, j, k), props)) =
  value Markup.lineN i @ value Markup.offsetN j @ value Markup.end_offsetN k @ props;

val def_properties_of = properties_of #> map (fn (x, y) => ("def_" ^ x, y));

fun entity_properties_of def serial pos =
  if def then (Markup.defN, Markup.print_int serial) :: properties_of pos
  else (Markup.refN, Markup.print_int serial) :: def_properties_of pos;

fun default_properties default props =
  if exists (member (op =) Markup.position_properties o #1) props then props
  else properties_of default @ props;

val markup = Markup.properties o properties_of;


(* reports *)

fun is_reported pos = is_some (offset_of pos) andalso is_some (get_id pos);
fun is_reported_range pos = is_reported pos andalso is_some (end_offset_of pos);

fun reported_text pos m txt = if is_reported pos then Markup.markup (markup pos m) txt else "";
fun report_text pos markup txt = Output.report [reported_text pos markup txt];
fun report pos markup = report_text pos markup "";

type report = T * Markup.T;
type report_text = report * string;

val reports_text =
  map (fn ((pos, m), txt) => if is_reported pos then Markup.markup (markup pos m) txt else "")
  #> Output.report;

val reports = map (rpair "") #> reports_text;

fun store_reports _ [] _ _ = ()
  | store_reports (r: report_text list Unsynchronized.ref) ps markup x =
      let val ms = markup x
      in Unsynchronized.change r (fold (fn p => fold (fn m => cons ((p, m), "")) ms) ps) end;

fun append_reports (r: report_text list Unsynchronized.ref) reports =
  Unsynchronized.change r (append (map (rpair "") reports));


(* here: user output *)

fun here pos =
  let
    val props = properties_of pos;
    val (s1, s2) =
      (case (line_of pos, file_of pos) of
        (SOME i, NONE) => (" ", "(line " ^ Markup.print_int i ^ ")")
      | (SOME i, SOME name) => (" ", "(line " ^ Markup.print_int i ^ " of " ^ quote name ^ ")")
      | (NONE, SOME name) => (" ", "(file " ^ quote name ^ ")")
      | _ => if is_reported pos then ("", "\\<here>") else ("", ""));
  in
    if null props then ""
    else s1 ^ Markup.markup (Markup.properties props Markup.position) s2
  end;

val here_list = space_implode " " o map here;


(* range *)

type range = T * T;

val no_range = (none, none);

fun set_range (Pos ((i, j, _), props), Pos ((_, j', _), _)) = Pos ((i, j, j'), props);
fun reset_range (Pos ((i, j, _), props)) = Pos ((i, j, 0), props);

fun range pos pos' = (set_range (pos, pos'), pos');

fun range_of_properties props =
  let
    val pos = of_properties props;
    val pos' =
      make {line = get props Markup.end_lineN,
        offset = get props Markup.end_offsetN,
        end_offset = 0,
        props = props};
  in (pos, pos') end;

fun properties_of_range (pos, pos') =
  properties_of pos @ value Markup.end_lineN (the_default 0 (line_of pos'));


(* thread data *)

local val tag = Universal.tag () : T Universal.tag in

fun thread_data () = the_default none (Thread.getLocal tag);

fun setmp_thread_data pos = Library.setmp_thread_data tag (thread_data ()) pos;

end;

fun default pos =
  if pos = none then (false, thread_data ())
  else (true, pos);

end;
