(*  Title:      Pure/General/position.ML
    ID:         $Id: position.ML,v 1.19 2008/05/14 12:43:38 wenzelm Exp $
    Author:     Markus Wenzel, TU Muenchen

Source positions.
*)

signature POSITION =
sig
  type T
  val line_of: T -> int option
  val column_of: T -> int option
  val file_of: T -> string option
  val none: T
  val line_file: int -> string -> T
  val line: int -> T
  val file: string -> T
  val advance: Symbol.symbol -> T -> T
  val of_properties: Markup.property list -> T
  val properties_of: T -> Markup.property list
  val default_properties: T -> Markup.property list -> Markup.property list
  val str_of: T -> string
  val thread_data: unit -> T
  val setmp_thread_data: T -> ('a -> 'b) -> 'a -> 'b
  val setmp_thread_data_seq: T -> ('a -> 'b Seq.seq) -> 'a -> 'b Seq.seq
end;

structure Position: POSITION =
struct

(* datatype position *)

datatype T = Pos of (int * int) option * Markup.property list;

fun line_of (Pos (SOME (m, _), _)) = SOME m
  | line_of _ = NONE;

fun column_of (Pos (SOME (_, n), _)) = SOME n
  | column_of _ = NONE;

fun file_of (Pos (_, props)) = AList.lookup (op =) props Markup.fileN;


val none = Pos (NONE, []);

fun line_file m name = Pos (SOME (m, 1), if name = "" then [] else [(Markup.fileN, name)]);
fun line m = line_file m "";
val file = line_file 1;


fun advance "\n" (Pos (SOME (m, _), props)) = Pos (SOME (m + 1, 1), props)
  | advance s (pos as Pos (SOME (m, n), props)) =
      if Symbol.is_regular s andalso not (Symbol.is_utf8_trailer s)
      then Pos (SOME (m, n + 1), props) else pos
  | advance _ pos = pos;


(* markup properties *)

fun get_int props (name: string) =
  (case AList.lookup (op =) props name of
    NONE => NONE
  | SOME s => Int.fromString s);

fun of_properties props =
  let
    val count =
      (case get_int props Markup.lineN of
        NONE => NONE
      | SOME m => SOME (m, the_default 1 (get_int props Markup.columnN)));
    fun property name = the_list (find_first (fn (x: string, _) => x = name) props);
  in (Pos (count, property Markup.fileN @ property Markup.idN)) end;

fun properties_of (Pos (count, props)) =
  (case count of
    NONE => []
  | SOME (m, n) => [(Markup.lineN, string_of_int m), (Markup.columnN, string_of_int n)]) @ props;

fun default_properties default props =
  if exists (member (op =) Markup.position_properties o #1) props then props
  else properties_of default @ props;


(* str_of *)

fun str_of pos =
  let
    val props = properties_of pos;
    val s =
      (case (line_of pos, file_of pos) of
        (SOME m, NONE) => "(line " ^ string_of_int m ^ ")"
      | (SOME m, SOME name) => "(line " ^ string_of_int m ^ " of " ^ quote name ^ ")"
      | _ => "");
  in
    if null props then ""
    else (if s = "" then "" else " ") ^ Markup.markup (Markup.properties props Markup.position) s
  end;


(* thread data *)

local val tag = Universal.tag () : T Universal.tag in

fun thread_data () = the_default none (Multithreading.get_data tag);

fun setmp_thread_data pos f x =
  if ! Output.debugging then f x
  else Library.setmp_thread_data tag (thread_data ()) pos f x;

fun setmp_thread_data_seq pos f seq =
  setmp_thread_data pos f seq |> Seq.wrap (fn pull => setmp_thread_data pos pull ());

end;

end;
