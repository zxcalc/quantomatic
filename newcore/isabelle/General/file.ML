(*  Title:      Pure/General/file.ML
    Author:     Markus Wenzel, TU Muenchen

File system operations.
*)

signature FILE =
sig
  val platform_path: Path.T -> string
  val shell_quote: string -> string
  val shell_path: Path.T -> string
  val cd: Path.T -> unit
  val pwd: unit -> Path.T
  val full_path: Path.T -> Path.T -> Path.T
  val tmp_path: Path.T -> Path.T
  val exists: Path.T -> bool
  val rm: Path.T -> unit
  val is_dir: Path.T -> bool
  val check_dir: Path.T -> Path.T
  val check_file: Path.T -> Path.T
  val open_dir: (OS.FileSys.dirstream -> 'a) -> Path.T -> 'a
  val open_input: (TextIO.instream -> 'a) -> Path.T -> 'a
  val open_output: (TextIO.outstream -> 'a) -> Path.T -> 'a
  val open_append: (TextIO.outstream -> 'a) -> Path.T -> 'a
  val fold_dir: (string -> 'a -> 'a) -> Path.T -> 'a -> 'a
  val read_dir: Path.T -> string list
  val fold_lines: (string -> 'a -> 'a) -> Path.T -> 'a -> 'a
  val fold_pages: (string -> 'a -> 'a) -> Path.T -> 'a -> 'a
  val read_lines: Path.T -> string list
  val read_pages: Path.T -> string list
  val read: Path.T -> string
  val write: Path.T -> string -> unit
  val append: Path.T -> string -> unit
  val write_list: Path.T -> string list -> unit
  val append_list: Path.T -> string list -> unit
  val write_buffer: Path.T -> Buffer.T -> unit
  val eq: Path.T * Path.T -> bool
  val copy: Path.T -> Path.T -> unit
end;

structure File: FILE =
struct

(* system path representations *)

val platform_path = Path.implode o Path.expand;

val shell_quote = enclose "'" "'";
val shell_path = shell_quote o platform_path;


(* current working directory *)

val cd = cd o platform_path;
val pwd = Path.explode o pwd;


(* full_path *)

fun full_path dir path =
  let
    val path' = Path.expand path;
    val _ = Path.is_current path' andalso error "Bad file specification";
    val path'' = Path.append dir path';
  in
    if Path.is_absolute path'' then path''
    else Path.append (pwd ()) path''
  end;


(* tmp_path *)

fun tmp_path path =
  Path.append (Path.variable "ISABELLE_TMP") (Path.base path);


(* directory entries *)

val exists = can OS.FileSys.modTime o platform_path;

val rm = OS.FileSys.remove o platform_path;

fun is_dir path =
  the_default false (try OS.FileSys.isDir (platform_path path));

fun check_dir path =
  if exists path andalso is_dir path then path
  else error ("No such directory: " ^ Path.print path);

fun check_file path =
  if exists path andalso not (is_dir path) then path
  else error ("No such file: " ^ Path.print path);


(* open streams *)

local

fun with_file open_file close_file f path =
  let val file = open_file path
  in Exn.release (Exn.capture f file before close_file file) end;

in

fun open_dir f = with_file OS.FileSys.openDir OS.FileSys.closeDir f o platform_path;
fun open_input f = with_file TextIO.openIn TextIO.closeIn f o platform_path;
fun open_output f = with_file TextIO.openOut TextIO.closeOut f o platform_path;
fun open_append f = with_file TextIO.openAppend TextIO.closeOut f o platform_path;

end;


(* directory content *)

fun fold_dir f path a = open_dir (fn stream =>
  let
    fun read x =
      (case OS.FileSys.readDir stream of
        NONE => x
      | SOME entry => read (f entry x));
  in read a end) path;

fun read_dir path = rev (fold_dir cons path []);


(* input *)

(*
  scalable iterator:
  . avoid size limit of TextIO.inputAll and overhead of many TextIO.inputLine
  . optional terminator at end-of-input
*)
fun fold_chunks terminator f path a = open_input (fn file =>
  let
    fun read buf x =
      (case TextIO.input file of
        "" => (case Buffer.content buf of "" => x | line => f line x)
      | input =>
          (case String.fields (fn c => c = terminator) input of
            [rest] => read (Buffer.add rest buf) x
          | line :: more => read_lines more (f (Buffer.content (Buffer.add line buf)) x)))
    and read_lines [rest] x = read (Buffer.add rest Buffer.empty) x
      | read_lines (line :: more) x = read_lines more (f line x);
  in read Buffer.empty a end) path;

fun fold_lines f = fold_chunks #"\n" f;
fun fold_pages f = fold_chunks #"\f" f;

fun read_lines path = rev (fold_lines cons path []);
fun read_pages path = rev (fold_pages cons path []);

val read = open_input TextIO.inputAll;


(* output *)

fun output txts file = List.app (fn txt => TextIO.output (file, txt)) txts;

fun write_list path txts = open_output (output txts) path;
fun append_list path txts = open_append (output txts) path;

fun write path txt = write_list path [txt];
fun append path txt = append_list path [txt];

fun write_buffer path buf = open_output (Buffer.output buf) path;


(* copy *)

fun eq paths =
  (case try (pairself (OS.FileSys.fileId o platform_path)) paths of
    SOME ids => is_equal (OS.FileSys.compare ids)
  | NONE => false);

fun copy src dst =
  if eq (src, dst) then ()
  else
    let val target = if is_dir dst then Path.append dst (Path.base src) else dst
    in write target (read src) end;

end;
