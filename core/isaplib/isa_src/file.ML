(*  Title:      Pure/General/file.ML
    ID:         $Id: file.ML,v 1.45 2008/05/24 12:47:43 wenzelm Exp $
    Author:     Markus Wenzel, TU Muenchen

File system operations.
*)

signature FILE =
sig
  val platform_path: Path.T -> string
  val shell_path: Path.T -> string
  val cd: Path.T -> unit
  val pwd: unit -> Path.T
  val full_path: Path.T -> Path.T
  val tmp_path: Path.T -> Path.T
  val isatool: string -> int
  val system_command: string -> unit
  eqtype ident
  val rep_ident: ident -> string
  val ident: Path.T -> ident option
  val exists: Path.T -> bool
  val check: Path.T -> unit
  val rm: Path.T -> unit
  val mkdir: Path.T -> unit
  val open_input: (TextIO.instream -> 'a) -> Path.T -> 'a
  val open_output: (TextIO.outstream -> 'a) -> Path.T -> 'a
  val open_append: (TextIO.outstream -> 'a) -> Path.T -> 'a
  val read: Path.T -> string
  val write: Path.T -> string -> unit
  val append: Path.T -> string -> unit
  val write_list: Path.T -> string list -> unit
  val append_list: Path.T -> string list -> unit
  val eq: Path.T * Path.T -> bool
  val copy: Path.T -> Path.T -> unit
  val copy_dir: Path.T -> Path.T -> unit
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

fun full_path path =
  if Path.is_absolute path then path
  else Path.append (pwd ()) path;


(* tmp_path *)

fun tmp_path path =
  Path.append (Path.variable "ISABELLE_TMP") (Path.base path);


(* system commands *)

fun isatool cmd = system ("\"$ISATOOL\" " ^ cmd);

fun system_command cmd =
  if system cmd <> 0 then error ("System command failed: " ^ cmd)
  else ();


(* file identification *)

local
  val ident_cache = ref (Symtab.empty: {time_stamp: string, id: string} Symtab.table);
in

fun check_cache (path, time) = CRITICAL (fn () =>
  (case Symtab.lookup (! ident_cache) path of
    NONE => NONE
  | SOME {time_stamp, id} => if time_stamp = time then SOME id else NONE));

fun update_cache (path, (time, id)) = CRITICAL (fn () =>
  change ident_cache (Symtab.update (path, {time_stamp = time, id = id})));

end;

datatype ident = Ident of string;
fun rep_ident (Ident s) = s;

fun ident path =
  let val physical_path = perhaps (try OS.FileSys.fullPath) (platform_path path) in
    (case try (Time.toString o OS.FileSys.modTime) physical_path of
      NONE => NONE
    | SOME mod_time => SOME (Ident
        (case getenv "ISABELLE_FILE_IDENT" of
          "" => physical_path ^ ": " ^ mod_time
        | cmd =>
            (case check_cache (physical_path, mod_time) of
              SOME id => id
            | NONE =>
                let val (id, rc) =  (*potentially slow*)
                  system_out ("\"$ISABELLE_HOME/lib/scripts/fileident\" " ^ shell_quote physical_path)
                in
                  if id <> "" andalso rc = 0 then (update_cache (physical_path, (mod_time, id)); id)
                  else error ("Failed to identify file " ^ quote (Path.implode path) ^ " by " ^ cmd)
                end))))
  end;


(* directory entries *)

val exists = can OS.FileSys.modTime o platform_path;

fun check path =
  if exists path then ()
  else error ("No such file or directory: " ^ quote (Path.implode path));

val rm = OS.FileSys.remove o platform_path;

fun mkdir path = system_command ("mkdir -p " ^ shell_path path);

fun is_dir path =
  the_default false (try OS.FileSys.isDir (platform_path path));


(* read / write files *)

local

fun with_file open_file close_file f path =
  let val file = open_file path
  in Exn.release (Exn.capture f file before close_file file) end;

fun output txts file = List.app (fn txt => TextIO.output (file, txt)) txts;

in

fun open_input f = with_file TextIO.openIn TextIO.closeIn f o platform_path;
fun open_output f = with_file TextIO.openOut TextIO.closeOut f o platform_path;
fun open_append f = with_file TextIO.openAppend TextIO.closeOut f o platform_path;

val read = open_input TextIO.inputAll;

fun write_list path txts = open_output (output txts) path;
fun append_list path txts = open_append (output txts) path;

fun write path txt = write_list path [txt];
fun append path txt = append_list path [txt];

end;

fun eq paths =
  (case try (pairself (OS.FileSys.fileId o platform_path)) paths of
    SOME ids => is_equal (OS.FileSys.compare ids)
  | NONE => false);

fun copy src dst =
  if eq (src, dst) then ()
  else
    let val target = if is_dir dst then Path.append dst (Path.base src) else dst
    in write target (read src) end;

fun copy_dir src dst =
  if eq (src, dst) then ()
  else (system_command ("cp -r -f " ^ shell_path src ^ "/. " ^ shell_path dst); ());

end;
