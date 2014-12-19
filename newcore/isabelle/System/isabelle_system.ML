(*  Title:      Pure/System/isabelle_system.ML
    Author:     Makarius

Isabelle system support.
*)

signature ISABELLE_SYSTEM =
sig
  val isabelle_tool: string -> string -> int
  val mkdirs: Path.T -> unit
  val mkdir: Path.T -> unit
  val copy_dir: Path.T -> Path.T -> unit
  val copy_file: Path.T -> Path.T -> unit
  val copy_file_base: Path.T * Path.T -> Path.T -> unit
  val create_tmp_path: string -> string -> Path.T
  val with_tmp_file: string -> string -> (Path.T -> 'a) -> 'a
  val with_tmp_dir: string -> (Path.T -> 'a) -> 'a
  val with_tmp_fifo: (Path.T -> 'a) -> 'a
  val bash_output: string -> string * int
  val bash: string -> int
end;

structure Isabelle_System: ISABELLE_SYSTEM =
struct

(* bash *)

fun bash_output s =
  let
    val {out, err, rc, ...} = Bash.process s;
    val _ = warning (trim_line err);
  in (out, rc) end;

fun bash s =
  let
    val (out, rc) = bash_output s;
    val _ = writeln (trim_line out);
  in rc end;


(* system commands *)

fun isabelle_tool name args =
  (case space_explode ":" (getenv "ISABELLE_TOOLS") |> get_first (fn dir =>
      let val path = File.platform_path (Path.append (Path.explode dir) (Path.basic name)) in
        if can OS.FileSys.modTime path andalso
          not (OS.FileSys.isDir path) andalso
          OS.FileSys.access (path, [OS.FileSys.A_READ, OS.FileSys.A_EXEC])
        then SOME path
        else NONE
      end handle OS.SysErr _ => NONE) of
    SOME path => bash (File.shell_quote path ^ " " ^ args)
  | NONE => (warning ("Unknown Isabelle tool: " ^ name); 2));

fun system_command cmd =
  if bash cmd <> 0 then error ("System command failed: " ^ cmd)
  else ();


(* directory operations *)

fun mkdirs path = system_command ("mkdir -p " ^ File.shell_path path);

fun mkdir path =
  if File.is_dir path then () else OS.FileSys.mkDir (File.platform_path path);

fun copy_dir src dst =
  if File.eq (src, dst) then ()
  else (system_command ("cp -p -R -f " ^ File.shell_path src ^ "/. " ^ File.shell_path dst); ());

fun copy_file src0 dst0 =
  let
    val src = Path.expand src0;
    val dst = Path.expand dst0;
    val target = if File.is_dir dst then Path.append dst (Path.base src) else dst;
  in
    if File.eq (src, target) then ()
    else
      ignore (system_command ("cp -p -f " ^ File.shell_path src ^ " " ^ File.shell_path target))
  end;

fun copy_file_base (base_dir, src0) target_dir =
  let
    val src = Path.expand src0;
    val src_dir = Path.dir src;
    val _ =
      if Path.starts_basic src then ()
      else error ("Illegal path specification " ^ Path.print src ^ " beyond base directory");
    val _ = mkdirs (Path.append target_dir src_dir);
  in copy_file (Path.append base_dir src) (Path.append target_dir src) end;


(* tmp files *)

fun create_tmp_path name ext =
  let
    val path = File.tmp_path (Path.basic (name ^ serial_string ()) |> Path.ext ext);
    val _ = File.exists path andalso
      raise Fail ("Temporary file already exists: " ^ Path.print path);
  in path end;

fun with_tmp_file name ext f =
  let val path = create_tmp_path name ext
  in Exn.release (Exn.capture f path before ignore (try File.rm path)) end;


(* tmp dirs *)

fun rm_tree path = system_command ("rm -r -f " ^ File.shell_path path);

fun with_tmp_dir name f =
  let
    val path = create_tmp_path name "";
    val _ = mkdirs path;
  in Exn.release (Exn.capture f path before ignore (try rm_tree path)) end;


(* fifo *)

fun with_tmp_fifo f =
  with_tmp_file "isabelle-fifo-" ""
    (fn path =>
      (case bash_output ("mkfifo -m 600 " ^ File.shell_path path) of
        (_, 0) => f path
      | (out, _) => error (trim_line out)));

end;

