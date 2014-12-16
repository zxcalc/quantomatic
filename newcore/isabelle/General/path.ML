(*  Title:      Pure/General/path.ML
    Author:     Markus Wenzel, TU Muenchen

Algebra of file-system paths: basic POSIX notation, extended by named
roots (e.g. //foo) and variables (e.g. $BAR).
*)

signature PATH =
sig
  eqtype T
  val is_current: T -> bool
  val current: T
  val root: T
  val named_root: string -> T
  val parent: T
  val basic: string -> T
  val variable: string -> T
  val is_absolute: T -> bool
  val is_basic: T -> bool
  val append: T -> T -> T
  val appends: T list -> T
  val make: string list -> T
  val implode: T -> string
  val explode: string -> T
  val pretty: T -> Pretty.T
  val print: T -> string
  val dir: T -> T
  val base: T -> T
  val ext: string -> T -> T
  val split_ext: T -> T * string
  val expand: T -> T
  val position: T -> Position.T
end;

structure Path: PATH =
struct

(* path elements *)

datatype elem =
  Root of string |
  Basic of string |
  Variable of string |
  Parent;

local

fun err_elem msg chs = error (msg ^ " path element specification: " ^ quote (implode chs));

fun check_elem (chs as []) = err_elem "Illegal" chs
  | check_elem (chs as ["~"]) = err_elem "Illegal" chs
  | check_elem (chs as ["~", "~"]) = err_elem "Illegal" chs
  | check_elem chs =
      (case inter (op =) ["/", "\\", "$", ":", "\"", "'"] chs of
        [] => chs
      | bads => err_elem ("Illegal character(s) " ^ commas_quote bads ^ " in") chs);

in

val root_elem = Root o implode o check_elem;
val basic_elem = Basic o implode o check_elem;
val variable_elem = Variable o implode o check_elem;

end;


(* type path *)

datatype T = Path of elem list;    (*reversed elements*)

fun rep (Path xs) = xs;

fun is_current (Path []) = true
  | is_current _ = false;

val current = Path [];
val root = Path [Root ""];
fun named_root s = Path [root_elem (raw_explode s)];
fun basic s = Path [basic_elem (raw_explode s)];
fun variable s = Path [variable_elem (raw_explode s)];
val parent = Path [Parent];

fun is_absolute (Path xs) =
  (case try List.last xs of
    SOME (Root _) => true
  | _ => false);

fun is_basic (Path [Basic _]) = true
  | is_basic _ = false;


(* append and norm *)

fun apply (y as Root _) _ = [y]
  | apply Parent (xs as (Root _ :: _)) = xs
  | apply Parent (Basic _ :: rest) = rest
  | apply y xs = y :: xs;

fun append (Path xs) (Path ys) = Path (fold_rev apply ys xs);
fun appends paths = Library.foldl (uncurry append) (current, paths);
val make = appends o map basic;

fun norm elems = fold_rev apply elems [];


(* implode *)

local

fun implode_elem (Root "") = ""
  | implode_elem (Root s) = "//" ^ s
  | implode_elem (Basic s) = s
  | implode_elem (Variable s) = "$" ^ s
  | implode_elem Parent = "..";

in

fun implode_path (Path []) = "."
  | implode_path (Path [Root ""]) = "/"
  | implode_path (Path xs) = space_implode "/" (rev (map implode_elem xs));

end;


(* explode *)

local

fun explode_elem ".." = Parent
  | explode_elem "~" = Variable "USER_HOME"
  | explode_elem "~~" = Variable "ISABELLE_HOME"
  | explode_elem s =
      (case raw_explode s of
        "$" :: cs => variable_elem cs
      | cs => basic_elem cs);

val explode_elems =
  rev o map explode_elem o filter_out (fn c => c = "" orelse c = ".");

in

fun explode_path str =
  let val (roots, raw_elems) =
    (case take_prefix (equal "") (space_explode "/" str) |>> length of
      (0, es) => ([], es)
    | (1, es) => ([Root ""], es)
    | (_, []) => ([Root ""], [])
    | (_, e :: es) => ([root_elem (raw_explode e)], es))
  in Path (norm (explode_elems raw_elems @ roots)) end;

end;


(* print *)

fun pretty path =
  let val s = implode_path path
  in Pretty.mark (Markup.path s) (Pretty.str (quote s)) end;

val print = Pretty.str_of o pretty;


(* base element *)

fun split_path f (Path (Basic s :: xs)) = f (Path xs, s)
  | split_path _ path = error ("Cannot split path into dir/base: " ^ print path);

val dir = split_path #1;
val base = split_path (fn (_, s) => Path [Basic s]);

fun ext "" = I
  | ext e = split_path (fn (prfx, s) => append prfx (basic (s ^ "." ^ e)));

val split_ext = split_path (fn (prfx, s) => apfst (append prfx)
  (case take_suffix (fn c => c <> ".") (raw_explode s) of
    ([], _) => (Path [Basic s], "")
  | (cs, e) => (Path [Basic (implode (take (length cs - 1) cs))], implode e)));


(* expand variables *)

fun eval (Variable s) =
      let val path = explode_path (getenv_strict s) in
        if exists (fn Variable _ => true | _ => false) (rep path) then
          error ("Illegal path variable nesting: " ^ s ^ "=" ^ print path)
        else rep path
      end
  | eval x = [x];

val expand = rep #> maps eval #> norm #> Path;


(* source position -- with smart replacement of ISABELLE_HOME *)

val isabelle_home = explode_path "~~";

fun position path =
  let
    val s = implode_path path;
    val prfx = implode_path (expand isabelle_home) ^ "/";
  in
    Position.file
      (case try (unprefix prfx) s of
        NONE => s
      | SOME s' => "~~/" ^ s')
  end;


(*final declarations of this structure!*)
val implode = implode_path;
val explode = explode_path;

end;

