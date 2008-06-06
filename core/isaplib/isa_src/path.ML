(*  Title:      Pure/General/path.ML
    ID:         $Id: path.ML,v 1.22 2008/05/14 09:05:08 wenzelm Exp $
    Author:     Markus Wenzel, TU Muenchen

Abstract algebra of file paths (external encoding in Unix style).
*)

signature PATH =
sig
  eqtype T
  val is_current: T -> bool
  val current: T
  val root: T
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

datatype elem = Root | Parent | Basic of string | Variable of string;

fun err_elem msg chs = error (msg ^ " path element specification: " ^ quote (implode chs));

fun check_elem (chs as []) = err_elem "Illegal" chs
  | check_elem (chs as ["~"]) = err_elem "Illegal" chs
  | check_elem (chs as ["~", "~"]) = err_elem "Illegal" chs
  | check_elem chs =
      (case ["/", "\\", "$", ":"] inter_string chs of
        [] => chs
      | bads => err_elem ("Illegal character(s) " ^ commas_quote bads ^ " in") chs);

val basic_elem = Basic o implode o check_elem;
val variable_elem = Variable o implode o check_elem;

fun is_var (Variable _) = true
  | is_var _ = false;


(* type path *)

datatype T = Path of elem list;

fun rep (Path xs) = xs;

fun is_current (Path []) = true
  | is_current _ = false;

val current = Path [];
val root = Path [Root];
val parent = Path [Parent];
fun basic s = Path [basic_elem (explode s)];
fun variable s = Path [variable_elem (explode s)];

fun is_absolute (Path (Root :: _)) = true
  | is_absolute _ = false;

fun is_basic (Path [Basic _]) = true
  | is_basic _ = false;


(* append and norm *)

(*append non-normal path (2n arg) to reversed normal one, result is normal*)
fun rev_app xs [] = rev xs
  | rev_app _ (Root :: ys) = rev_app [Root] ys
  | rev_app (x :: xs) (Parent :: ys) =
      if x = Parent orelse is_var x then rev_app (Parent :: x :: xs) ys
      else if x = Root then rev_app (x :: xs) ys
      else rev_app xs ys
  | rev_app xs (y :: ys) = rev_app (y :: xs) ys;

fun append (Path xs) (Path ys) = Path (rev_app (rev xs) ys);
fun appends paths = Library.foldl (uncurry append) (current, paths);
val make = appends o map basic;
fun norm path = rev_app [] path;


(* implode *)

fun implode_elem Root = ""
  | implode_elem Parent = ".."
  | implode_elem (Basic s) = s
  | implode_elem (Variable s) = "$" ^ s;

fun implode_path (Path []) = "."
  | implode_path (Path (Root :: xs)) = "/" ^ space_implode "/" (map implode_elem xs)
  | implode_path (Path xs) = space_implode "/" (map implode_elem xs);


(* explode *)

fun explode_elem "" = Root
  | explode_elem ".." = Parent
  | explode_elem "~" = Variable "HOME"
  | explode_elem "~~" = Variable "ISABELLE_HOME"
  | explode_elem s =
      (case explode s of
        "$" :: cs => variable_elem cs
      | cs => basic_elem cs);

val explode_elems = map explode_elem o filter_out (fn c => c = "" orelse c = ".");

fun explode_path str = Path (norm
  (case space_explode "/" str of
    "" :: ss => Root :: explode_elems ss
  | ss => explode_elems ss));


(* base element *)

fun split_path f (path as Path xs) =
  (case try split_last xs of
    SOME (prfx, Basic s) => f (prfx, s)
  | _ => error ("Cannot split path into dir/base: " ^ quote (implode_path path)));

val dir = split_path (fn (prfx, _) => Path prfx);
val base = split_path (fn (_, s) => Path [Basic s]);

fun ext "" path = path
  | ext e path = split_path (fn (prfx, s) => append (Path prfx) (basic (s ^ "." ^ e))) path;

val split_ext = split_path (fn (prfx, s) => apfst (append (Path prfx))
  (case take_suffix (fn c => c <> ".") (explode s) of
    ([], _) => (Path [Basic s], "")
  | (cs, e) => (Path [Basic (implode (Library.take (length cs - 1, cs)))], implode e)));


(* expand variables *)

fun eval (Variable s) =
    (case getenv s of
      "" => error ("Undefined Isabelle environment variable: " ^ quote s)
    | path => rep (explode_path path))
  | eval x = [x];

val expand = rep #> maps eval #> norm #> Path;


(* source position *)

val position = Position.file o implode_path o expand;


(*final declarations of this structure!*)
val implode = implode_path;
val explode = explode_path;

end;
