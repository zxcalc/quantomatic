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
  val starts_basic: T -> bool
  val append: T -> T -> T
  val appends: T list -> T
  val make: string list -> T
  val implode: T -> string
  val explode: string -> T
  val split: string -> T list
  val pretty: T -> Pretty.T
  val print: T -> string
  val dir: T -> T
  val base: T -> T
  val ext: string -> T -> T
  val split_ext: T -> T * string
  val expand: T -> T
  val smart_implode: T -> string
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

fun err_elem msg s = error (msg ^ " path element specification " ^ quote s);

fun check_elem s =
  if s = "" orelse s = "~" orelse s = "~~" then err_elem "Illegal" s
  else
    let
      fun check c =
        if exists_string (fn c' => c = c') s then
          err_elem ("Illegal character " ^ quote c ^ " in") s
        else ();
      val _ = List.app check ["/", "\\", "$", ":", "\"", "'"];
    in s end;

in

val root_elem = Root o check_elem;
val basic_elem = Basic o check_elem;
val variable_elem = Variable o check_elem;

end;


(* type path *)

datatype T = Path of elem list;    (*reversed elements*)

fun rep (Path xs) = xs;

fun is_current (Path []) = true
  | is_current _ = false;

val current = Path [];
val root = Path [Root ""];
fun named_root s = Path [root_elem s];
fun basic s = Path [basic_elem s];
fun variable s = Path [variable_elem s];
val parent = Path [Parent];

fun is_absolute (Path xs) =
  (case try List.last xs of
    SOME (Root _) => true
  | _ => false);

fun is_basic (Path [Basic _]) = true
  | is_basic _ = false;

fun starts_basic (Path xs) =
  (case try List.last xs of
    SOME (Basic _) => true
  | _ => false);


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

fun explode_path str =
  let
    fun explode_elem s =
     (if s = ".." then Parent
      else if s = "~" then Variable "USER_HOME"
      else if s = "~~" then Variable "ISABELLE_HOME"
      else
        (case try (unprefix "$") s of
          SOME s' => variable_elem s'
        | NONE => basic_elem s))
      handle ERROR msg => cat_error msg ("The error(s) above occurred in " ^ quote str);

    val (roots, raw_elems) =
      (case take_prefix (equal "") (space_explode "/" str) |>> length of
        (0, es) => ([], es)
      | (1, es) => ([Root ""], es)
      | (_, []) => ([Root ""], [])
      | (_, e :: es) => ([root_elem e], es));
    val elems = raw_elems |> filter_out (fn c => c = "" orelse c = ".") |> map explode_elem;

  in Path (norm (rev elems @ roots)) end;

fun split str =
  space_explode ":" str
  |> map_filter (fn s => if s = "" then NONE else SOME (explode_path s));


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


(* smart implode *)

fun smart_implode path =
  let
    val full_name = implode_path (expand path);
    fun fold_path a =
      let val b = implode_path (expand (explode_path a)) in
        if full_name = b then SOME a
        else
          (case try (unprefix (b ^ "/")) full_name of
            SOME name => SOME (a ^ "/" ^ name)
          | NONE => NONE)
      end;
  in
    (case get_first fold_path ["~~", "$ISABELLE_HOME_USER", "~"] of
      SOME name => name
    | NONE => implode_path path)
  end;

val position = Position.file o smart_implode;

(*final declarations of this structure!*)
val implode = implode_path;
val explode = explode_path;

end;

