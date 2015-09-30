(*  Title:      Pure/System/options.ML
    Author:     Makarius

System options with external string representation.
*)

signature OPTIONS =
sig
  val boolT: string
  val intT: string
  val realT: string
  val stringT: string
  val unknownT: string
  type T
  val empty: T
  val names: T -> string list
  val markup: T -> string * Position.T -> Markup.T
  val typ: T -> string -> string
  val bool: T -> string -> bool
  val int: T -> string -> int
  val real: T -> string -> real
  val string: T -> string -> string
  val put_bool: string -> bool -> T -> T
  val put_int: string -> int -> T -> T
  val put_real: string -> real -> T -> T
  val put_string: string -> string -> T -> T
  val declare: {pos: Position.T, name: string, typ: string, value: string} -> T -> T
  val update: string -> string -> T -> T
  val decode: XML.body -> T
  val default: unit -> T
  val default_markup: string * Position.T -> Markup.T
  val default_typ: string -> string
  val default_bool: string -> bool
  val default_int: string -> int
  val default_real: string -> real
  val default_string: string -> string
  val default_put_bool: string -> bool -> unit
  val default_put_int: string -> int -> unit
  val default_put_real: string -> real -> unit
  val default_put_string: string -> string -> unit
  val get_default: string -> string
  val put_default: string -> string -> unit
  val set_default: T -> unit
  val reset_default: unit -> unit
  val load_default: unit -> unit
end;

structure Options: OPTIONS =
struct

(* representation *)

val boolT = "bool";
val intT = "int";
val realT = "real";
val stringT = "string";
val unknownT = "unknown";

datatype T = Options of {pos: Position.T, typ: string, value: string} Symtab.table;

val empty = Options Symtab.empty;

fun names (Options tab) = sort_strings (Symtab.keys tab);


(* check *)

fun check_name (Options tab) name =
  let val opt = Symtab.lookup tab name in
    if is_some opt andalso #typ (the opt) <> unknownT then the opt
    else error ("Unknown system option " ^ quote name)
  end;

fun check_type options name typ =
  let val opt = check_name options name in
    if #typ opt = typ then opt
    else error ("Ill-typed system option " ^ quote name ^ " : " ^ #typ opt ^ " vs. " ^ typ)
  end;


(* markup *)

fun markup options (name, pos) =
  let
    val opt =
      check_name options name
        handle ERROR msg => error (msg ^ Position.here pos);
    val props = Position.def_properties_of (#pos opt);
  in Markup.properties props (Markup.entity Markup.system_optionN name) end;


(* typ *)

fun typ options name = #typ (check_name options name);


(* basic operations *)

fun put T print name x (options as Options tab) =
  let val opt = check_type options name T
  in Options (Symtab.update (name, {pos = #pos opt, typ = #typ opt, value = print x}) tab) end;

fun get T parse options name =
  let val opt = check_type options name T in
    (case parse (#value opt) of
      SOME x => x
    | NONE =>
        error ("Malformed value for system option " ^ quote name ^
          " : " ^ T ^ " =\n" ^ quote (#value opt)))
  end;


(* internal lookup and update *)

val bool = get boolT (try Markup.parse_bool);
val int = get intT (try Markup.parse_int);
val real = get realT (try Markup.parse_real);
val string = get stringT SOME;

val put_bool = put boolT Markup.print_bool;
val put_int = put intT Markup.print_int;
val put_real = put realT Markup.print_real;
val put_string = put stringT I;


(* external updates *)

fun check_value options name =
  let val opt = check_name options name in
    if #typ opt = boolT then ignore (bool options name)
    else if #typ opt = intT then ignore (int options name)
    else if #typ opt = realT then ignore (real options name)
    else if #typ opt = stringT then ignore (string options name)
    else ()
  end;

fun declare {pos, name, typ, value} (Options tab) =
  let
    val options' =
      (case Symtab.lookup tab name of
        SOME other =>
          error ("Duplicate declaration of system option " ^ quote name ^ Position.here pos ^
            Position.here (#pos other))
      | NONE => Options (Symtab.update (name, {pos = pos, typ = typ, value = value}) tab));
    val _ =
      typ = boolT orelse typ = intT orelse typ = realT orelse typ = stringT orelse
        error ("Unknown type for system option " ^ quote name ^ " : " ^ quote typ ^
          Position.here pos);
    val _ = check_value options' name;
  in options' end;

fun update name value (options as Options tab) =
  let
    val opt = check_name options name;
    val options' =
      Options (Symtab.update (name, {pos = #pos opt, typ = #typ opt, value = value}) tab);
    val _ = check_value options' name;
  in options' end;


(* decode *)

fun decode_opt body =
  let open XML.Decode
  in list (pair properties (pair string (pair string string))) end body
  |> map (fn (props, (name, (typ, value))) =>
      {pos = Position.of_properties props, name = name, typ = typ, value = value});

fun decode body = fold declare (decode_opt body) empty;



(** global default **)

val global_default = Synchronized.var "Options.default" (NONE: T option);

fun err_no_default () = error "Missing default for system options within Isabelle process";

fun change_default f x y =
  Synchronized.change global_default
    (fn SOME options => SOME (f x y options)
      | NONE => err_no_default ());

fun default () =
  (case Synchronized.value global_default of
    SOME options => options
  | NONE => err_no_default ());

fun default_markup arg = markup (default ()) arg;
fun default_typ name = typ (default ()) name;
fun default_bool name = bool (default ()) name;
fun default_int name = int (default ()) name;
fun default_real name = real (default ()) name;
fun default_string name = string (default ()) name;

val default_put_bool = change_default put_bool;
val default_put_int = change_default put_int;
val default_put_real = change_default put_real;
val default_put_string = change_default put_string;

fun get_default name =
  let val options = default () in get (typ options name) SOME options name end;
val put_default = change_default update;

fun set_default options = Synchronized.change global_default (K (SOME options));
fun reset_default () = Synchronized.change global_default (K NONE);

fun load_default () =
  (case getenv "ISABELLE_PROCESS_OPTIONS" of
    "" => ()
  | name =>
      let val path = Path.explode name in
        (case try File.read path of
          SOME s => (set_default (decode (YXML.parse_body s)); ignore (try File.rm path))
        | NONE => ())
      end);

val _ = load_default ();

end;

