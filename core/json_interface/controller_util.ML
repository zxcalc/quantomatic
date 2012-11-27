infix 5 -:

structure ControllerUtil =
struct

(* Fixed set of types to annotate protocol calls. Used to
   generate bindings. *)
datatype ptype =
  list_t of ptype |
  string_t | int_t | json_t |
  graphname_t | vertexname_t | edgename_t | bboxname_t | rulename_t |
  graph_t | rule_t

(* expect single arg or named args *)
datatype pargs =
  S of ptype |
  N of (string * ptype) list

(* TODO: Pretty.T versions *)
fun string_of_ptype x = case x
      of list_t x     => "[" ^ string_of_ptype x ^ "]"
       | string_t     => "string"
       | int_t        => "int"
       | json_t       => "json"
       | graphname_t  => "graphname"
       | vertexname_t => "vertexname"
       | edgename_t   => "edgename"
       | bboxname_t   => "bboxname"
       | rulename_t   => "rulename"
       | graph_t  => "graph"
       | rule_t   => "rule"

fun string_of_pargs (S x) = string_of_ptype x
  | string_of_pargs (N xs) =
      "{ " ^
        String.concatWith ", "
        (map (fn (k,t) => k ^ ": " ^ string_of_ptype t) xs) ^
      " }"

fun (nm:string) -: (typ:ptype) = (nm, typ)

type fdesc = {
  name : string,
  doc: string,
  input: pargs,
  output: pargs
}
type ftab = ((Json.json -> Json.json) * fdesc) Symtab.table

fun register (fd : fdesc) f = Symtab.update_new (#name fd, (f,fd))

(* got bad input from the protocol, should crash *)
exception protocol_exn of string

(* got bad input from the user, should report error and carry on *)
exception user_exn of string

(* {{{ JSON argument helpers *)
fun optarg_json x arg =
let
  val obj = case x of Json.Object y => y | _ => raise protocol_exn "Expected: JSON object"
in Json.lookup obj arg
end

fun optarg_str x arg =
  case optarg_json x arg
    of SOME (Json.String y) => SOME y
     | SOME _ => raise protocol_exn ("Expected: string for arg: " ^ arg)
     | NONE   => NONE
     
fun optarg_str x arg =
  case optarg_json x arg
    of SOME (Json.String y) => SOME y
     | SOME _ => raise protocol_exn ("Expected: string for arg: " ^ arg)
     | NONE   => NONE

fun optarg_int x arg =
  case optarg_json x arg
    of SOME (Json.Int y) => SOME y
     | SOME _ => raise protocol_exn ("Expected: string for arg: " ^ arg)
     | NONE   => NONE


fun arg_json x arg = case optarg_json x arg of SOME x => x
                     | NONE => raise protocol_exn ("Could not find arg: " ^ arg)

fun arg_str x arg = case optarg_str x arg of SOME y => y
                    | NONE => raise protocol_exn ("Could not find arg: " ^ arg)

fun arg_int x arg = case optarg_int x arg of SOME y => y
                    | NONE => raise protocol_exn ("Could not find arg: " ^ arg)

(* }}} *)

end

