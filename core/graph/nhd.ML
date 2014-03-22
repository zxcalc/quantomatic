PolyML.SaveState.loadState "../heaps/quanto.heap";
PolyML.Compiler.printDepth:=100;

signature NHD =
sig

datatype expr =
  NM of E.name |
  LEFT of B.name * (expr list) |
  RIGHT of B.name * (expr list)

type T

val mk : expr list -> T
val empty : T

val of_json : Json.json -> T
val json_of : T -> Json.json

val rename : (ESub.T * BSub.T) -> T -> T
val eq : T * T -> bool

exception nhd_exn of string

end

structure Nhd : NHD =
struct

exception nhd_exn of string

datatype expr =
  NM of E.name |
  LEFT of B.name * (expr list) |
  RIGHT of B.name * (expr list)

datatype T = NHD of expr list

val mk = NHD

val empty = NHD []

fun rename (emap, bmap) (NHD exprs) =
let
  fun rn (NM e) = NM (ESub.get emap e)
    | rn (LEFT (bb, es)) = LEFT (BSub.get bmap bb, map rn es)
    | rn (RIGHT (bb, es)) = RIGHT (BSub.get bmap bb, map rn es)
in NHD (map rn exprs)
end

fun eq (NHD exprs, NHD exprs') =
let
  fun ex_eq (LEFT (bb,es), LEFT (bb',es')) =
       (B.name_eq (bb, bb') andalso
        forall ex_eq (ListPair.zipEq (es,es'))
        handle ListPair.UnequalLengths => false)
    | ex_eq (RIGHT (bb,es), RIGHT (bb',es')) =
       (B.name_eq (bb, bb') andalso
        forall ex_eq (ListPair.zipEq (es,es'))
        handle ListPair.UnequalLengths => false)
    | ex_eq (NM e, NM e') = E.name_eq (e,e')
in forall ex_eq (ListPair.zipEq (exprs,exprs'))
   handle ListPair.UnequalLengths => false
end

(*fun expand bb (emap, bmap) (NHD exprs) =
let
  fun ex (LEFT (bb', exprs') rest =
    if B.name_eq (bb, bb')
    then LEFT (bb, exprs') :: exprs' @ rest
    else LEFT (bb', fold_rev ex rest []) :: rest
*)


fun expr_of_json (Json.Array lst) =
 (case lst
    of (Json.String bb :: Json.String "<" :: rest) =>
         LEFT (B.mk bb, map expr_of_json rest)
     | (Json.String bb :: Json.String ">" :: rest) =>
         RIGHT (B.mk bb, map expr_of_json rest)
     | _ => raise nhd_exn "expected: [BB_NAME, DIR, ....]")
  | expr_of_json (Json.String e) = NM (E.mk e)
  | expr_of_json _ = raise nhd_exn "expected: array or string"

fun of_json (Json.Object obj) =
  (case Json.lookup obj "expr"
     of SOME (Json.Array lst) => mk (map expr_of_json lst)
      | SOME _ => raise nhd_exn "expected: array"
      | NONE => raise nhd_exn "expected: field 'expr'")
  | of_json _ = raise nhd_exn "expected: object"

fun json_of_expr (LEFT (bb, es)) =
      Json.Array ([Json.String (B.dest bb), Json.String "<"] @ map json_of_expr es)
  | json_of_expr (RIGHT (bb, es)) =
      Json.Array ([Json.String (B.dest bb), Json.String ">"] @ map json_of_expr es)
  | json_of_expr (NM e) = Json.String (E.dest e)

fun json_of (NHD exps) = Json.mk_object [("expr", Json.Array (map json_of_expr exps))]


end

val test = Json.of_string "{ \"expr\": [[\"A\", \"<\", \"a\", [\"B\", \">\", \"b\", \"c\"], \"d\"]] }"
