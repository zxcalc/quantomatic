signature NHD =
sig

(* an edge expression *)
datatype expr =
  I of E.name | (* input *)
  O of E.name | (* output *)
  U of E.name | (* undirected *)
  L of B.name * (expr list) | (* bbox (expand left) *)
  R of B.name * (expr list) (* bbox (expand right) *)

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
  I of E.name |
  O of E.name |
  U of E.name |
  L of B.name * (expr list) |
  R of B.name * (expr list)

datatype T = NHD of expr list

val mk = NHD

val empty = NHD []

fun rename (emap, bmap) (NHD exprs) =
let
  fun rn (I e) = I (ESub.get emap e)
    | rn (O e) = O (ESub.get emap e)
    | rn (U e) = U (ESub.get emap e)
    | rn (L (bb, es)) = L (BSub.get bmap bb, map rn es)
    | rn (R (bb, es)) = R (BSub.get bmap bb, map rn es)
in NHD (map rn exprs)
end

fun eq (NHD exprs, NHD exprs') =
let
  fun ex_eq (L (bb,es), L (bb',es')) =
       (B.name_eq (bb, bb') andalso
        ListPair.allEq ex_eq (es,es')
        handle ListPair.UnequalLengths => false)
    | ex_eq (R (bb,es), R (bb',es')) =
       (B.name_eq (bb, bb') andalso
        ListPair.allEq ex_eq (es,es')
        handle ListPair.UnequalLengths => false)
    | ex_eq (I e, I e') = E.name_eq (e,e')
    | ex_eq (O e, O e') = E.name_eq (e,e')
    | ex_eq (U e, U e') = E.name_eq (e,e')
in ListPair.allEq ex_eq (exprs,exprs')
   handle ListPair.UnequalLengths => false
end

(*fun expand bb (emap, bmap) (NHD exprs) =
let
  fun ex (L (bb', exprs') rest =
    if B.name_eq (bb, bb')
    then L (bb, exprs') :: exprs' @ rest
    else L (bb', fold_rev ex rest []) :: rest
*)


fun expr_of_json (Json.Array lst) =
 (case lst
    of (Json.String bb :: Json.String "<" :: rest) =>
         L (B.mk bb, map expr_of_json rest)
     | (Json.String bb :: Json.String ">" :: rest) =>
         R (B.mk bb, map expr_of_json rest)
     | _ => raise nhd_exn "expected: [BB_NAME, DIR, ....]")
  | expr_of_json (Json.String e) =
     (case String.explode e
        of (#"i" :: #":" :: e') => I (E.mk (String.implode e'))
         | (#"o" :: #":" :: e') => O (E.mk (String.implode e'))
         | (#"u" :: #":" :: e') => U (E.mk (String.implode e'))
         | _ => raise nhd_exn "expected: 'i:...', 'o:...', or 'u:...'")
  | expr_of_json _ = raise nhd_exn "expected: array or string"

fun of_json (Json.Object obj) =
  (case Json.lookup obj "expr"
     of SOME (Json.Array lst) => mk (map expr_of_json lst)
      | SOME _ => raise nhd_exn "expected: array"
      | NONE => raise nhd_exn "expected: field 'expr'")
  | of_json _ = raise nhd_exn "expected: object"

fun json_of_expr (L (bb, es)) =
      Json.Array ([Json.String (B.dest bb), Json.String "<"] @ map json_of_expr es)
  | json_of_expr (R (bb, es)) =
      Json.Array ([Json.String (B.dest bb), Json.String ">"] @ map json_of_expr es)
  | json_of_expr (I e) = Json.String ("i:" ^ E.dest e)
  | json_of_expr (O e) = Json.String ("o:" ^ E.dest e)
  | json_of_expr (U e) = Json.String ("u:" ^ E.dest e)

fun json_of (NHD exps) = Json.mk_object [("expr", Json.Array (map json_of_expr exps))]

end
