signature NHD =
sig

(* an edge expression *)
datatype expr =
  I of E.name | (* input *)
  O of E.name | (* output *)
  U of E.name | (* undirected *)
  L of B.name * (expr list) | (* bbox (expand left) *)
  R of B.name * (expr list) (* bbox (expand right) *)

datatype dir = IN | OUT | UNDIR
type ectx = E.name * dir * B.name list

type T

val mk : expr list -> T
val empty : T

val of_json : Json.json -> T
val json_of : T -> Json.json

val rename : (ESub.T * BSub.T) -> T -> T
val eq : T * T -> bool

val reduce : T -> T
val drop : B.name -> T -> T
val kill : B.name -> T -> T
val expand : B.name -> (ESub.T * BSub.T) -> T -> T
val copy : B.name -> (ESub.T * BSub.T) -> T -> T

(* get a list of all edge contexts *)
val get_ctxs : T -> ectx list

(* get edge contexts referring to a particular name. There should be
 * one for normal edges and two for self-loops. *)
val get_ctxs_for : E.name -> T -> ectx list

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

datatype dir = IN | OUT | UNDIR
type ectx = E.name * dir * B.name list

datatype T = NHD of expr list

val mk = NHD

val empty = NHD []

fun rename_expr (emap, bmap) (I e) = I (ESub.get emap e)
  | rename_expr (emap, bmap) (O e) = O (ESub.get emap e)
  | rename_expr (emap, bmap) (U e) = U (ESub.get emap e)
  | rename_expr (emap, bmap) (L (bb, es)) =
      L (BSub.get bmap bb, map (rename_expr (emap, bmap)) es)
  | rename_expr (emap, bmap) (R (bb, es)) =
      R (BSub.get bmap bb, map (rename_expr (emap, bmap)) es)

fun rename (emap, bmap) (NHD exprs) = mk (map (rename_expr (emap, bmap)) exprs)

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
    | ex_eq _ = false
in ListPair.allEq ex_eq (exprs,exprs')
   handle ListPair.UnequalLengths => false
end

(* remove empty bboxes *)
fun reduce (NHD exprs) =
let
  fun reduce_e (L (bb, es)) =
       (case maps reduce_e es of [] => [] | es' => [L (bb, es')])
    | reduce_e (R (bb, es)) =
       (case maps reduce_e es of [] => [] | es' => [R (bb, es')])
    | reduce_e e = [e]
in mk (maps reduce_e exprs)
end

fun kill bb (NHD exprs) =
let
  fun kill_e (L (bb', es)) =
        if B.name_eq (bb, bb')
        then NONE
        else SOME (L (bb', map_filter kill_e es))
    | kill_e (R (bb', es)) =
        if B.name_eq (bb, bb')
        then NONE
        else SOME (R (bb', map_filter kill_e es))
    | kill_e e = SOME e
in reduce (mk (map_filter kill_e exprs))
end

fun drop bb (NHD exprs) =
let
  fun drop_e (L (bb', es)) =
        if B.name_eq (bb, bb')
        then maps drop_e es
        else [L (bb', maps drop_e es)]
    | drop_e (R (bb', es)) =
        if B.name_eq (bb, bb')
        then maps drop_e es
        else [R (bb', maps drop_e es)]
    | drop_e e = [e]
in mk (maps drop_e exprs)
end

fun expand bb (emap, bmap) (NHD exprs) =
let
  fun expand_e (L (bb', exprs')) =
        if B.name_eq (bb, bb')
        then (map (rename_expr (emap, bmap)) exprs') @ [L (bb, exprs')]
        else [L (bb', maps expand_e exprs')]
    | expand_e (R (bb', exprs')) =
        if B.name_eq (bb, bb')
        then R (bb, exprs') :: (map (rename_expr (emap, bmap)) exprs')
        else [R (bb', maps expand_e exprs')]
    | expand_e expr = [expr]
in mk (maps expand_e exprs)
end

fun copy bb (emap, bmap) (NHD exprs) =
let
  fun copy_e (expr as L (bb', exprs')) =
        if B.name_eq (bb, bb')
        then [rename_expr (emap, bmap) expr, expr]
        else [L (bb', maps copy_e exprs')]
    | copy_e (expr as R (bb', exprs')) =
        if B.name_eq (bb, bb')
        then [expr, rename_expr (emap, bmap) expr]
        else [R (bb', maps copy_e exprs')]
    | copy_e expr = [expr]
in mk (maps copy_e exprs)
end


fun get_ctxs (NHD exprs) =
let
  fun ctxs (L (bb', exprs')) = map (fn (e,d,bbs) => (e,d,bb' :: bbs)) (maps ctxs exprs')
    | ctxs (R (bb', exprs')) = map (fn (e,d,bbs) => (e,d,bb' :: bbs)) (maps ctxs exprs')
    | ctxs (I e) = [(e, IN, [])]
    | ctxs (O e) = [(e, OUT, [])]
    | ctxs (U e) = [(e, UNDIR, [])]
in maps ctxs exprs
end

fun get_ctxs_for e nhd = filter (fn (e',_,_) => E.name_eq (e,e')) (get_ctxs nhd)

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
