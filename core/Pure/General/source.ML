(*  Title:      Pure/General/source.ML
    Author:     Markus Wenzel, TU Muenchen

Coalgebraic data sources -- efficient purely functional input streams.
*)

signature SOURCE =
sig
  type ('a, 'b) source
  val get: ('a, 'b) source -> 'a list * ('a, 'b) source
  val unget: 'a list * ('a, 'b) source -> ('a, 'b) source
  val get_single: ('a, 'b) source -> ('a * ('a, 'b) source) option
  val exhaust: ('a, 'b) source -> 'a list
  val map_filter: ('a -> 'b option) -> ('a, 'c) source -> ('b, ('a, 'c) source) source
  val filter: ('a -> bool) -> ('a, 'b) source -> ('a, ('a, 'b) source) source
  val of_list: 'a list -> ('a, 'a list) source
  val exhausted: ('a, 'b) source -> ('a, 'a list) source
  val of_string: string -> (string, string list) source
  val of_string_limited: int -> string -> (string, substring) source
  val source': 'a -> 'b Scan.stopper -> ('a * 'b list -> 'c list * ('a * 'b list)) ->
    ('b, 'e) source -> ('c, 'a * ('b, 'e) source) source
  val source: 'a Scan.stopper -> ('a list -> 'b list * 'a list) ->
    ('a, 'd) source -> ('b, ('a, 'd) source) source
end;

structure Source: SOURCE =
struct


(** datatype source **)

datatype ('a, 'b) source =
  Source of
   {buffer: 'a list,
    info: 'b,
    drain: 'b -> 'a list * 'b};

fun make_source buffer info drain =
  Source {buffer = buffer, info = info, drain = drain};


(* get / unget *)

fun get (Source {buffer = [], info, drain}) =
      let val (xs, info') = drain info
      in (xs, make_source [] info' drain) end
  | get (Source {buffer, info, drain}) = (buffer, make_source [] info drain);

fun unget (xs, Source {buffer, info, drain}) = make_source (xs @ buffer) info drain;


(* variations on get *)

fun get_single src =
  (case get src of
    ([], _) => NONE
  | (x :: xs, src') => SOME (x, unget (xs, src')));

fun exhaust src =
  (case get src of
    ([], _) => []
  | (xs, src') => xs @ exhaust src');


(* (map)filter *)

fun drain_map_filter f src =
  let
    val (xs, src') = get src;
    val xs' = map_filter f xs;
  in
    if null xs orelse not (null xs') then (xs', src')
    else drain_map_filter f src'
  end;

fun map_filter f src = make_source [] src (drain_map_filter f);
fun filter pred = map_filter (fn x => if pred x then SOME x else NONE);



(** build sources **)

(* list source *)

fun of_list xs = make_source [] xs (fn xs => (xs, []));

fun exhausted src = of_list (exhaust src);


(* string source *)

val of_string = of_list o raw_explode;

fun of_string_limited limit str =
  make_source [] (Substring.full str)
    (fn s =>
      let
        val (s1, s2) = Substring.splitAt (s, Int.min (Substring.size s, limit));
        val cs = map String.str (Substring.explode s1);
      in (cs, s2) end);



(** cascade sources **)

(* state-based *)

fun drain_source' stopper scan (state, src) =
  let
    val drain = Scan.drain get stopper;
    val (xs, s) = get src;
    val ((ys, (state', xs')), src') =
      if null xs then (([], (state, [])), s)
      else drain (Scan.error scan) ((state, xs), s);
  in (ys, (state', unget (xs', src'))) end;

fun source' init_state stopper scan src =
  make_source [] (init_state, src) (drain_source' stopper scan);


(* non state-based *)

fun drain_source stopper scan =
  Scan.unlift (drain_source' stopper (Scan.lift scan));

fun source stopper scan src =
  make_source [] src (drain_source stopper scan);

end;
