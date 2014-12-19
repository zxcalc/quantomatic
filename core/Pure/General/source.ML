(*  Title:      Pure/General/source.ML
    Author:     Markus Wenzel, TU Muenchen

Coalgebraic data sources -- efficient purely functional input streams.
*)

signature SOURCE =
sig
  type ('a, 'b) source
  val default_prompt: string
  val set_prompt: string -> ('a, 'b) source -> ('a, 'b) source
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
  val tty: TextIO.instream -> (string, unit) source
  val source': 'a -> 'b Scan.stopper -> ('a * 'b list -> 'c list * ('a * 'b list)) ->
    (bool * (string -> 'a * 'b list -> 'c list * ('a * 'b list))) option ->
    ('b, 'e) source -> ('c, 'a * ('b, 'e) source) source
  val source: 'a Scan.stopper -> ('a list -> 'b list * 'a list) ->
    (bool * (string -> 'a list -> 'b list * 'a list)) option ->
    ('a, 'd) source -> ('b, ('a, 'd) source) source
end;

structure Source: SOURCE =
struct


(** datatype source **)

datatype ('a, 'b) source =
  Source of
   {buffer: 'a list,
    info: 'b,
    prompt: string,
    drain: string -> 'b -> 'a list * 'b};

fun make_source buffer info prompt drain =
  Source {buffer = buffer, info = info, prompt = prompt, drain = drain};


(* prompt *)

val default_prompt = "> ";

fun set_prompt prompt (Source {buffer, info, prompt = _, drain}) =
  make_source buffer info prompt drain;


(* get / unget *)

fun get (Source {buffer = [], info, prompt, drain}) =
      let val (xs, info') = drain prompt info
      in (xs, make_source [] info' prompt drain) end
  | get (Source {buffer, info, prompt, drain}) =
      (buffer, make_source [] info prompt drain);

fun unget (xs, Source {buffer, info, prompt, drain}) =
  make_source (xs @ buffer) info prompt drain;


(* variations on get *)

fun get_prompt prompt src = get (set_prompt prompt src);

fun get_single src =
  (case get src of
    ([], _) => NONE
  | (x :: xs, src') => SOME (x, unget (xs, src')));

fun exhaust src =
  (case get src of
    ([], _) => []
  | (xs, src') => xs @ exhaust src');


(* (map)filter *)

fun drain_map_filter f prompt src =
  let
    val (xs, src') = get_prompt prompt src;
    val xs' = map_filter f xs;
  in
    if null xs orelse not (null xs') then (xs', src')
    else drain_map_filter f prompt src'
  end;

fun map_filter f src = make_source [] src default_prompt (drain_map_filter f);
fun filter pred = map_filter (fn x => if pred x then SOME x else NONE);



(** build sources **)

(* list source *)

fun of_list xs = make_source [] xs default_prompt (fn _ => fn xs => (xs, []));

fun exhausted src = of_list (exhaust src);


(* string source *)

val of_string = of_list o raw_explode;

fun of_string_limited limit str =
  make_source [] (Substring.full str) default_prompt
    (fn _ => fn s =>
      let
        val (s1, s2) = Substring.splitAt (s, Int.min (Substring.size s, limit));
        val cs = map String.str (Substring.explode s1);
      in (cs, s2) end);


(* stream source *)

fun slurp_input instream =
  let
    fun slurp () =
      (case TextIO.canInput (instream, 1) handle IO.Io _ => NONE of
        NONE => []
      | SOME 0 => []
      | SOME _ => TextIO.input instream :: slurp ());
  in maps raw_explode (slurp ()) end;

fun tty in_stream = make_source [] () default_prompt (fn prompt => fn () =>
  let val input = slurp_input in_stream in
    if exists (fn c => c = "\n") input then (input, ())
    else
      (case (Output.prompt prompt; TextIO.inputLine in_stream) of
        SOME line => (input @ raw_explode line, ())
      | NONE => (input, ()))
  end);



(** cascade sources **)

(* state-based *)

fun drain_source' stopper scan opt_recover prompt (state, src) =
  let
    val drain = Scan.drain prompt get_prompt stopper;
    val (xs, s) = get_prompt prompt src;
    val inp = ((state, xs), s);
    val ((ys, (state', xs')), src') =
      if null xs then (([], (state, [])), s)
      else
        (case opt_recover of
          NONE => drain (Scan.error scan) inp
        | SOME (interactive, recover) =>
            (drain (Scan.catch scan) inp handle Fail msg =>
              (if interactive then Output.error_message msg else ();
                drain (Scan.unless (Scan.lift (Scan.one (Scan.is_stopper stopper))) (recover msg))
                  inp)));
  in (ys, (state', unget (xs', src'))) end;

fun source' init_state stopper scan recover src =
  make_source [] (init_state, src) default_prompt (drain_source' stopper scan recover);


(* non state-based *)

fun drain_source stopper scan opt_recover prompt =
  Scan.unlift (drain_source' stopper (Scan.lift scan)
    (Option.map (fn (int, r) => (int, Scan.lift o r)) opt_recover) prompt);

fun source stopper scan recover src =
  make_source [] src default_prompt (drain_source stopper scan recover);


end;
