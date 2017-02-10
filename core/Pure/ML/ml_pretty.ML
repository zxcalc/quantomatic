(*  Title:      Pure/ML/ml_pretty.ML
    Author:     Makarius

Minimal support for raw ML pretty printing, notably for toplevel pp.
*)

signature ML_PRETTY =
sig
  datatype pretty =
    Block of (string * string) * bool * FixedInt.int * pretty list |
    String of string * FixedInt.int |
    Break of bool * FixedInt.int * FixedInt.int
  val block: pretty list -> pretty
  val str: string -> pretty
  val brk: FixedInt.int -> pretty
  val pair: ('a * FixedInt.int -> pretty) -> ('b * FixedInt.int -> pretty) ->
    ('a * 'b) * FixedInt.int -> pretty
  val enum: string -> string -> string -> ('a * FixedInt.int -> pretty) ->
    'a list * FixedInt.int -> pretty
  val to_polyml: pretty -> PolyML_Pretty.pretty
  val from_polyml: PolyML_Pretty.pretty -> pretty
  val format_polyml: int -> PolyML_Pretty.pretty -> string
  val format: int -> pretty -> string
  val default_margin: int
  val string_of_polyml: PolyML_Pretty.pretty -> string
  val make_string_fn: string
end;

structure ML_Pretty: ML_PRETTY =
struct

(* datatype pretty *)

datatype pretty =
  Block of (string * string) * bool * FixedInt.int * pretty list |
  String of string * FixedInt.int |
  Break of bool * FixedInt.int * FixedInt.int;

fun block prts = Block (("", ""), false, 2, prts);
fun str s = String (s, FixedInt.fromInt (size s));
fun brk width = Break (false, width, 0);

fun pair pretty1 pretty2 ((x, y), depth: FixedInt.int) =
  block [str "(", pretty1 (x, depth), str ",", brk 1, pretty2 (y, depth - 1), str ")"];

fun enum sep lpar rpar pretty (args, depth: FixedInt.int) =
  let
    fun elems _ [] = []
      | elems 0 _ = [str "..."]
      | elems d [x] = [pretty (x, d)]
      | elems d (x :: xs) = pretty (x, d) :: str sep :: brk 1 :: elems (d - 1) xs;
  in block (str lpar :: (elems (FixedInt.max (depth, 0)) args @ [str rpar])) end;


(* convert *)

fun to_polyml (Break (false, width, offset)) = PolyML_Pretty.PrettyBreak (width, offset)
  | to_polyml (Break (true, _, _)) =
      PolyML_Pretty.PrettyBlock (0, false, [PolyML_Pretty.ContextProperty ("fbrk", "")],
        [PolyML_Pretty.PrettyString " "])
  | to_polyml (Block ((bg, en), consistent, ind, prts)) =
      let val context =
        (if bg = "" then [] else [PolyML_Pretty.ContextProperty ("begin", bg)]) @
        (if en = "" then [] else [PolyML_Pretty.ContextProperty ("end", en)])
      in PolyML_Pretty.PrettyBlock (ind, consistent, context, map to_polyml prts) end
  | to_polyml (String (s, len)) =
      if len = FixedInt.fromInt (size s) then PolyML_Pretty.PrettyString s
      else
        PolyML_Pretty.PrettyBlock
          (0, false,
            [PolyML_Pretty.ContextProperty ("length", FixedInt.toString len)], [PolyML_Pretty.PrettyString s]);

val from_polyml =
  let
    fun convert _ (PolyML_Pretty.PrettyBreak (width, offset)) = Break (false, width, offset)
      | convert _ (PolyML_Pretty.PrettyBlock (_, _,
            [PolyML_Pretty.ContextProperty ("fbrk", _)], [PolyML_Pretty.PrettyString " "])) =
          Break (true, 1, 0)
      | convert len (PolyML_Pretty.PrettyBlock (ind, consistent, context, prts)) =
          let
            fun property name default =
              (case List.find (fn PolyML_Pretty.ContextProperty (a, _) => name = a | _ => false) context of
                SOME (PolyML_Pretty.ContextProperty (_, b)) => b
              | _ => default);
            val bg = property "begin" "";
            val en = property "end" "";
            val len' = property "length" len;
          in Block ((bg, en), consistent, ind, map (convert len') prts) end
      | convert len (PolyML_Pretty.PrettyString s) =
          String (s, FixedInt.fromInt (case Int.fromString len of SOME i => i | NONE => size s))
  in convert "" end;


(* format *)

fun format_polyml margin prt =
  let
    val result = Unsynchronized.ref [];
    val () = PolyML.prettyPrint (fn s => result := s :: ! result, margin) prt
  in String.concat (List.rev (! result)) end;

fun format margin = format_polyml margin o to_polyml;

val default_margin = 76;


(* make string *)

val string_of_polyml = format_polyml default_margin;

val make_string_fn =
  "(fn x => ML_Pretty.string_of_polyml (ML_system_pretty \
    \(x, FixedInt.fromInt (ML_Print_Depth.get_print_depth ()))))";

end;
