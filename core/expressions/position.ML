(*
 * 
 * Totally ordered position data, encoded as binary tree addresses (or, equivalently,
 * variable-length bitstrings). We define a total order <=, a successor relation <<,
 * a cyclic successor relation <o, and a splitting operation.
 *
 * An element of the form 00..0 is called initial, and 11..1 terminal. Cyclic successor
 * is defined to be the same as successor, with an additional relation "t <o i" for any
 * terminal element t and initial element i.
 *
 * Any prefix-free set X of positions has the following properties:
 *
 *  1. it is totally ordered
 *  2. initial and terminal elements are unique, when they exist
 *  3. (cyclic) successors and predecessors are unique, when they exist
 *  4. suppose split(y) = (y', y''), then (X - {y}) U {y',y''} is prefix-free and
 *     for all x, z:
 *       - x << y << z  ==>  x << y' << y'' << z
 *       - x <o y <o z  ==>  x <o y' <o y'' <o z
 *
 *)

signature POSITION =
sig
  type pos
  exception bad_format_exp of string
  val succ : pos * pos -> bool
  val succ_cyclic : pos * pos -> bool
  val ord : pos * pos -> order
  val eq : pos * pos -> bool
  val of_bitstring : string -> pos
  val bitstring_of : pos -> string
end

structure Position :> POSITION =
struct

exception bad_format_exp of string
datatype pos = L of pos | R of pos | P

fun all_Ls P     = true
  | all_Ls (L x) = all_Ls x
  | all_Ls _     = false
fun all_Rs P     = true
  | all_Rs (R x) = all_Rs x
  | all_Rs _     = false

fun succ (L x, R y) = (all_Rs x) andalso (all_Ls y)
  | succ (L x, L y) = succ (x,y)
  | succ (R x, R y) = succ (x,y)
  | succ _ = false

fun succ_cyclic (x, y) = ((all_Ls x) andalso (all_Rs y)) orelse (succ (x,y))

(* lexicographic, and s < t if s is a prefix of t *)
fun ord (P, P) = EQUAL
  | ord (P, _) = LESS
  | ord (_, P) = GREATER
  | ord (L x, L y) = ord (x,y)
  | ord (R x, R y) = ord (x,y)
  | ord (L _, R _) = LESS
  | ord (R _, L _) = GREATER

fun eq (x,y) = (ord (x,y) = EQUAL)

fun split P = (L P, R P)
  | split (L x) = let val (y,z) = split x in (L y, L z) end
  | split (R x) = let val (y,z) = split x in (R y, R z) end

fun of_bitstring' [] = P
  | of_bitstring' (#"0" :: s) = L (of_bitstring' s)
  | of_bitstring' (#"1" :: s) = R (of_bitstring' s)
  | of_bitstring' _ = raise bad_format_exp "Expected: bitstring"

fun of_bitstring s = of_bitstring' (String.explode s)

fun bitstring_of P = ""
  | bitstring_of (L x) = "0" ^ bitstring_of x
  | bitstring_of (R x) = "1" ^ bitstring_of x

val _ = PolyML.addPrettyPrinter
  (fn depth => fn _ => fn p =>
    PolyML.PrettyString(
      if depth <= 0 then "..."
      else bitstring_of p
  ))

end