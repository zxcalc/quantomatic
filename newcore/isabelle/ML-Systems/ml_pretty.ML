(*  Title:      Pure/ML-Systems/ml_pretty.ML
    Author:     Makarius

Minimal support for raw ML pretty printing -- for boot-strapping only.
*)

structure ML_Pretty =
struct

datatype pretty =
  Block of (string * string) * pretty list * int |
  String of string * int |
  Break of bool * int;

fun block prts = Block (("", ""), prts, 2);
fun str s = String (s, size s);
fun brk wd = Break (false, wd);

fun pair pretty1 pretty2 ((x, y), depth: int) =
  block [str "(", pretty1 (x, depth), str ",", brk 1, pretty2 (y, depth - 1), str ")"];

fun enum sep lpar rpar pretty (args, depth) =
  let
    fun elems _ [] = []
      | elems 0 _ = [str "..."]
      | elems d [x] = [pretty (x, d)]
      | elems d (x :: xs) = pretty (x, d) :: str sep :: brk 1 :: elems (d - 1) xs;
  in block (str lpar :: (elems (Int.max (depth, 0)) args @ [str rpar])) end;

end;

