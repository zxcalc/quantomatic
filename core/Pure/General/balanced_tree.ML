(*  Title:      Pure/General/balanced_tree.ML
    Author:     Lawrence C Paulson and Makarius

Balanced binary trees.
*)

signature BALANCED_TREE =
sig
  val make: ('a * 'a -> 'a) -> 'a list -> 'a
  val dest: ('a -> 'a * 'a) -> int -> 'a -> 'a list
  val access: {left: 'a -> 'a, right: 'a -> 'a, init: 'a} -> int -> int -> 'a
  val accesses: {left: 'a -> 'a, right: 'a -> 'a, init: 'a} -> int -> 'a list
end;

structure Balanced_Tree: BALANCED_TREE =
struct

fun make _ [] = raise List.Empty
  | make _ [x] = x
  | make f xs =
      let
        val m = length xs div 2;
        val (ps, qs) = chop m xs;
      in f (make f ps, make f qs) end;

fun dest f n x =
  if n <= 0 then raise List.Empty
  else if n = 1 then [x]
  else
    let
      val m = n div 2;
      val (left, right) = f x;
    in dest f m left @ dest f (n - m) right end;

(*construct something of the form f(...g(...(x)...)) for balanced access*)
fun access {left = f, right = g, init = x} len i =
  let
    fun acc 1 _ = x
      | acc n i =
          let val m = n div 2 in
            if i <= m then f (acc m i)
            else g (acc (n - m) (i - m))
          end;
  in if 1 <= i andalso i <= len then acc len i else raise Subscript end;

(*construct ALL such accesses; could try harder to share recursive calls!*)
fun accesses {left = f, right = g, init = x} len =
  let
    fun acc 1 = [x]
      | acc n =
          let
            val m = n div 2;
            val accs_left = acc m;
            val accs_right =
              if n - m = m then accs_left
              else acc (n - m);
          in map f accs_left @ map g accs_right end;
  in if 1 <= len then acc len else raise Subscript end;

end;
