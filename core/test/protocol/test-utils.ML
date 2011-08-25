
signature TEST_UTILS =
sig
  val unordered_eq : ''a list -> ''a list -> bool;
end

structure TestUtils : TEST_UTILS =
struct
  fun find_and_remove x [] = NONE
    | find_and_remove x (y::ys) =
        if x = y then SOME ys else
        case (find_and_remove x ys)
          of NONE => NONE
           | SOME zs => SOME (y::zs);

  fun unordered_eq' [] [] = true
    | unordered_eq' (x::xs) ys =
        case (find_and_remove x ys)
          of NONE => false
           | SOME zs => unordered_eq' xs zs;

  fun unordered_eq xs ys =
        (List.length xs) = (List.length ys) andalso (unordered_eq' xs ys)
end
open TestUtils;

(* vi:et:sw=2:sts=2
*)
