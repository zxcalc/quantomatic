(*  Title:      Pure/interpretation.ML
    Author:     Florian Haftmann and Makarius

Generic interpretation of theory data.
*)

signature INTERPRETATION =
sig
  type T
  val result: theory -> T list
  val interpretation: (T -> theory -> theory) -> theory -> theory
  val data: T -> theory -> theory
  val init: theory -> theory
end;

functor Interpretation(type T val eq: T * T -> bool): INTERPRETATION =
struct

type T = T;

structure Interp = Theory_Data
(
  type T = T list * (((T -> theory -> theory) * stamp) * T list) list;
  val empty = ([], []);
  val extend = I;
  fun merge ((data1, interps1), (data2, interps2)) : T =
    (Library.merge eq (data1, data2),
     AList.join (eq_snd (op =)) (K (Library.merge eq)) (interps1, interps2));
);

val result = #1 o Interp.get;

fun consolidate thy =
  let
    val (data, interps) = Interp.get thy;
    val unfinished = interps |> map (fn ((f, _), xs) =>
      (f, if eq_list eq (xs, data) then [] else subtract eq xs data));
    val finished = interps |> map (fn (interp, _) => (interp, data));
  in
    if forall (null o #2) unfinished then NONE
    else SOME (thy |> fold_rev (uncurry fold_rev) unfinished |> Interp.put (data, finished))
  end;

fun interpretation f = Interp.map (apsnd (cons ((f, stamp ()), []))) #> perhaps consolidate;
fun data x = Interp.map (apfst (cons x)) #> perhaps consolidate;

val init = Theory.at_begin consolidate;

end;

