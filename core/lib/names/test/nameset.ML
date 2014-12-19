local
structure N = StrNSet;
structure N2 = StrIntNSet;
structure N3 = IntNSet;
in 
val _ = Testing.test "NSet tests" (fn () => let 
(* 
 install_pp (make_pp ["N", "T"] (Pretty.pprint o N.pretty_struct));
*)

fun assert_correct m = N.assert_correct m;

(* --- basic tests --- *)
val m = N.empty;
val m = m |> N.add "z" |> assert_correct;
val m = m |> N.add "aa" |> assert_correct;
val m = m |> N.add "ab" |> assert_correct;
val m = m |> N.add "za" |> assert_correct;
val m = m |> N.add "zb" |> assert_correct;
val m = m |> N.add "zc" |> assert_correct;
val m2 = m |> N.delete "zb" |> assert_correct;
val m2 = m |> N.delete "aa" |> assert_correct;
val m2 = m2 |> N.delete "ab" |> assert_correct;
val m2 = m2 |> N.delete "zb" |> assert_correct;
val m2 = m2 |> N.delete "zc" |> assert_correct;
val m2 = m2 |> N.delete "z" |> assert_correct;
val m2 = m2 |> N.delete "za" |> assert_correct;
val m2 = m2 |> N.delete "za" |> assert_correct;
val m = m |> N.add_range ("ac","f") |> assert_correct;
val m = m |> N.add_range ("a","c") |> assert_correct;
val m = m |> N.add_range ("bb","cc") |> assert_correct;
val m = m |> N.add_range ("dd","ee") |> assert_correct;
val m = m |> N.add_range ("ef","zb") |> assert_correct;


(* --- Test from parser code --- *)
(* stuff for initial table *)
fun chars_between startchar endchar = 
    (case Char.compare (startchar, endchar) of
       LESS => startchar :: chars_between (Char.succ startchar) endchar
     | _ => [endchar]);

(* little helper function to print id's nicely *)
fun id_of_str "" = "unit" (* empty string mapped to char 0 *)
  | id_of_str " " = "\" \""
  | id_of_str s = 
    let val s2 = String.toString s
    in if s2 = s then  s else ("\"" ^ s2 ^ "\"") end;

val empty_str_id = id_of_str (Char.toString #"\^@"); (* empty string mapped to char 0 *)

(* all initial characters *)
val base_chars = [#"\t", #"\n"] @ (chars_between (#" ") (#"~"));
val base_char_strs = map Char.toString base_chars;
val base_char_ids = map id_of_str base_char_strs;

(* quoted characters *)
val quote_char = (#"\\");
val quoted_chars = [(#"\\"), (#"'")];
(* note subtract x y = x subtracted_from y = y - x *)
val unquoted_chars = subtract (op =) base_chars quoted_chars;

val basetype_names = (base_char_ids ~~ base_chars);

val big_table = 
    fold 
      (N.assert_correct oo N.add)
      base_char_ids
      N.empty;


(* --- Bugs found and cleared up... lets not make them again... *)


val tr2 = 
    N.empty 
      |> N.add_range ("conjEm","conjEn")
      |> N.add_range ("aw","bk")
      |> N.add "exEh"
      |> N.add_range ("g","j")
      |> N.add "exEf"
      |> N.add_range ("conjEj","conjEk")
      |> N.add_range ("conjEg","conjEh")
      |> assert_correct;
(*val _ = N.print_full tr2;*)

(* causes major rebalance... *)
val tr3 = tr2 |> N.add "conjEl";



(* --- intersection test 1 --- *)
val m = N.empty;
val m = m |> N.add "a" |> assert_correct;
val m = m |> N.add "b" |> assert_correct;

val m2 = N.empty;
val m2 = m2 |> N.add "a" |> assert_correct;
val m2 = m2 |> N.add "c" |> assert_correct;

val _ = N.intersect m m2;
val _ = N.intersect m2 m;


(* --- intersection test 2 --- *)
val m = N2.empty;
val m = m |> N2.add ("C",15) |> N2.assert_correct;
val m = m |> N2.add ("C",13) |> N2.assert_correct;

val m2 = N2.empty;
val m2 = m2 |> N2.add ("C",14) |> N2.assert_correct;
val m2 = m2 |> N2.add ("C",15) |> N2.assert_correct;

val _ = N2.intersect m m2;
val _ = N2.intersect m2 m;


(* --- union_merge test 1 --- *)
val s = N3.empty
      |> N3.add_range (96,101)
      |> N3.add_range (116,118)
      |> N3.add 112
      |> N3.add 110;

val s2 = s |> N3.add_range (111,111) |> N3.assert_correct;

val s2 = N3.empty |> N3.add_range (111,111);

val (v as (b,t)) = (111,111);
open N3;
val _ = case s
          of (tr as Br (d,v2 as (b2,t2),l,r)) => ()
           | _ => raise ERROR "s wrong"

val _ = N3.union_merge s2 s |> N3.assert_correct;;

(* --- renaming --- *)

val s = N3.of_list [1,2,3];
(*val _ = N3.print s;*)
val s = s |> N3.add 5;
(*val _ = N3.print s;*)
val s = the (s |> N3.try_rename1 5 4)
val _ = case s |> N3.try_rename1 4 2
          of SOME _ => raise ERROR "N3.try_rename1 4 2: failed :( "
           | _ => ();
(*val _ = N3.print s;*)


in () end (* let *)
) ()
val _ = Testing.assert_no_failed_tests();
end (* local *)





(* debugging ....


val tr = 
    N.empty 
      |> N.add_range ("conjEm","conjEn")
      |> N.add_range ("aw","bk")
      |> N.add "exEh"
      |> N.add_range ("g","j")
      |> N.add "exEf"
      |> N.add_range ("conjEj","conjEk")
      |> N.add_range ("conjEg","conjEh")
      |> assert_correct;
val _ = N.print_full tr;

open N;

val s = "conjEl";
val (s as (sp,sx,ss)) = (pred_or_same s, s, N.suc s);

val tr3 = tr |> N.add' s;

val tr2 = tr |> N.add "conjEl";



val s= n;
val s = (N.pred_or_same s, s, N.N.suc s);
val (s as (sp,sx,ss)) = s;
val (tr as N.Br (d,v as (b,t),l,r)) = tr;
N.print_full tr;

val (tr as N.Br (d,v as (b,t),l,r)) = r;


N.print_full (N.add' s tr);


N.N.ord (sx,b);
N.N.ord(ss,b);
N.N.ord (sp,t);

N.print_full l;
N.print_full (N.add' s l);
val newl = N.add' s l;
N.print_full newl;

val (tr as N.Br(_,v, l as N.Br(ld,lv,ll,lr), r as N.Br(rd,rv,rl,rr))) =
rebalance(N.Br (N.br_depth newl r,(b,t),newl,r));

N.print_full (N.Br(rd + 1,v,lr,r));

N.add' s l;

N.print_full (N.rebalance (N.add' s r));
N.print_full ((N.add' s r));


N.N.ord (sx," ");
N.N.ord ("$","\" \"");

*)
