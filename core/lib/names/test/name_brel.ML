local 

structure IntStrBinRel = NameBRelFun(structure Dom = IntName and Cod = StrName)
structure BinRel = IntStrBinRel;

in val _ = Testing.test "BinRel tests" (fn () => let

val rel0 = BinRel.empty;
val rel = rel0 |> (BinRel.add_many (IntName.NSet.of_list [1,2,3]) 
                                   (StrName.NSet.of_list ["a","b","c"]));
(*val _ = BinRel.print rel;*)

val rel = rel |> (BinRel.add_to_dom 5 (StrName.NSet.of_list ["a","b"]));
(*val _ = BinRel.print rel;*)

(* val rel = rel |> (BinRel.try_rename1_dom 5 4);
val _ = BinRel.print rel; *)

val rel = rel |> (BinRel.del_many (IntName.NSet.of_list [1,2,4]) 
                                  (StrName.NSet.of_list ["b","c"]));
(*val _ = BinRel.print rel;*)

val rel = rel |> (BinRel.del_many (IntName.NSet.of_list [1]) 
                                  (StrName.NSet.of_list ["a"]));
(*val _ = BinRel.print rel;*)


in () end (* let *)
) ()
val _ = Testing.assert_no_failed_tests();
end
