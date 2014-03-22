structure Nhd_Tests =
struct

val [Ba,Bb,Bc] = map B.mk ["Ba","Bb","Bc"]
val [a,b,c,d,e,f] = map E.mk ["a","b","c","d","e","f"]

val _ = Testing.test "Nhd.mk" (fn () => (
    Nhd.mk [Nhd.L (Ba, [Nhd.I a, Nhd.I b]), Nhd.R (Bb, [Nhd.O c]), Nhd.U d, Nhd.U d]
  )) ()

val _ = Testing.test "Nhd.eq" (fn () =>
  let
    val n1 = Nhd.mk [Nhd.L (Ba, [Nhd.I a, Nhd.R (Bb, [Nhd.I b, Nhd.O c]), Nhd.O d])]
    val n2 = Nhd.mk [Nhd.L (Ba, [Nhd.I a, Nhd.R (Bb, [Nhd.I b, Nhd.O c, Nhd.O e]), Nhd.O d])]
    val _ = Testing.assert "succeeds for equal nhds" (Nhd.eq (n1, n1))
    val _ = Testing.assert "fails for non-equal nhds" (not (Nhd.eq (n1, n2)))
  in ()
  end) ()

val _ = Testing.test "Nhd.of_json/json_of" (fn () =>
  let
    val json = Json.of_string "{ \"expr\": [[\"Ba\", \"<\", \"i:a\", [\"Bb\", \">\", \"i:b\", \"o:c\"], \"o:d\"]] }"
    val nhd = Nhd.mk [Nhd.L (Ba, [Nhd.I a, Nhd.R (Bb, [Nhd.I b, Nhd.O c]), Nhd.O d])]
    val _ = Testing.assert "loads json correctly" (Nhd.eq (Nhd.of_json json, nhd))
    val _ = Testing.assert "saves json correctly"
              (Json.string_of (Nhd.json_of nhd) = Json.string_of json)
  in ()
  end) ()


val _ = Testing.test "Nhd.rename" (fn () =>
  let
    val n1 = Nhd.mk [Nhd.L (Ba, [Nhd.I a, Nhd.R (Bb, [Nhd.I b, Nhd.O c]), Nhd.O d])]
    val n2 = Nhd.mk [Nhd.L (Bb, [Nhd.I d, Nhd.R (Ba, [Nhd.I b, Nhd.O c]), Nhd.O e])]
    val esub = ESub.empty |> ESub.add (a, d) |> ESub.add (d, e)
                          |> ESub.extend_fixed (E.NSet.of_list [b, c])
    val bsub = BSub.empty |> BSub.add (Ba, Bb) |> BSub.add (Bb, Ba)
    val _ = Testing.assert "renaming works correctly"
              (Nhd.eq (Nhd.rename (esub,bsub) n1, n2))
  in ()
  end) ()

val _ = Testing.assert_no_failed_tests()


end;

(*val _ = OS.Process.exit OS.Process.success;*)
