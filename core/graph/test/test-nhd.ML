structure Nhd_Tests =
struct

val [Ba,Bb,Bc,Bd] = map B.mk ["Ba","Bb","Bc","Bd"]
val [a,b,c,d,e,f,g,h] = map E.mk ["a","b","c","d","e","f", "g", "h"]


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

val _ = Testing.test "Nhd.reduce" (fn () =>
  let
    val n1 = Nhd.mk [Nhd.L (Ba, [Nhd.R (Bb, []), Nhd.O d])]
    val n2 = Nhd.mk [Nhd.L (Ba, [Nhd.R (Bb, [])])]
    val n1red = Nhd.mk [Nhd.L (Ba, [Nhd.O d])]
    val n2red = Nhd.empty
    val _ = Testing.assert "reduce n1" (Nhd.eq (Nhd.reduce n1, n1red))
    val _ = Testing.assert "reduce n2" (Nhd.eq (Nhd.reduce n2, n2red))
  in ()
  end) ()

val nhd = Nhd.mk [Nhd.L (Ba, [Nhd.I a, Nhd.O b, Nhd.R (Bb, [Nhd.I c])]), Nhd.O d]

val fr_e = fold ESub.add [(a,e),(b,f),(c,g),(d,h)] ESub.empty
val fr_bb = fold BSub.add [(Ba,Bc),(Bb,Bd)] BSub.empty


val _ = Testing.test "Nhd.get_edge_contexts" (fn () =>
  let
    val ctxs = [(a, Nhd.IN, [Ba]), (b, Nhd.OUT, [Ba]), (c, Nhd.IN, [Ba, Bb]), (d, Nhd.OUT, [])]
    val _ = Testing.assert "gets correct contexts" (ListPair.allEq
             (fn ((e,d,bs), (e',d',bs')) => (
                E.name_eq (e, e') andalso d = d' andalso ListPair.allEq B.name_eq (bs,bs')
             ))
             (Nhd.get_edge_contexts nhd, ctxs))
  in ()
  end) ()

val _ = Testing.test "Nhd.get_edge_contexts_for" (fn () =>
  let
    val ctxs = [(a, Nhd.IN, [Ba])]
    val _ = Testing.assert "gets correct contexts" (ListPair.allEq
             (fn ((e,d,bs), (e',d',bs')) => (
                E.name_eq (e, e') andalso d = d' andalso ListPair.allEq B.name_eq (bs,bs')
             ))
             (Nhd.get_edge_contexts_for a nhd, ctxs))
  in ()
  end) ()

val _ = Testing.test "Nhd.kill" (fn () =>
  let
    val k_Ba = Nhd.mk [Nhd.O d]
    val k_Bb = Nhd.mk [Nhd.L (Ba, [Nhd.I a, Nhd.O b]), Nhd.O d]
    val nhd1 = Nhd.mk [Nhd.L (Ba, [Nhd.R (Bb, [Nhd.I c])]), Nhd.O d]
    val k_Bb1 = Nhd.mk [Nhd.O d]
    val _ = Testing.assert "kill outer bbox" (Nhd.eq (Nhd.kill Ba nhd, k_Ba))
    val _ = Testing.assert "kill inner bbox" (Nhd.eq (Nhd.kill Bb nhd, k_Bb))
    val _ = Testing.assert "kill and reduce empty bbox" (Nhd.eq (Nhd.kill Bb nhd1, k_Bb1))
  in ()
  end) ()

val _ = Testing.test "Nhd.drop" (fn () =>
  let
    val d_Ba = Nhd.mk [Nhd.I a, Nhd.O b, Nhd.R (Bb, [Nhd.I c]), Nhd.O d]
    val d_Bb = Nhd.mk [Nhd.L (Ba, [Nhd.I a, Nhd.O b, Nhd.I c]), Nhd.O d]
    val _ = Testing.assert "drop outer bbox" (Nhd.eq (Nhd.drop Ba nhd, d_Ba))
    val _ = Testing.assert "drop inner bbox" (Nhd.eq (Nhd.drop Bb nhd, d_Bb))
  in ()
  end) ()

val _ = Testing.test "Nhd.copy" (fn () =>
  let
    val cp_Ba = Nhd.mk [
                  Nhd.L (Bc, [Nhd.I e, Nhd.O f, Nhd.R (Bd, [Nhd.I g])]),
                  Nhd.L (Ba, [Nhd.I a, Nhd.O b, Nhd.R (Bb, [Nhd.I c])]),
                  Nhd.O d]
    val cp_Bb = Nhd.mk [
                  Nhd.L (Ba, [Nhd.I a, Nhd.O b,
                    Nhd.R (Bb, [Nhd.I c]),
                    Nhd.R (Bd, [Nhd.I g])
                  ]),
                  Nhd.O d]
    val _ = Testing.assert "copy outer bbox" (Nhd.eq (Nhd.copy Ba (fr_e, fr_bb) nhd, cp_Ba))
    val _ = Testing.assert "copy inner bbox" (Nhd.eq (Nhd.copy Bb (fr_e, fr_bb) nhd, cp_Bb))
  in ()
  end) ()

val _ = Testing.test "Nhd.expand" (fn () =>
  let
    val _ = Testing.assert "expand outer bbox"
              (Nhd.eq (nhd |> Nhd.expand Ba (fr_e, fr_bb),
                       nhd |> Nhd.copy Ba (fr_e, fr_bb) |> Nhd.drop Bc))
    val _ = Testing.assert "expand inner bbox"
              (Nhd.eq (nhd |> Nhd.expand Bb (fr_e, fr_bb),
                       nhd |> Nhd.copy Bb (fr_e, fr_bb) |> Nhd.drop Bd))
  in ()
  end) ()

val _ = Testing.assert_no_failed_tests()


end;

val _ = OS.Process.exit OS.Process.success;
