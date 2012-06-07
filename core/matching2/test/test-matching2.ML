
structure Test_InnerMatcher = SimpleInnerMatchSearch(structure G = Test_MkG.G)
structure Test_Matcher = SimpleOuterMatchSearch(structure InnerMatchSearch = Test_InnerMatcher)

(*local*)
  open Test_MkG
  structure M = Test_Matcher
  
  val bare_wire = G.empty
  val (b1,bare_wire) = bare_wire |> G.add_named_vertex (V.mk "b1") bvert
  val (b2,bare_wire) = bare_wire |> G.add_named_vertex (V.mk "b2") bvert
  val (_,bare_wire)  = bare_wire |> G.add_edge dir_edge b1 b2
  
  val two_bare_wires = bare_wire
  val (b3,two_bare_wires) = two_bare_wires |> G.add_named_vertex (V.mk "b3") bvert
  val (b4,two_bare_wires) = two_bare_wires |> G.add_named_vertex (V.mk "b4") bvert
  val (_,two_bare_wires)  = two_bare_wires |> G.add_edge dir_edge b3 b4
  
  val _ = Testing.test "M.match - empty target"
  (fn () => case Seq.pull (M.match bare_wire G.empty)
              of NONE => I | _ => raise ERROR "match with empty target should return 0 results") ()
  
  val mseq = M.match G.empty bare_wire
  val _ = Testing.test "M.match - empty source"
  (fn () => if length (Seq.list_of mseq) = 1 then I
            else raise ERROR "match with empty pattern should return 1 result") ()
  
  val mseq = M.match bare_wire two_bare_wires
  val _ = Testing.test "M.match - 1 wire --> 2 wires "
  (fn () => if length (Seq.list_of mseq) = 2 then I
            else raise ERROR ("expected 2 matchings, got " ^ Int.toString (length (Seq.list_of mseq)))) ()
(*in*)
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR MATCHING2/BARE WIRES PASSED!"
(*end*)