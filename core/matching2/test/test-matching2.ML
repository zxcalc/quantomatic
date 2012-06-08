
structure Test_InnerMatcher = SimpleInnerMatchSearch(structure G = Test_MkG.G)
structure Test_Matcher = SimpleOuterMatchSearch(structure InnerMatchSearch = Test_InnerMatcher)

local
  open Test_MkG
  structure M = Test_Matcher
  
  val bare_wire = G.empty
  val (b1,bare_wire) = bare_wire |> G.add_named_vertex (V.mk "b1") bvert
  val (b2,bare_wire) = bare_wire |> G.add_named_vertex (V.mk "b2") bvert
  val (_,bare_wire)  = bare_wire |> G.add_named_edge (E.mk "e1") dir_edge b1 b2
  
  val two_bare_wires = G.empty
  val (b3,two_bare_wires) = two_bare_wires |> G.add_named_vertex (V.mk "b3") bvert
  val (b4,two_bare_wires) = two_bare_wires |> G.add_named_vertex (V.mk "b4") bvert
  val (_,two_bare_wires)  = two_bare_wires |> G.add_named_edge (E.mk "e2") dir_edge b3 b4
  val (b5,two_bare_wires) = two_bare_wires |> G.add_named_vertex (V.mk "b5") bvert
  val (b6,two_bare_wires) = two_bare_wires |> G.add_named_vertex (V.mk "b6") bvert
  val (_,two_bare_wires)  = two_bare_wires |> G.add_named_edge (E.mk "e3") dir_edge b5 b6
  
  val circle = G.empty
  val (b7,circle) = circle |> G.add_named_vertex (V.mk "b7") bvert
  val (_,circle)  = circle |> G.add_named_edge (E.mk "e3") dir_edge b7 b7
  
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
  
  val mseq = M.match two_bare_wires bare_wire
  val _ = Testing.test "M.match - 2 wires --> 1 wire "
  (fn () => if length (Seq.list_of mseq) = 2 then I
            else raise ERROR ("expected 2 matchings, got " ^ Int.toString (length (Seq.list_of mseq)))) ()
  
  val mseq = M.match two_bare_wires two_bare_wires
  val _ = Testing.test "M.match - 2 wires --> 2 wires"
  (fn () => if length (Seq.list_of mseq) = 6 then I
            else raise ERROR ("expected 6 matchings, got " ^ Int.toString (length (Seq.list_of mseq)))) ()
  
  val mseq = M.match bare_wire circle
  val _ = Testing.test "M.match - 1 wire --> circle"
  (fn () => if length (Seq.list_of mseq) = 1 then I
            else raise ERROR ("expected 1 matching, got " ^ Int.toString (length (Seq.list_of mseq)))) ()
  
  (* for circles, there should be as many matches as there are cyclic permutations *) 
  val mseq = M.match two_bare_wires circle
  val _ = Testing.test "M.match - 2 wires --> circle"
  (fn () => if length (Seq.list_of mseq) = 1 then I
            else raise ERROR ("expected 1 matching, got " ^ Int.toString (length (Seq.list_of mseq)))) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR MATCHING2/BARE WIRES PASSED!"
end

(*local*)
  open Test_MkG
  structure M = Test_Matcher
  
  val bare_wire = G.empty
  val (b1,bare_wire) = bare_wire |> G.add_named_vertex (V.mk "b1") bvert
  val (b2,bare_wire) = bare_wire |> G.add_named_vertex (V.mk "b2") bvert
  val (_,bare_wire)  = bare_wire |> G.add_named_edge (E.mk "e1") dir_edge b1 b2
  
  val circle = G.empty
  val (b3,circle) = circle |> G.add_named_vertex (V.mk "b3") bvert
  val (_,circle)  = circle |> G.add_named_edge (E.mk "e2") dir_edge b3 b3
  
  
  val mseq = M.match circle G.empty
  val _ = Testing.test "M.match - circle --> empty"
  (fn () => if length (Seq.list_of mseq) = 0 then I
            else raise ERROR ("expected 0 matching, got " ^ Int.toString (length (Seq.list_of mseq)))) ()
  
  val mseq = M.match circle circle
  val _ = Testing.test "M.match - circle --> circle"
  (fn () => if length (Seq.list_of mseq) = 1 then I
            else raise ERROR ("expected 1 matching, got " ^ Int.toString (length (Seq.list_of mseq)))) ()
(*in*)
  (*val _ = Testing.assert_no_failed_tests();*)
  val _ = "UNIT TESTS FOR MATCHING2/CIRCLES PASSED!"
(*end*)

