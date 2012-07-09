structure Test_MatchState =
  OGraphMatchState(
    structure G = Test_MkG.G
    type data = unit
    val empty_data = ()
  )
structure Test_BGMatchState =
  BangGraphMatchState(
    structure G = Test_MkG.G
    type data = unit
    val empty_data = ()
  )
structure Test_InnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_MatchState)
structure Test_BGInnerMatcher =
  SimpleInnerMatchSearch(structure MatchState = Test_BGMatchState)
structure Test_ConcreteMatcher =
  ConcreteMatchSearch(structure InnerMatchSearch = Test_InnerMatcher)
structure Test_GreedyMatcher =
  GreedyMatchSearch(
    structure InnerMatchSearch = Test_BGInnerMatcher
    structure BGMatchState = Test_BGMatchState
  )

structure Test_MatchUtil =
struct
  fun assert_n_match n mseq () = let
    val mlist = Seq.list_of mseq
    val nm = length mlist
  in if nm = n then I
     else
       (List.map Test_MatchState.print mlist;
        raise ERROR ("expected " ^ Int.toString n ^
                     " matching(s), got " ^ Int.toString nm))
  end
end

structure Test_BGMatchUtil =
struct
  structure G = Test_MkG.G

  fun assert_n_match n mseq () = let
    val mlist = Seq.list_of mseq
    val nm = length mlist
  in if nm = n then I
     else
       (List.map Test_BGMatchState.print mlist;
        raise ERROR ("expected " ^ Int.toString n ^
                     " matching(s), got " ^ Int.toString nm))
  end

  fun assert_boundary_coherent (g1,g2) = let
    val _ = if B.NSet.eq (G.get_bboxes g1) (G.get_bboxes g2) then ()
            else raise ERROR "Differing !-boxes!"
    val _ = if V.NSet.eq (G.get_inputs g1) (G.get_inputs g2) then ()
            else raise ERROR "Differing inputs!"
    val _ = if V.NSet.eq (G.get_outputs g1) (G.get_outputs g2) then ()
            else raise ERROR "Differing outputs!"
    val _ = V.NSet.fold (fn v => fn _ =>
              if B.NSet.eq (G.get_bboxes_of g1 v) (G.get_bboxes_of g2 v) then ()
              else raise ERROR ((V.string_of_name v)^" has differing !-boxes")
              ) (G.get_inputs g1) ()
    val _ = V.NSet.fold (fn v => fn _ =>
              if B.NSet.eq (G.get_bboxes_of g1 v) (G.get_bboxes_of g2 v) then ()
              else raise ERROR ((V.string_of_name v)^" has differing !-boxes")
              ) (G.get_outputs g1) ()
  in
    (g1,g2)
  end
end

