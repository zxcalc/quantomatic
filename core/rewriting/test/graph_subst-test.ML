structure Test_OGraphSubst : GRAPH_SUBST = OGraphSubst(Test_MatchState);
structure Test_BangGraphSubst : GRAPH_SUBST = BangGraphSubst(Test_BGMatchState);
(* set to 4 for really detailed messages *)
(* Test_OGraphSubst.Log.level_ref := 2; *)
(* Test_BangGraphSubst.Log.level_ref := 2; *)

local
  (* TODO: separate tests for the OGraph version?      *)
  (*       kind of depends on OGraph file parsing code *)
  structure Tools = Test_Bang_Match_Tools(Test_GreedyMatcher);
  open Tools;
  structure GSub = Test_BangGraphSubst;

  type test_data = {
    name     : string,
    lhs      : string,
    rhs      : string,
    target   : string,
    expected : string
  };

  fun test_f ({ name = nm,
                lhs = lhs_f,
                rhs = rhs_f,
                target = tgt_f,
                expected = exp_f }:test_data) =
  let
    fun test () =
    let
      val lhs = load_graph lhs_f
      val rhs = load_graph rhs_f
      val tgt = load_graph tgt_f
      val exp = load_graph exp_f
      (* sanity-check the rule *)
      val _ = Test_Rule.mk (lhs,rhs)
      val rhs_vnames = G.get_vnames rhs
      val mseq = M.match_avoiding_tracking (rhs_vnames,rhs_vnames) lhs tgt
      val ms = case Seq.pull mseq
                 of SOME (ms,_) => ms
                  | NONE => raise ERROR "No matches!"
      val result = GSub.do_rewrite ms rhs
      val result_min = G.minimise result
      val exp_min = G.minimise exp
    in
      if GIso.test exp_min result_min
      then result
      else (
        save_dot_graph result_min (tgt_f^".result.dot");
        save_dot_graph exp_min (exp_f^".dot");
        writeln "Result (minimised):";
        writeln ("  (written to "^tgt_f^".result.dot)");
        G.print result_min;
        writeln "Expected (minimised):";
        writeln ("  (written to "^exp_f^".dot)");
        G.print exp_min;
        raise ERROR "Got wrong rewrite result"
      )
    end
  in
    Testing.test nm test ()
  end;

  fun run_tests [] = Testing.assert_no_failed_tests()
    | run_tests (t::ts) = (test_f t; run_tests ts);

in
  (* Add tests here; paths are relative to this file *)
  val _ = run_tests [
    { name = "spider rewrite",
      lhs = "graphs/spider-lhs.graph",
      rhs = "graphs/spider-rhs.graph",
      target = "graphs/spider-target.graph",
      expected = "graphs/spider-expected.graph"
    },
    { name = "spider rewrite with angles",
      lhs = "graphs/spider-subst-lhs.graph",
      rhs = "graphs/spider-subst-rhs.graph",
      target = "graphs/spider-subst-target.graph",
      expected = "graphs/spider-subst-expected.graph"
    },
    { name = "spider rewrite with !-boxes",
      lhs = "graphs/spider-bbox-lhs.graph",
      rhs = "graphs/spider-bbox-rhs.graph",
      target = "graphs/spider-bbox-target.graph",
      expected = "graphs/spider-bbox-expected.graph"
    }
    (* TODO: more tests:
     *  - clashing vertex and edge names
     *  - clashing expression variable names (in pattern and target)
     *  - !-boxes with non-boundary vertices
     *  - nested/overlapping !-boxes
     *)
  ];
end;

