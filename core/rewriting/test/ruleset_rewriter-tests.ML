structure Test_RulesetRewriter : RULESET_REWRITER = RulesetRewriter(
  structure Ruleset = Test_Ruleset;
  structure MatchSearch = Test_GreedyMatcher;
  structure GraphSubst = Test_BangGraphSubst;
);
(* set to 4 for really detailed messages *)
(* Test_RulesetRewriter.Log.level_ref := 2; *)

local (* low-level rewriting functions *)
  structure Tools = Test_Bang_Graph_Tools(Test_Bang_Graph);
  structure G = Tools.G;
  structure GIso = Tools.GIso;

  type test_data = {
    name     : string,
    subgraph : (string list) option,
    lhs      : string,
    rhs      : string,
    target   : string,
    expected : string
  };
  exception graph_load_exn of (string*exn);

  fun test_f ({ name = nm,
                subgraph = subg,
                lhs = lhs_f,
                rhs = rhs_f,
                target = tgt_f,
                expected = exp_f }:test_data) =
  let
    fun test () =
    let
      val lhs = Tools.load_graph lhs_f
        handle e => raise graph_load_exn (lhs_f,e);
      val rhs = Tools.load_graph rhs_f
        handle e => raise graph_load_exn (rhs_f,e);
      val tgt = Tools.load_graph tgt_f
        handle e => raise graph_load_exn (tgt_f,e);
      val exp = Tools.load_graph exp_f
        handle e => raise graph_load_exn (exp_f,e);
      val rule = Test_Rule.mk (lhs,rhs)
      val rw_seq =
        case subg
          of SOME vs => Test_RulesetRewriter.rule_rewrites_within
                          (V.NSet.of_list (map V.mk vs))
                          rule
                          tgt
           | NONE    => Test_RulesetRewriter.rule_rewrites
                          rule
                          tgt
      val ((rule',result),rw_seq') =
        case Seq.pull rw_seq
          of SOME x => x
           | NONE => raise ERROR "No matches!"
      (* make sure the remaining rewrites actually succeed *)
      val _ = Seq.list_of rw_seq'
    in
      if GIso.test exp result
      then result
      else (
        Tools.save_dot_graph result (tgt_f^".result.dot");
        Tools.save_dot_graph exp (exp_f^".dot");
        writeln "Result (minimised):";
        writeln ("  (written to "^tgt_f^".result.dot)");
        G.print result;
        writeln "Expected (minimised):";
        writeln ("  (written to "^exp_f^".dot)");
        G.print exp;
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
      subgraph = NONE,
      lhs = "graphs/spider-lhs.graph",
      rhs = "graphs/spider-rhs.graph",
      target = "graphs/spider-target.graph",
      expected = "graphs/spider-expected.graph"
    },
    { name = "spider rewrite with angles",
      subgraph = NONE,
      lhs = "graphs/spider-subst-lhs.graph",
      rhs = "graphs/spider-subst-rhs.graph",
      target = "graphs/spider-subst-target.graph",
      expected = "graphs/spider-subst-expected.graph"
    },
    { name = "spider rewrite with !-boxes",
      subgraph = NONE,
      lhs = "graphs/spider-bbox-lhs.graph",
      rhs = "graphs/spider-bbox-rhs.graph",
      target = "graphs/spider-bbox-target.graph",
      expected = "graphs/spider-bbox-expected.graph"
    },
    { name = "spider rewrite with odd names",
      subgraph = NONE,
      lhs = "graphs/red-spider-1-lhs.graph",
      rhs = "graphs/red-spider-1-rhs.graph",
      target = "graphs/red-spider-1-tgt.graph",
      expected = "graphs/red-spider-1-expected.graph"
    },
    { name = "spider rewrite with odd names (subgraph)",
      subgraph = SOME ["Vm","Vl","Vc","Vr","Vy"],
      lhs = "graphs/red-spider-1-lhs.graph",
      rhs = "graphs/red-spider-1-rhs.graph",
      target = "graphs/red-spider-1-tgt.graph",
      expected = "graphs/red-spider-1-expected.graph"
    },
    { name = "bare wire rewrite",
      subgraph = NONE,
      lhs = "graphs/bare-wire-rule-1-lhs.graph",
      rhs = "graphs/bare-wire-rule-1-rhs.graph",
      target = "graphs/bare-wire-rule-1-target.graph",
      expected = "graphs/bare-wire-rule-1-expected.graph"
    },
    { name = "bare wire rewrite (subgraph)",
      subgraph = SOME ["m","p"],
      lhs = "graphs/bare-wire-rule-1-lhs.graph",
      rhs = "graphs/bare-wire-rule-1-rhs.graph",
      target = "graphs/bare-wire-rule-1-target.graph",
      expected = "graphs/bare-wire-rule-1-expected.graph"
    },
    { name = "bare wire rewrite with some initially clashing names",
      subgraph = NONE,
      lhs = "graphs/bare-wire-rule-2-lhs.graph",
      rhs = "graphs/bare-wire-rule-2-rhs.graph",
      target = "graphs/bare-wire-rule-2-target.graph",
      expected = "graphs/bare-wire-rule-2-expected.graph"
    },
    { name = "bare wire rewrite with some initially clashing names (subgraph)",
      subgraph = SOME ["Va","Vb"],
      lhs = "graphs/bare-wire-rule-2-lhs.graph",
      rhs = "graphs/bare-wire-rule-2-rhs.graph",
      target = "graphs/bare-wire-rule-2-target.graph",
      expected = "graphs/bare-wire-rule-2-expected.graph"
    }
    (* TODO: more tests:
     *  - clashing vertex and edge names
     *  - clashing expression variable names (in pattern and target)
     *  - !-boxes with non-boundary vertices
     *  - nested/overlapping !-boxes
     *)
  ];
end;

