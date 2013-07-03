local
  structure Rule = RG_Theory.Rule
  structure Ruleset = RG_Theory.Ruleset
  structure Rewriter = BangGraphRewriter(
    structure Rule = Ruleset.BangGraphRule
    structure Matcher = RG_Theory.MatchSearch
  )
  structure RulesetRewriter = RG_Theory.RulesetRewriter
  structure MatchSearch = RG_Theory.MatchSearch
  structure G = RG_Theory.Graph
  val ruleset = "rulesets/red_green/basic.rules"
                |> Json.read_file
                |> RG_GraphicalTheoryIO.InputRulesetJSON.input

  val isom_rule = Ruleset.get_rule ruleset (R.mk "isometry_red")
  val x_abelian1_rule = Ruleset.get_rule ruleset (R.mk "x_abelian1")

  fun list_of_rule_matches r g =
      let val matches = Rewriter.find_matches r g in
        Seq.list_of matches
      end

  val zero_angle = LinratAngleExpr.zero
  val parse_angle = LinratAngleExpr.parse
  fun mkX a = G.NVert (RG_Data.Xnd a)
  fun mkZ a = G.NVert (RG_Data.Znd a)
  val mkH = G.NVert RG_Data.Hnd
  val undir_edge = (Undirected,())
  val dir_edge = (Directed,())
  val bvert = G.WVert
in

val _ = Testing.test "rule2: !match(isom_rule, X - X - Z)" (fn () => let
  (* Example of applications of rule that does not match *)
  val g = G.empty;
  val (n1, g) = g |> G.add_vertex (mkX zero_angle);
  val (n2, g) = g |> G.add_vertex (mkX zero_angle);
  val (n3, g) = g |> G.add_vertex (mkZ zero_angle);
  val (_,  g) = g |> G.add_edge undir_edge n1 n3;
  val (_,  g) = g |> G.add_edge undir_edge n2 n3;
  (*val _ = G.print g;*)
  val _ = case list_of_rule_matches isom_rule g
            of [] => ()
             | _ => raise ERROR "Expected no matches"
  in () end) ();


val _ = Testing.test "rule3: [g1,g2,g3,g4] = subst(isom_lhs, isom_lhs)" (fn () => let
  (* test rewriting isometry lhs with isometry rule *)
  val g = G.empty;
  val (b1, g) = g |> G.add_vertex bvert;
  val (n1, g) = g |> G.add_vertex (mkX zero_angle);
  val (n2, g) = g |> G.add_vertex (mkX zero_angle);
  val (b2, g) = g |> G.add_vertex bvert;
  val (_,  g) = g |> G.add_edge undir_edge b1 n1;
  val (_,  g) = g |> G.add_edge undir_edge n1 n2;
  val (_,  g) = g |> G.add_edge undir_edge n1 n2;
  val (_,  g) = g |> G.add_edge undir_edge n2 b2;
  (*val _ = G.print g;*)

  val _ =
    case list_of_rule_matches isom_rule g
      of [m1,m2,m3,m4] =>
          let
            (*
            val _ = Rule.print r';
            val _ = Rewriter.print_match m1;
            *)
            (*
            val inst_isom_rule = Rewriter.instantiate_rule m1 r';
            val _ = Rule.print inst_isom_rule;
            *)
            val _ = Rewriter.rewrite_at isom_rule m1
            val _ = Rewriter.rewrite_at isom_rule m2
            val _ = Rewriter.rewrite_at isom_rule m3
            val _ = Rewriter.rewrite_at isom_rule m4
          in () end
       | _ => raise ERROR "Expected four matches"
  in () end) ();

val _ = Testing.test "rule4: rewriting bialgebra (self-matches)" (fn () => let
  val r = Ruleset.get_rule ruleset (R.mk "bialgebra1");
  val g = Rule.get_lhs r;
  val _ =
    case list_of_rule_matches r g
      of [m1,m2,m3,m4] =>
          let
            (*
            val _ = Rule.print r';
            val _ = Rewriter.print_match m1;
            *)
            (*
            val inst_isom_rule = Rewriter.instantiate_rule m1 r';
            val _ = Rule.print inst_isom_rule;
            *)
            val _ = Rewriter.rewrite_at r m1
            val _ = Rewriter.rewrite_at r m2
            val _ = Rewriter.rewrite_at r m3
            val _ = Rewriter.rewrite_at r m4
          in () end
       | _ => raise ERROR "Expected four matches"
  in () end) ();  
  

  (* Self-Loop, we create two vertices with self-loops and try to rewrite the
   * whole graph. *)
  val _ = Testing.test "rule5: rewriting x_abelian1 (loop)" (fn () => let
  val g = G.empty;
  val (n1, g) = g |> G.add_vertex (mkX zero_angle);
  val (n2, g) = g |> G.add_vertex (mkX zero_angle);
  val (_,  g) = g |> G.add_edge undir_edge n1 n1;
  val (_,  g) = g |> G.add_edge undir_edge n2 n2;
  (*val _ = G.print g;*)

  val m1 = list_of_rule_matches x_abelian1_rule g
  (*
  val _ = Rewriter.print_match (hd m1)
  *)
  val (_,g) = Rewriter.rewrite_at x_abelian1_rule (hd m1)
  val m1 = list_of_rule_matches x_abelian1_rule g
  val (_,g) = Rewriter.rewrite_at x_abelian1_rule (hd m1)
  (*val _ = G.print g*)

  in () end) ();  


val _ = Testing.test "rule6: X(a+b)- => X(a)-X(b)-, matching X(c)-Z(b): renaming of internal variables prior to rewriting." (fn () => let
  (* lhs *)
  val lhs = let val g = G.empty;
      val (b1, g) = g |> G.add_vertex bvert;
      val (x1, g) = g |> G.add_vertex (mkX (parse_angle "a + b"));
      val (_,  g) = g |> G.add_edge undir_edge x1 b1;
  in g end;
  (*val _ = G.print lhs;*)
  (* rhs *)
  val rhs = let val g = G.empty;
      val (b1, g) = g |> G.add_vertex bvert;
      val (x1, g) = g |> G.add_vertex (mkX (parse_angle "a"));
      val (x2, g) = g |> G.add_vertex (mkX (parse_angle "b"));
      val (_,  g) = g |> G.add_edge undir_edge x1 x2;
      val (_,  g) = g |> G.add_edge undir_edge x2 b1;
  in g end;
  (*val _ = G.print rhs;*)
  val rule = Rule.mk (lhs,rhs);
  (* tgt graph *)
  val tgt = let val g = G.empty;
      val (x1, g) = g |> G.add_vertex (mkX (parse_angle "c"));
      val (x2, g) = g |> G.add_vertex (mkZ (parse_angle "b"));
      val (_,  g) = g |> G.add_vertex bvert;
      val (_,  g) = g |> G.add_edge undir_edge x1 x2;
  in g end
  
  val _ = list_of_rule_matches rule tgt
  
  in () end) ();


val _ = Testing.test "rule7: isom_rule rewrites subgraph" (fn () => let
  (* Example of applications of rule that does not match *)
  val g = G.empty;
  val (x1, g) = g |> G.add_vertex (mkX zero_angle);
  val (x2, g) = g |> G.add_vertex (mkX zero_angle);
  val (z1, g) = g |> G.add_vertex (mkZ zero_angle);
  val (z2, g) = g |> G.add_vertex (mkZ zero_angle);
  val (_,  g) = g |> G.add_edge undir_edge z1 x1;
  val (_,  g) = g |> G.add_edge undir_edge x1 x2;
  val (_,  g) = g |> G.add_edge undir_edge x1 x2;
  val (_,  g) = g |> G.add_edge undir_edge x2 z2;
  (*val _ = G.print g;*)
  val verts = V.NSet.single x1;
  val rseq = RulesetRewriter.applicable_rules ruleset
      (fn r => fn g => Rewriter.find_matches_in_subgraph r g verts)
      (R.NSet.single (R.mk "isometry_red")) g;
  val _ = Seq.list_of rseq;
  in () end) ();


  val Ba = B.mk "Ba"
  
  val Va = V.mk "Va"
  val Vb = V.mk "Vb"
  val Vc = V.mk "Vc"
  val Vd = V.mk "Vd"
  val Vf = V.mk "Vf"
  
  val Vx = V.mk "Vx"
  val Vy = V.mk "Vy"

(* lhs *)
  
  
  val lhs = G.empty
    |> G.add_named_vertex Va bvert
    |> G.add_named_vertex Vc (mkZ zero_angle)
    |> G.add_named_bbox Ba
    |> G.add_to_bbox_anon Ba (V.NSet.of_list [Va])
    |> G.add_edge_anon dir_edge Vc Va
  
  (*
  val _ = Pretty.writeln (Pretty.str "interior node bbox rule")
  val _ = G.print lhs;
  *)
  (* rhs *)
  val rhs = G.empty
    |> G.add_named_vertex Va bvert
    |> G.add_named_vertex Vf (mkX zero_angle)
    |> G.add_named_vertex Vd (mkH)
    |> G.add_named_bbox Ba
    |> G.add_to_bbox_anon Ba (V.NSet.of_list [Va,Vd])
    |> G.add_edge_anon dir_edge Vf Vd
    |> G.add_edge_anon dir_edge Vd Va
    
  (*val rhs = G.empty
    |> G.add_named_vertex Va bvert
    |> G.add_named_vertex Vf (mkX zero_angle)
    |> G.add_named_vertex Vd (mkH)
    |> G.add_named_bbox Ba
    |> G.add_to_bbox_anon Ba (V.NSet.of_list [Va,Vd])
    |> G.add_edge_anon dir_edge Vf Vd
    |> G.add_edge_anon dir_edge Vd Va*)
  
  (*val _ = G.print rhs;*)
  val rule = Rule.mk (lhs,rhs);
  
  (*
  val _ = Pretty.writeln (Pretty.str "*** rule (before freshening):")
  val _ = Rule.print rule;
  *)
  
  (* tgt graph *)
  val tgt = G.empty
    |> G.add_named_vertex Va bvert
    |> G.add_named_vertex Vb (mkZ zero_angle)
    |> G.add_edge_anon dir_edge Vb Va

  val _ = Testing.test "rule with interior node in bbox on RHS" (fn () => let
  val m1 = list_of_rule_matches rule tgt
  
  (*
  val _ = Pretty.writeln (Pretty.str "*** match state:")
  val match = hd m1
  val _ = Rewriter.print_match match
  *)
  (*val rhs' = (MatchSearch.BGMatchState.replay_pat_bbox_ops match (Rule.get_rhs rule))
  *)
  
  (*exception die_exp of unit
  val _ = raise die_exp ()*)
  (*val (_,g) = Rewriter.rewrite_at rule (hd m1)
  val _ = G.print g;*)

  in () end) ();
  
  
  val _ = Testing.assert_no_failed_tests();
end





