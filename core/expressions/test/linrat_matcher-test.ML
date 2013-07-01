local
  val pretty_subst = XTab.pretty LinratAngleExpr.pretty

  fun assert_subst_eq (s1,s2) =
    if XTab.tab_eq (fn (x,y) => LinratAngleExpr.eq x y) (s1,s2)
    then ()
    else raise ERROR ("Substitutions not equal.\n" ^
                      Pretty.string_of (pretty_subst s1) ^ "\n" ^
                      Pretty.string_of (pretty_subst s2) ^ "\n")

  val matr = RationalMatrix.from_lists (map (map (fn i => Rational.mk (i,1))) [
    [1, 0, 0, ~5, 1,  0],
    [0, 1, 2, ~6, 0, ~1],
    [0, 0, 1, ~7, 2,  0]
  ])

  val matr_under_spec = RationalMatrix.from_lists (map (map (fn i => Rational.mk (i,1))) [
    [1, 0, 1, ~5, 1,  0],
    [0, 1, 2, ~6, 0, ~1]
  ])

  val (tab,_) = LinratMatcher.solve_psubst
    (matr, X.NSet.of_list (map X.mk ["a","b","c"]), X.NSet.of_list (map X.mk ["d","e"]))

  val (tab_notfresh,_) = LinratMatcher.solve_psubst
    (matr, X.NSet.of_list (map X.mk ["a","b","c"]), X.NSet.of_list (map X.mk ["a","b"]))

  val (tab_underspec,_) = LinratMatcher.solve_psubst
    (matr_under_spec, X.NSet.of_list (map X.mk ["a","b","c"]), X.NSet.of_list (map X.mk ["d","e"]))

  val (tab_underspec_notfresh,_) = LinratMatcher.solve_psubst
    (matr_under_spec, X.NSet.of_list (map X.mk ["a","b","c"]), X.NSet.of_list (map X.mk ["c","d"]))
in

  val _ = Testing.test "Gauss matrix to subst" assert_subst_eq (
    tab,
    XTab.of_list [
      (X.mk "a", LinratAngleExpr.parse "5 d - e"),
      (X.mk "b", LinratAngleExpr.parse "\\pi - 8 d + 4 e"),
      (X.mk "c", LinratAngleExpr.parse "7 d - 2 e")
    ]
  )

  (* NOTE: it shouldn't make any difference whether the target vars are fresh w.r.t. the pattern *)
  val _ = Testing.test "Gauss matrix to subst (target not fresh w.r.t. pattern)" assert_subst_eq (
    tab_notfresh,
    XTab.of_list [
      (X.mk "a", LinratAngleExpr.parse "5 a - b"),
      (X.mk "b", LinratAngleExpr.parse "\\pi - 8 a + 4 b"),
      (X.mk "c", LinratAngleExpr.parse "7 a - 2 b")
    ]
  )

  val _ = Testing.test "Gauss matrix to subst (under-specified system)" assert_subst_eq (
    tab_underspec,
    XTab.of_list [
      (X.mk "a", LinratAngleExpr.parse "5 d - e - c"),
      (X.mk "b", LinratAngleExpr.parse "\\pi + 6 d - 2 c"),
      (X.mk "c", LinratAngleExpr.parse "c")
    ]
  )

  (* note this assumes NSet.add_new grabs lowest available fresh name above the requested one. In
   * this case, add_new("c") = "e". *)
  val _ = Testing.test "Gauss matrix to subst (under-specified + not fresh)" assert_subst_eq (
    tab_underspec_notfresh,
    XTab.of_list [
      (X.mk "a", LinratAngleExpr.parse "5 c - d - e"),
      (X.mk "b", LinratAngleExpr.parse "\\pi + 6 c - 2 e"),
      (X.mk "c", LinratAngleExpr.parse "e")
    ]
  )

  val _ = Testing.assert_no_failed_tests ()
end