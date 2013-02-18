(* Testing Linear Rational Angle Expressions and matching of them. *)

(* TODO: more extensive testing, including substitutions *)
local 
  structure Expr = LinratAngleExpr; (* expressions *)
  structure Var = X; (* variable names *)
  structure Coeff = Expr.Coeff; (* rational numbers *)
  
  val zero = Expr.zero;
  val pi = Expr.pi;
  
  val a_v = Var.mk "a";
  val b_v = Var.mk "b";
  val c_v = Var.mk "c";
  
  val a = Expr.mk_var a_v;
  val b = Expr.mk_var b_v;
  val c = Expr.mk_var c_v;

  val one = Coeff.mk 1 1;
  val two = Coeff.mk 2 1;

  val one_e = Expr.mk one [];
  
  fun op +(x,y) = Expr.add_expr x y;
  fun op -(x,y) = Expr.subtr_expr x y;
  fun op *(k,x) = Expr.scale_expr k x;

  val expr_str = Pretty.string_of o Expr.pretty;
  val match_str = Pretty.string_of o Expr.pretty_match;

  fun has_match (e1,e2) =
    case Expr.match e1 e2 Expr.empty_match
      of SOME m => m
       | NONE => raise ERROR ("Expected "^(expr_str e1)^" to match "
                              ^(expr_str e2));
  fun no_match (e1,e2) =
    case Expr.match e1 e2 Expr.empty_match
      of NONE => ()
       | SOME m => raise ERROR ("Unexpected match "^(match_str m)^" of "
                                ^(expr_str e1)^" onto " ^(expr_str e2));

  fun rename_fails (e,(from,to)) =
    case Expr.try_rename1 from to e
      of NONE => ()
       | SOME e1 => raise ERROR (
            "Renaming \""^(Var.string_of_name from)^"\" to \""
            ^(Var.string_of_name to)^"\" in \""^(expr_str e)^
            "\" unexpectedly succeeded, giving \""^(expr_str e1)^"\"");
  fun rename_succeeds (e,(from,to),result) =
    case Expr.try_rename1 from to e
      of SOME e1 =>
          if Expr.eq e1 result
          then e1
          else raise ERROR ("Renaming \""^(Var.string_of_name from)^"\" to \""
                            ^(Var.string_of_name to)^"\" in \""^(expr_str e)^
                             "\": expected \""^(expr_str result)^"\" but got \""
                             ^(expr_str e1)^"\"")
       | NONE => raise ERROR ("Renaming \""^(Var.string_of_name from)^"\" to \""
                              ^(Var.string_of_name to)^"\" in \""^(expr_str e)^
                              "unexpectedly failed");

  fun parse (str,exp) =
    let val result = Expr.parse str
    in
      if Expr.eq result exp then ()
      else raise ERROR ("Parsing \""^str^"\" gave \""^(expr_str result)^
                        "\" instead of \""^(expr_str exp)^"\"")
    end;

  fun parse_fail str =
    (Expr.parse str; raise ERROR ("Parser accepted bad expression \""^str^"\""))
    handle Expr.parse_exp => ()
         | Coeff.BadExpression _ => ();
in
  val _ = Testing.test "Parse 0" parse ("0", zero);
  val _ = Testing.test "Parse a + b" parse ("a + b", a + b);
  val _ = Testing.test "Parse 2\\pi + 1" parse ("2\\pi + 1", two*pi + one_e);
  val _ = Testing.test "Parse 3/4 a" parse ("3/4 a", (Coeff.mk 3 4)*a);
  val _ = Testing.test "Parse 1/2" parse ("1/2", Expr.mk (Coeff.mk 1 2) []);

  val _ = Testing.test "Parse (fail) &" parse_fail "&";
  val _ = Testing.test "Parse (fail) b/a" parse_fail "b/a";
  val _ = Testing.test "Parse (fail) 1/0" parse_fail "1/0";

  (* TODO: check the right results are produced, not just *some* matching! *)
  val _ = Testing.test "match a 0 = SOME ..."  has_match (a, zero);
          (* a => (0) - (0) *)

  val _ = Testing.test "match (a + b) 0 = SOME ..." has_match (a + b, zero);
          (* a => (0) - (b) *)

  val _ = Testing.test "match (a + b) a = SOME ..." has_match (a + b, zero);
          (*  a => (a) - (b) *)

  val _ = Testing.test "match a (a + b) = SOME ..." has_match (a, a + b);
          (* a => (a + b) - (0) *)

  val _ = Testing.test "match pi pi = SOME ..." has_match (pi,pi);
  val _ = Testing.test "match (a - pi) (2 * pi) = SOME ..." has_match
          (a - pi, two * pi);

  val _ = Testing.test "match 0 a = false" no_match (zero,a);
  val _ = Testing.test "match pi a = false" no_match (pi,a);
  val _ = Testing.test "match pi 0 = false" no_match (pi,zero);
  val _ = Testing.test "match 0 pi = false" no_match (zero,pi);
  val _ = Testing.test "match pi 2pi = false" no_match (pi,two*pi);

  val _ = Testing.test "try_rename1 (a + b)[a -> b]" rename_fails
          (a + b,(a_v,b_v));
  val _ = Testing.test "try_rename1 (a + b)[a -> c]" rename_succeeds
          (a + b,(a_v,c_v),b + c);

  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR LinratAngleExpr PASSED!";
end;
