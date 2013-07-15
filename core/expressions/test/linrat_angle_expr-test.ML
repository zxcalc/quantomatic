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

  val one = Coeff.mk (1, 1);
  val two = Coeff.mk (2, 1);

  val one_e = Expr.mk one [];
  
  fun op +(x,y) = Expr.add_expr x y;
  fun op -(x,y) = Expr.subtr_expr x y;
  fun op *(k,x) = Expr.scale_expr k x;

  val expr_str = Pretty.string_of o Expr.pretty;

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
         | Div => ();
in
  val _ = Testing.test "Parse 0" parse ("0", zero);
  val _ = Testing.test "Parse a + b" parse ("a + b", a + b);
  val _ = Testing.test "Parse 2\\pi + 1" parse ("2\\pi + 1", two*pi + one_e);
  val _ = Testing.test "Parse 3/4 a" parse ("3/4 a", (Coeff.mk (3, 4))*a);
  val _ = Testing.test "Parse 1/2" parse ("1/2", Expr.mk (Coeff.mk (1, 2)) []);

  val _ = Testing.test "Parse (fail) &" parse_fail "&";
  val _ = Testing.test "Parse (fail) b/a" parse_fail "b/a";
  val _ = Testing.test "Parse (fail) 1/0" parse_fail "1/0";


  val _ = Testing.assert_no_failed_tests();
end;
