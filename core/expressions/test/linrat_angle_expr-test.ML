(* Testing Linear Rational Angle Expressions *)

(* TODO: more extensive testing, including substitutions *)
local
  structure Expr = LinratAngleExpr (* expressions *)
  structure Var = X (* variable names *)
  structure Coeff = Expr.Coeff (* rational numbers *)
  
  val zero = Expr.zero
  val pi = Expr.pi
  
  val a_v = Var.mk "a"
  val b_v = Var.mk "b"
  
  val a = Expr.mk_var a_v
  val b = Expr.mk_var b_v

  val one = Coeff.mk (1, 1)
  val neg_one = Coeff.mk (~1, 1)
  val two = Coeff.mk (2, 1)

  val one_e = Expr.mk one []
  fun const coeff = Expr.mk coeff []
  
  fun op +(x,y) = Expr.add_expr x y
  fun op -(x,y) = Expr.subtr_expr x y
  fun op *(k,x) = Expr.scale_expr k x

  val expr_str = Pretty.string_of o Expr.pretty

  fun reparse exp =
    let
      val str = expr_str exp
    in
      let
        val result = Expr.parse str
      in
        if not (Expr.eq result exp)
        then raise ERROR ("Reparsing \""^str^"\" gave \""^(expr_str result))
        else ()
      end
      handle Expr.parse_exp =>
        raise ERROR ("Got parse_exp when reparsing "^str)
    end

  fun parse (str,exp) =
    let val result = Expr.parse str
    in
      if not (Expr.eq result exp)
      then raise ERROR ("Parsing \""^str^"\" gave \""^(expr_str result)^
                        "\" instead of \""^(expr_str exp)^"\"")
      else ()
    end
    handle Expr.parse_exp =>
      raise ERROR ("Got parse_exp when parsing "^str)

  fun parse_fail str =
    (Expr.parse str; raise ERROR ("Parser accepted bad expression \""^str^"\""))
    handle Expr.parse_exp => ()
         | Div => ()

  fun test_reparse expr =
    Testing.test ("Reparse "^(expr_str expr)) reparse expr
  fun test_parse str expr =
    Testing.test ("Parse "^str) parse (str,expr)
  fun test_parse_fail str =
    Testing.test ("Parse (fail) "^str) parse_fail str
in
  val _ = test_reparse zero
  val _ = test_parse "0" zero
  val _ = test_parse "" zero

  val _ = test_reparse (const one)
  val _ = test_parse "\\pi" (const one)
  val _ = test_parse "1\\pi" (const one)
  val _ = test_parse "1*\\pi" (const one)
  val _ = test_parse "1/1\\pi" (const one)
  val _ = test_parse "1/1*\\pi" (const one)
  val _ = test_parse "1\\pi/1" (const one)
  val _ = test_parse "1*\\pi/1" (const one)
  val _ = test_parse "\\pi/1" (const one)

  val _ = test_parse "pi" (const one)
  val _ = test_parse "1pi" (const one)
  val _ = test_parse "1*pi" (const one)
  val _ = test_parse "1/1pi" (const one)
  val _ = test_parse "1/1*pi" (const one)
  val _ = test_parse "1pi/1" (const one)
  val _ = test_parse "1*pi/1" (const one)
  val _ = test_parse "pi/1" (const one)

  val _ = test_parse "PI" (const one)
  val _ = test_parse "1PI" (const one)
  val _ = test_parse "1*PI" (const one)
  val _ = test_parse "1/1PI" (const one)
  val _ = test_parse "1/1*PI" (const one)
  val _ = test_parse "1PI/1" (const one)
  val _ = test_parse "1*PI/1" (const one)
  val _ = test_parse "PI/1" (const one)

  val _ = test_reparse a
  val _ = test_parse "a" a
  val _ = test_parse "1a" a
  val _ = test_parse "1*a" a
  val _ = test_parse "1/1a" a
  val _ = test_parse "1/1*a" a
  val _ = test_parse "1a/1" a
  val _ = test_parse "1*a/1" a
  val _ = test_parse "a/1" a

  val _ = test_reparse (const neg_one)
  val _ = test_parse "-pi" (const neg_one)
  val _ = test_parse "-1pi" (const neg_one)
  val _ = test_parse "-1*pi" (const neg_one)
  val _ = test_parse "-1/1pi" (const neg_one)
  val _ = test_parse "-1/1*pi" (const neg_one)
  val _ = test_parse "-1pi/1" (const neg_one)
  val _ = test_parse "-1*pi/1" (const neg_one)
  val _ = test_parse "-pi/1" (const neg_one)

  val _ = test_reparse (neg_one*a)
  val _ = test_parse "-a" (neg_one*a)
  val _ = test_parse "-1a" (neg_one*a)
  val _ = test_parse "-1*a" (neg_one*a)
  val _ = test_parse "-1/1a" (neg_one*a)
  val _ = test_parse "-1/1*a" (neg_one*a)
  val _ = test_parse "-1a/1" (neg_one*a)
  val _ = test_parse "-1*a/1" (neg_one*a)
  val _ = test_parse "-a/1" (neg_one*a)

  val _ = test_reparse (const (Coeff.mk (3, 4)))
  val _ = test_parse "3/4" (const (Coeff.mk (3, 4)))
  val _ = test_parse "3/4pi" (const (Coeff.mk (3, 4)))
  val _ = test_parse "3/4*pi" (const (Coeff.mk (3, 4)))
  val _ = test_parse "3pi/4" (const (Coeff.mk (3, 4)))
  val _ = test_parse "3*pi/4" (const (Coeff.mk (3, 4)))

  val _ = test_reparse (const (Coeff.mk (~3, 4)))
  val _ = test_parse "-3/4" (const (Coeff.mk (~3, 4)))
  val _ = test_parse "-3/4pi" (const (Coeff.mk (~3, 4)))
  val _ = test_parse "-3/4*pi" (const (Coeff.mk (~3, 4)))
  val _ = test_parse "-3pi/4" (const (Coeff.mk (~3, 4)))
  val _ = test_parse "-3*pi/4" (const (Coeff.mk (~3, 4)))

  val _ = test_reparse ((Coeff.mk (3, 4))*a)
  val _ = test_parse "3/4a" ((Coeff.mk (3, 4))*a)
  val _ = test_parse "3/4*a" ((Coeff.mk (3, 4))*a)
  val _ = test_parse "3a/4" ((Coeff.mk (3, 4))*a)
  val _ = test_parse "3*a/4" ((Coeff.mk (3, 4))*a)

  val _ = test_reparse ((Coeff.mk (~3, 4))*a)
  val _ = test_parse "-3/4a" ((Coeff.mk (~3, 4))*a)
  val _ = test_parse "-3/4*a" ((Coeff.mk (~3, 4))*a)
  val _ = test_parse "-3a/4" ((Coeff.mk (~3, 4))*a)
  val _ = test_parse "-3*a/4" ((Coeff.mk (~3, 4))*a)

  val _ = test_parse "pi/4" (const (Coeff.mk (1, 4)))
  val _ = test_parse "-pi/4" (const (Coeff.mk (~1, 4)))
  val _ = test_parse "a/4" ((Coeff.mk (1, 4))*a)
  val _ = test_parse "-a/4" ((Coeff.mk (~1, 4))*a)

  val _ = test_reparse (a + b)
  val _ = test_reparse (a - b)
  val _ = test_reparse ((neg_one*a) - b)

  val _ = test_parse "a + b" (a + b)
  val _ = test_parse "a - b" (a - b)
  val _ = test_parse "-a + b" (b - a)
  val _ = test_parse "- a - b" ((neg_one*a) - b)
  val _ = test_parse "-(a + b)" ((neg_one*a) - b)
  val _ = test_parse "-(a - b)" (b - a)

  val _ = test_parse "2\\pi + 1" (two*pi + (const one))
  val _ = test_reparse (two*pi + (const one))

  val _ = test_parse_fail "&"
  val _ = test_parse_fail "b/a"
  val _ = test_parse_fail "a*b"
  val _ = test_parse_fail "1/0"

  val _ = Testing.assert_no_failed_tests()
end
