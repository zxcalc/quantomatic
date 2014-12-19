local
  open TestMapsUtil
  
  val inj = ABInj.empty
  
  (*** typical mapping tests ***)
  
  val _ = test "An empty injection" (fn () => (
      assert "should have size 0" (ABInj.dom_size inj = 0);
      assert "should report true for is_empy" (ABInj.is_empty inj);
      assert "should return NONE for get_opt" (is_none (ABInj.get_opt inj a1));
    ())) ()
  
  val inj = inj |> ABInj.set (a1, b1)
  val inj = inj |> ABInj.set (a2, b2)
  
  val _ = test "An injection with two elements" (fn () => (
      assert "should have size 2" (ABInj.dom_size inj = 2);
      assert "should return correct value for a1" (B.name_eq (ABInj.get inj a1, b1));
      assert "should return correct value for a2" (B.name_eq (ABInj.get inj a2, b2));
      assert "should return correct inverse-value for b1" (A.name_eq (ABInj.inv_get inj b1, a1));
      assert "should return correct inverse-value for b2" (A.name_eq (ABInj.inv_get inj b2, a2));
      assert "should return SOME (elem) for get_opt" (B.name_eq (the (ABInj.get_opt inj a1), b1));
      assert "should fail on duplicate_exp for duplicate add"
        (catch_dup_fail (fn () => inj |> ABInj.add (a1,b2)));
    ())) ()
  
  val inj = inj |> ABInj.set (a2, b3)
  
  val _ = test "An injection after override" (fn () => (
      assert "should still have size 2" (ABInj.dom_size inj = 2);
      assert "should still have codomain size 2" (B.NSet.cardinality (ABInj.get_cod_set inj) = 2);
      assert "should return new element" (B.name_eq (ABInj.get inj a2, b3));
      assert "should return NONE for old codomain" (is_none (ABInj.inv_get_opt inj b2));
    ())) ()
  
  val inj = inj |> ABInj.delete a1
  
  val _ = test "An injection after deletion" (fn () => (
      assert "should have size 1" (ABInj.dom_size inj = 1);
      assert "should have codomain size 1" (B.NSet.cardinality (ABInj.get_cod_set inj) = 1);
      assert "should return NONE for deleted element" (is_none (ABInj.get_opt inj a1));
    ())) ()
  
  (*** coercion tests ***)
  
  fun di x = case A.dest x of "a1" => [b1] | "a2" => [b2] | _ => []
  fun multi_di x = case A.dest x of "a1" => [b1] | "a2" => [b2,b3] | _ => []
  fun non_inj_di x = case A.dest x of "a1" => [b1] | "a2" => [b1] | _ => []
  fun dom () = [a1,a2]
  val abs_inj = Map.mk (Map.mk_graph (dom,di), I)
  val multi_abs = Map.mk (Map.mk_graph (dom,multi_di), I)
  val non_inj_abs = Map.mk (Map.mk_graph (dom,non_inj_di), I)
  val coerced_inj = ABInj.coerce abs_inj
  
  val _ = test "A coerced map" (fn () => (
      assert "should have size 2" (ABInj.dom_size coerced_inj = 2);
      assert "should contain the correct values" (
        (B.name_eq (ABInj.get coerced_inj a1, b1)) andalso
        (B.name_eq (ABInj.get coerced_inj a2, b2)));
    ())) ()
  
  val _ = test "A bad coercion" (fn () => (
      assert "should raise bad_coercion_exp for multi-valued graph"
        (catch_coerce_fail (fn () => ABInj.coerce multi_abs));
      assert "should raise bad_coercion_exp for non-injective graph"
        (catch_coerce_fail (fn () => ABInj.coerce non_inj_abs));
    ())) ()
  
  (*** inverses and composition ***)
  val inj = ABInj.empty |> ABInj.add (a1,b1) |> ABInj.add (a2,b2)
  val inv_inj = BAInj.inverse_of inj
  
  val _ = test "The inverse of an injection with two elements" (fn () => (
      assert "should have size 2" (ABInj.dom_size inj = 2);
      assert "should return correct value for b1" (A.name_eq (BAInj.get inv_inj b1, a1));
      assert "should return correct value for b2" (A.name_eq (BAInj.get inv_inj b2, a2));
      assert "should return correct inverse-value for a1" (B.name_eq (BAInj.inv_get inv_inj a1, b1));
      assert "should return correct inverse-value for a2" (B.name_eq (BAInj.inv_get inv_inj a2, b2));
    ())) ()
  
  val comp_inj = AAInj.compose (inv_inj, inj)
  
  val _ = test "The composite of an injection with its inverse" (fn () => (
      assert "should have size 2" (AAInj.dom_size comp_inj = 2);
      assert "should be identity on a1" (A.name_eq (AAInj.get comp_inj a1, a1));
      assert "should be identity on a2" (A.name_eq (AAInj.get comp_inj a2, a2));
      assert "should be identity on inverse of a1" (A.name_eq (AAInj.inv_get comp_inj a1, a1));
      assert "should be identity on inverse of a2" (A.name_eq (AAInj.inv_get comp_inj a2, a2));
    ())) ()
  
in
  val _ = assert_no_failed_tests ()
end