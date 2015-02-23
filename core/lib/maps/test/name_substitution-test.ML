local
  open TestMapsUtil
  
  val sub = ASub.empty
  
  (*** typical mapping tests ***)
  
  val _ = test "An empty substitution" (fn () => (
      assert "should have size 0" (ASub.dom_size sub = 0);
      assert "should report true for is_empy" (ASub.is_empty sub);
      assert "should return NONE for get_opt" (is_none (ASub.get_opt sub a1));
    ())) ()
  
  val sub = sub |> ASub.set (a1, a1)
  val sub = sub |> ASub.set (a2, a2)
  
  val _ = test "An substitution with two elements" (fn () => (
      assert "should have size 2" (ASub.dom_size sub = 2);
      assert "should return correct value for a1" (A.name_eq (ASub.get sub a1, a1));
      assert "should return correct value for a2" (A.name_eq (ASub.get sub a2, a2));
      assert "should return correct inverse-value for a1" (A.name_eq (ASub.inv_get sub a1, a1));
      assert "should return correct inverse-value for a2" (A.name_eq (ASub.inv_get sub a2, a2));
      assert "should return SOME (elem) for get_opt" (A.name_eq (the (ASub.get_opt sub a1), a1));
      assert "should fail on duplicate_exp for duplicate add"
        (catch_dup_fail (fn () => sub |> ASub.add (a1,a2)));
    ())) ()
  
  val sub = sub |> ASub.set (a2, a3)
  
  val _ = test "An substitution after override" (fn () => (
      assert "should still have size 2" (ASub.dom_size sub = 2);
      assert "should still have codomain size 2" (A.NSet.cardinality (ASub.get_cod_set sub) = 2);
      assert "should return new element" (A.name_eq (ASub.get sub a2, a3));
      assert "should return NONE for old codomain" (is_none (ASub.inv_get_opt sub a2));
    ())) ()
  
  val sub = sub |> ASub.delete a1
  
  val _ = test "An substitution after deletion" (fn () => (
      assert "should have size 1" (ASub.dom_size sub = 1);
      assert "should have codomain size 1" (A.NSet.cardinality (ASub.get_cod_set sub) = 1);
      assert "should return NONE for deleted element" (is_none (ASub.get_opt sub a1));
    ())) ()
  
  (*** coercion tests ***)
  
  fun di x = case A.dest x of "a1" => [a1] | "a2" => [a2] | _ => []
  fun multi_di x = case A.dest x of "a1" => [a1] | "a2" => [a2,a3] | _ => []
  fun non_sub_di x = case A.dest x of "a1" => [a1] | "a2" => [a1] | _ => []
  fun dom () = [a1,a2]
  val abs_sub = Map.mk (Map.mk_graph (dom,di), I)
  val multi_abs = Map.mk (Map.mk_graph (dom,multi_di), I)
  val non_sub_abs = Map.mk (Map.mk_graph (dom,non_sub_di), I)
  val coerced_sub = ASub.coerce abs_sub
  
  val _ = test "A coerced map" (fn () => (
      assert "should have size 2" (ASub.dom_size coerced_sub = 2);
      assert "should contain the correct values" (
        (A.name_eq (ASub.get coerced_sub a1, a1)) andalso
        (A.name_eq (ASub.get coerced_sub a2, a2)));
    ())) ()
  
  val _ = test "A bad coercion" (fn () => (
      assert "should raise bad_coercion_exp for multi-valued graph"
        (catch_coerce_fail (fn () => ASub.coerce multi_abs));
      assert "should raise bad_coercion_exp for non-subective graph"
        (catch_coerce_fail (fn () => ASub.coerce non_sub_abs));
    ())) ()
  
  (*** inverses and composition, imported from NAME_INJECTION ***)
  
  val sub = ASub.empty |> ASub.add (a1,a1) |> ASub.add (a2,a2)
  val inv_sub = ASub.inverse_of sub
  
  val _ = test "The inverse of an substitution with two elements" (fn () => (
      assert "should have size 2" (ASub.dom_size sub = 2);
      assert "should return correct value for a1" (A.name_eq (ASub.get inv_sub a1, a1));
      assert "should return correct value for a2" (A.name_eq (ASub.get inv_sub a2, a2));
      assert "should return correct inverse-value for a1" (A.name_eq (ASub.inv_get inv_sub a1, a1));
      assert "should return correct inverse-value for a2" (A.name_eq (ASub.inv_get inv_sub a2, a2));
    ())) ()
  
  val comp_sub = ASub.compose (inv_sub, sub)
  
  val _ = test "The composite of an substitution with its inverse" (fn () => (
      assert "should have size 2" (ASub.dom_size comp_sub = 2);
      assert "should be identity on a1" (A.name_eq (ASub.get comp_sub a1, a1));
      assert "should be identity on a2" (A.name_eq (ASub.get comp_sub a2, a2));
      assert "should be identity on inverse of a1" (A.name_eq (ASub.inv_get comp_sub a1, a1));
      assert "should be identity on inverse of a2" (A.name_eq (ASub.inv_get comp_sub a2, a2));
    ())) ()

  val aset = A.NSet.of_list [a1,a2,a3]
  val avoids_sub = ASub.mk_from_avoids aset |> ASub.extend_fresh aset
  val disj = ASub.img_of_set avoids_sub aset

  val _ = test "A disjoint-renamed set" (fn () => (
      assert "should have size 3" (A.NSet.cardinality disj = 3);
      assert "should be disjoint" (A.NSet.is_empty (A.NSet.intersect aset disj));
    ())) ()

  val p_avoids_sub = ASub.empty
    |> ASub.extend_fixed (A.NSet.of_list [a1])
    |> ASub.avoid_set_in_cod (A.NSet.of_list [a2,a3])
    |> ASub.extend_fresh aset

  val p_disj = ASub.img_of_set p_avoids_sub aset

  val _ = test "A partially disjoint-renamed set" (fn () => (
      assert "should have size 3" (A.NSet.cardinality disj = 3);
      assert "should have intersection of 1" (A.NSet.cardinality (A.NSet.intersect aset p_disj) = 1);
    ())) ()
  
  local
    fun opt_eq (SOME n1, SOME n2) = A.name_eq (n1,n2)
      | opt_eq (NONE,NONE) = true
      | opt_eq _ = false
  in
    fun catch_clash (a,b,c) f = (f(); false)
      handle ASub.name_clash_exp (a',b',c') => (opt_eq (a,a') andalso opt_eq (b,b') andalso A.name_eq (c,c'))
  end

  val sub = ASub.empty
    |> ASub.add (a1,a2)
    |> ASub.add (a2,a3)
    |> ASub.avoid_set_in_cod (A.NSet.of_list [a4,a5])

  val _ = test "A name clashing update" (fn () => (
      assert "should raise correctly for mapping-mapping clash"
        (catch_clash (SOME a1, SOME a4, a2) (fn () => 
          sub |> ASub.set (a4,a2)
        ));
      assert "should raise correctly for mapping-avoids clash"
        (catch_clash (SOME a1, NONE, a2) (fn () => 
          sub |> ASub.avoid_in_cod a2
        ));
      assert "should raise correctly for avoids-mapping clash"
        (catch_clash (NONE, SOME a3, a4) (fn () => 
          sub |> ASub.set (a3,a4)
        ));
      assert "should not raise if existing mapping is repeated"
        (
          sub |> ASub.set (a1,a2);
          true
        );
    ())) ()  

in
  val _ = assert_no_failed_tests ()
end