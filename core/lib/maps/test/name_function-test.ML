local
  open TestMapsUtil
  
  (* naming convention: for arbitrary name-functions, use letters typical for
     functions, but doubled. e.g. ff, gg, hh *)
  val ff = ABFn.empty
  
  (*** typical mapping tests ***)
  
  val _ = test "An empty function" (fn () => (
      assert "should have size 0" (ABFn.dom_size ff = 0);
      assert "should report true for is_empy" (ABFn.is_empty ff);
      assert "should return NONE for get_opt" (is_none (ABFn.get_opt ff a1));
    ())) ()
  
  val ff = ff |> ABFn.set (a1, b1)
  val ff = ff |> ABFn.set (a2, b2)
  val ff = ff |> ABFn.set (a3, b2)
  
  val _ = test "A function mapping three elements" (fn () => (
      assert "should have size 3" (ABFn.dom_size ff = 3);
      assert "should have codomain size 2" (ABFn.cod_size ff = 2);
      assert "should return correct value for a1" (B.name_eq (ABFn.get ff a1, b1));
      assert "should return correct value for a2" (B.name_eq (ABFn.get ff a2, b2));
      assert "should return correct value for a3" (B.name_eq (ABFn.get ff a3, b2));
      assert "should return correct inverse image for b1"
        (A.NSet.eq (ABFn.inv_img ff b1) (A.NSet.of_list [a1]));
      assert "should return correct inverse image for b2"
        (A.NSet.eq (ABFn.inv_img ff b2) (A.NSet.of_list [a2,a3]));
      assert "should return SOME (elem) for get_opt"
        (B.name_eq (the (ABFn.get_opt ff a1), b1));
      assert "should fail with multiple_values_exp for inv_get(b2)"
        (catch_multi_fail (fn () => ABFn.inv_get ff b2));
      assert "should fail with multiple_values_exp for inv_get_opt(b2)"
        (catch_multi_fail (fn () => ABFn.inv_get_opt ff b2));
      assert "should fail on duplicate_exp for duplicate add"
        (catch_dup_fail (fn () => ff |> ABFn.add (a1,b3)));
    ())) ()
  
  val ff = ff |> ABFn.set (a3, b3)
  
  val _ = test "A function after override 1" (fn () => (
      assert "should still have size 3" (ABFn.dom_size ff = 3);
      assert "should have codomain size 3" (ABFn.cod_size ff = 3);
      assert "should return new element" (B.name_eq (ABFn.get ff a3, b3));
      assert "should return correct inverse image for b2"
        (A.NSet.eq (ABFn.inv_img ff b2) (A.NSet.of_list [a2]));
      assert "should return correct inverse image for b3"
        (A.NSet.eq (ABFn.inv_img ff b3) (A.NSet.of_list [a3]));
    ())) ()
  
  val ff = ff |> ABFn.set (a2, b3)
  
  val _ = test "A function after override 2" (fn () => (
      assert "should still have size 3" (ABFn.dom_size ff = 3);
      assert "should have codomain size 2" (ABFn.cod_size ff = 2);
      assert "should return new element" (B.name_eq (ABFn.get ff a2, b3));
      assert "should return empty inverse image for b2"
        (A.NSet.eq (ABFn.inv_img ff b2) (A.NSet.of_list []));
      assert "should return correct inverse image for b3"
        (A.NSet.eq (ABFn.inv_img ff b3) (A.NSet.of_list [a2, a3]));
    ())) ()
  
  val ff = ff |> ABFn.delete a2
  
  val _ = test "A function after deletion 1" (fn () => (
      assert "should have size 2" (ABFn.dom_size ff = 2);
      assert "should still have codomain size 2"
        (ABFn.cod_size ff = 2);
      assert "should return correct inverse image for b3"
        (A.NSet.eq (ABFn.inv_img ff b3) (A.NSet.of_list [a3]));
      assert "should return NONE for deleted element" (is_none (ABFn.get_opt ff a2));
    ())) ()
  
  val ff = ff |> ABFn.delete a3
  
  val _ = test "A function after deletion 2" (fn () => (
      assert "should have size 1" (ABFn.dom_size ff = 1);
      assert "should have codomain size 1"
        (ABFn.cod_size ff = 1);
      assert "should return empty inverse image for b3"
        (A.NSet.eq (ABFn.inv_img ff b3) (A.NSet.of_list []));
      assert "should return NONE for deleted element" (is_none (ABFn.get_opt ff a3));
    ())) ()
  
  (*** coercion tests ***)
  
  fun di x = case A.dest x of "a1" => [b1] | "a2" => [b2] | "a3" => [b2] | _ => []
  fun multi_di x = case A.dest x of "a1" => [b1] | "a2" => [b2,b3] | _ => []
  fun dom () = [a1,a2,a3]
  val abs_ff = Map.mk (Map.mk_graph (dom,di), I)
  val multi_abs = Map.mk (Map.mk_graph (dom,multi_di), I)
  val coerced_ff = ABFn.coerce abs_ff
  
  val _ = test "A coerced map" (fn () => (
      assert "should have size 3" (ABFn.dom_size coerced_ff = 3);
      assert "should contain the correct values" (
        (B.name_eq (ABFn.get coerced_ff a1, b1)) andalso
        (B.name_eq (ABFn.get coerced_ff a2, b2)) andalso
        (B.name_eq (ABFn.get coerced_ff a3, b2)));
    ())) ()
  
  val _ = test "A bad coercion" (fn () => (
      assert "should raise bad_coercion_exp for multi-valued graph"
        (catch_coerce_fail (fn () => ABFn.coerce multi_abs));
    ())) ()
  
  (*** composition ***)
  val ff = ABFn.empty  |> ABFn.add (a1,b1) |> ABFn.add (a2,b2)
                       |> ABFn.add (a3,b3) |> ABFn.add (a4,b2)
  val ff2 = BCFn.empty |> BCFn.add (b1,c2) |> BCFn.add (b2,c1)
                       |> BCFn.add (b3,c2)
  
  (* ff2 o ff :: a1 |-> c2, a2 |-> c1, a3 |-> c2, a4 |-> c1 *)
                       
  val ff_cmp = ACFn.compose (ff2,ff)
  
  val _ = test "A composed function" (fn () => (
      assert "should have size 4" (ACFn.dom_size ff_cmp = 4);
      assert "should have codomain size 2" (ACFn.cod_size ff_cmp = 2);
      assert "should return correct value for a1" (C.name_eq (ACFn.get ff_cmp a1, c2));
      assert "should return correct value for a2" (C.name_eq (ACFn.get ff_cmp a2, c1));
      assert "should return correct value for a3" (C.name_eq (ACFn.get ff_cmp a3, c2));
      assert "should return correct value for a4" (C.name_eq (ACFn.get ff_cmp a4, c1));
      assert "should return correct inverse image for c1"
        (A.NSet.eq (ACFn.inv_img ff_cmp c1) (A.NSet.of_list [a2,a4]));
      assert "should return correct inverse image for c2"
        (A.NSet.eq (ACFn.inv_img ff_cmp c2) (A.NSet.of_list [a1,a3]));
    ())) ()
  
  (* A renaming-like scenario. Suppose we take ff as above and rename its elements with: *)
  val rn_dom = AAInj.empty |> AAInj.add (a1,a5)
                           |> AAInj.add (a2,a2)
                           |> AAInj.add (a3,a1)
                           |> AAInj.add (a4,a3)
  val rn_cod = BBInj.empty |> BBInj.add (b1,b2)
                           |> BBInj.add (b2,b3)
                           |> BBInj.add (b3,b1)
  (* Note that renamings are performed all at once, so the codomain no longer needs to
     be fresh in the domain. *)
  
  
  (* The renamed function of:
       ff  :: a1 |-> b1, a2 |-> b2, a3 |-> b3, a4 |-> b2
    
     is computed as (rn_cod o ff o rn_dom^-1). It should be:
       ff' :: a5 |-> b2, a2 |-> b3, a1 |-> b1, a3 |-> b3 *)
  
  val ff' = ABFn.compose3 (rn_cod, ff, AAInj.inverse_of rn_dom)
  
  val _ = test "A renamed function" (fn () => (
      assert "should have size 4" (ABFn.dom_size ff' = 4);
      assert "should have codomain size 2" (ABFn.cod_size ff' = 3);
      assert "should return correct value for a5" (B.name_eq (ABFn.get ff' a5, b2));
      assert "should return correct value for a2" (B.name_eq (ABFn.get ff' a2, b3));
      assert "should return correct value for a1" (B.name_eq (ABFn.get ff' a1, b1));
      assert "should return correct value for a3" (B.name_eq (ABFn.get ff' a3, b3));
      assert "should return correct inverse image for b1"
        (A.NSet.eq (ABFn.inv_img ff' b1) (A.NSet.of_list [a1]));
      assert "should return correct inverse image for b2"
        (A.NSet.eq (ABFn.inv_img ff' b2) (A.NSet.of_list [a5]));
      assert "should return correct inverse image for b3"
        (A.NSet.eq (ABFn.inv_img ff' b3) (A.NSet.of_list [a2,a3]));
    ())) ()
  
in
  val _ = assert_no_failed_tests ()
end