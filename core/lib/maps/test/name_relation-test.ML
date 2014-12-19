functor TestNameRelation(
  structure ABRel : NAME_RELATION
      where type dom = TestMapsUtil.A.name
        and type DomSet.T = TestMapsUtil.A.NSet.T
        and type cod = TestMapsUtil.B.name
        and type CodSet.T = TestMapsUtil.B.NSet.T
  structure BCRel : NAME_RELATION
      where type dom = TestMapsUtil.B.name
        and type DomSet.T = TestMapsUtil.B.NSet.T
        and type cod = TestMapsUtil.C.name
        and type CodSet.T = TestMapsUtil.C.NSet.T
  structure ACRel : NAME_RELATION
      where type dom = TestMapsUtil.A.name
        and type DomSet.T = TestMapsUtil.A.NSet.T
        and type cod = TestMapsUtil.C.name
        and type CodSet.T = TestMapsUtil.C.NSet.T
) =
struct
  open TestMapsUtil
  
  (* naming convention: for arbitrary name-relations, use letters typical for
     relations, but doubled. e.g. rel, gg, hh *)
  val rel0 = ABRel.empty
  
  (*** typical mapping tests ***)
  
  val _ = test "An empty relation" (fn () => (
      assert "should have domain size 0" (ABRel.dom_size rel0 = 0);
      assert "should have codomain size 0" (ABRel.cod_size rel0 = 0);
      assert "should report true for is_empy" (ABRel.is_empty rel0);
      assert "should return NONE for get_opt" (is_none (ABRel.get_opt rel0 a1));
    ())) ()
  
  val rel0 = rel0 |> ABRel.add (a1, b1) |> ABRel.add (a2, b2)
                  |> ABRel.add (a3, b2) |> ABRel.add (a3, b3)
  val rel = rel0
  
  val _ = test "A relation" (fn () => (
      assert "should have domain size 3" (ABRel.dom_size rel = 3);
      assert "should have codomain size 3" (ABRel.cod_size rel = 3);
      assert "should return true for is_mapped(mapped pair)"
        (ABRel.is_mapped rel (a1,b1));
      assert "should return false for is_mapped(not mapped pair)"
        (not (ABRel.is_mapped rel (a1,b2)));
      assert "should return correct image for a1"
        (B.NSet.eq (ABRel.img rel a1) (B.NSet.of_list [b1]));
      assert "should return correct image for a2"
        (B.NSet.eq (ABRel.img rel a2) (B.NSet.of_list [b2]));
      assert "should return correct image for a3"
        (B.NSet.eq (ABRel.img rel a3) (B.NSet.of_list [b2,b3]));
      assert "should return correct inverse image for b1"
        (A.NSet.eq (ABRel.inv_img rel b1) (A.NSet.of_list [a1]));
      assert "should return correct inverse image for b2"
        (A.NSet.eq (ABRel.inv_img rel b2) (A.NSet.of_list [a2,a3]));
      assert "should return correct inverse image for b3"
        (A.NSet.eq (ABRel.inv_img rel b3) (A.NSet.of_list [a3]));
      assert "can get singleton for get(a2)"
        (B.name_eq (ABRel.get rel a2, b2));
      assert "can get SOME (singleton) for get_opt(a2)"
        (B.name_eq (the (ABRel.get_opt rel a1), b1));
      assert "should fail with multiple_values_exp for get(a3)"
        (catch_multi_fail (fn () => ABRel.get rel a3));
      assert "should fail with multiple_values_exp for get_opt(a3)"
        (catch_multi_fail (fn () => ABRel.get_opt rel a3));
      assert "should fail with multiple_values_exp for inv_get(b2)"
        (catch_multi_fail (fn () => ABRel.inv_get rel b2));
      assert "should fail with multiple_values_exp for inv_get_opt(b2)"
        (catch_multi_fail (fn () => ABRel.inv_get_opt rel b2));
    ())) ()

  val rel = rel0 |> ABRel.unmap (a3,b2)
  val _ = test "A relation after unmap" (fn () => (
      assert "should still have domain size 3" (ABRel.dom_size rel = 3);
      assert "should still have codomain size 3" (ABRel.cod_size rel = 3);
      assert "should return correct image for a1"
        (B.NSet.eq (ABRel.img rel a1) (B.NSet.of_list [b1]));
      assert "should return correct image for a2"
        (B.NSet.eq (ABRel.img rel a2) (B.NSet.of_list [b2]));
      assert "should return correct image for a3"
        (B.NSet.eq (ABRel.img rel a3) (B.NSet.of_list [b3]));
      assert "should return correct inverse image for b1"
        (A.NSet.eq (ABRel.inv_img rel b1) (A.NSet.of_list [a1]));
      assert "should return correct inverse image for b2"
        (A.NSet.eq (ABRel.inv_img rel b2) (A.NSet.of_list [a2]));
      assert "should return correct inverse image for b3"
        (A.NSet.eq (ABRel.inv_img rel b3) (A.NSet.of_list [a3]));
    ())) ()
  
  val rel = rel0 |> ABRel.delete a3
  val _ = test "A relation after domain deletion" (fn () => (
      assert "should have domain size 2" (ABRel.dom_size rel = 2);
      assert "should have codomain size 2" (ABRel.cod_size rel = 2);
      assert "should return correct image for a1"
        (B.NSet.eq (ABRel.img rel a1) (B.NSet.of_list [b1]));
      assert "should return correct image for a2"
        (B.NSet.eq (ABRel.img rel a2) (B.NSet.of_list [b2]));
      assert "should return correct image for a3"
        (B.NSet.eq (ABRel.img rel a3) (B.NSet.of_list []));
      assert "should return correct inverse image for b1"
        (A.NSet.eq (ABRel.inv_img rel b1) (A.NSet.of_list [a1]));
      assert "should return correct inverse image for b2"
        (A.NSet.eq (ABRel.inv_img rel b2) (A.NSet.of_list [a2]));
      assert "should return correct inverse image for b3"
        (A.NSet.eq (ABRel.inv_img rel b3) (A.NSet.of_list []));
    ())) ()
  
  val rel = rel0 |> ABRel.inv_delete b3
  val _ = test "A relation after codomain deletion" (fn () => (
      assert "should still have domain size 3" (ABRel.dom_size rel = 3);
      assert "should have codomain size 2" (ABRel.cod_size rel = 2);
      assert "should return correct image for a1"
        (B.NSet.eq (ABRel.img rel a1) (B.NSet.of_list [b1]));
      assert "should return correct image for a2"
        (B.NSet.eq (ABRel.img rel a2) (B.NSet.of_list [b2]));
      assert "should return correct image for a3"
        (B.NSet.eq (ABRel.img rel a3) (B.NSet.of_list [b2]));
      assert "should return correct inverse image for b1"
        (A.NSet.eq (ABRel.inv_img rel b1) (A.NSet.of_list [a1]));
      assert "should return correct inverse image for b2"
        (A.NSet.eq (ABRel.inv_img rel b2) (A.NSet.of_list [a2,a3]));
      assert "should return correct inverse image for b3"
        (A.NSet.eq (ABRel.inv_img rel b3) (A.NSet.of_list []));
    ())) ()
  
  
  
  (*** coercion tests ***)
  
  fun di x = case A.dest x of "a1" => [b1] | "a2" => [b2] | "a3" => [b2] | _ => []
  fun multi_di x = case A.dest x of "a1" => [b1] | "a2" => [b2,b3] | _ => []
  fun dom () = [a1,a2,a3]
  val abs_rel = Map.mk (Map.mk_graph (dom,di), I)
  val multi_abs = Map.mk (Map.mk_graph (dom,multi_di), I)
  val coerced_rel = ABRel.coerce abs_rel
  val multi_coerced_rel = ABRel.coerce multi_abs
  
  val _ = test "A coerced map" (fn () => (
      assert "should have domain size 3" (ABRel.dom_size coerced_rel = 3);
      assert "should contain the correct values" (
        (B.name_eq (ABRel.get coerced_rel a1, b1)) andalso
        (B.name_eq (ABRel.get coerced_rel a2, b2)) andalso
        (B.name_eq (ABRel.get coerced_rel a3, b2)));
    ())) ()
  
  val _ = test "A multi-valued coercion" (fn () => (
      assert "should return correct image for a1"
        (B.NSet.eq (ABRel.img multi_coerced_rel a1) (B.NSet.of_list [b1]));
      assert "should return correct image for a2"
        (B.NSet.eq (ABRel.img multi_coerced_rel a2) (B.NSet.of_list [b2,b3]));
      assert "should return correct image for a3"
        (B.NSet.eq (ABRel.img multi_coerced_rel a3) (B.NSet.of_list []));
    ())) ()
  
  
  (*** composition ***)
  val rel = ABRel.empty  |> ABRel.add (a1,b1) |> ABRel.add (a1,b2)
                         |> ABRel.add (a2,b2) |> ABRel.add (a3,b1)
                         |> ABRel.add (a4,b3)
  val rel2 = BCRel.empty |> BCRel.add (b1,c1) |> BCRel.add (b1,c2)
                         |> BCRel.add (b2,c2) |> BCRel.add (b4,c4)
  
  (* rel2 o rel :: a1 |-> c1, a1 |-> c2, a2 |-> c2, a3 |-> c1, a3 |-> c2 *)
                       
  val rel_cmp = ACRel.compose (rel2,rel)
  
  val _ = test "A composed relation" (fn () => (
      assert "should have domain size 3" (ACRel.dom_size rel_cmp = 3);
      assert "should have codomain size 2" (ACRel.cod_size rel_cmp = 2);
      assert "should return correct image for a1"
        (C.NSet.eq (ACRel.img rel_cmp a1) (C.NSet.of_list [c1,c2]));
      assert "should return correct image for a2"
        (C.NSet.eq (ACRel.img rel_cmp a2) (C.NSet.of_list [c2]));
      assert "should return correct image for a3"
        (C.NSet.eq (ACRel.img rel_cmp a3) (C.NSet.of_list [c1,c2]));
      assert "should return empty image for a4"
        (C.NSet.eq (ACRel.img rel_cmp a4) (C.NSet.of_list []));
    ())) ()
  
  
  (* A renaming-like scenario. Suppose we take rel as above and rename its elements with: *)
  val rn_dom = AAInj.empty |> AAInj.add (a1,a5)
                           |> AAInj.add (a2,a2)
                           |> AAInj.add (a3,a1)
                           |> AAInj.add (a4,a3)
  val rn_cod = BBInj.empty |> BBInj.add (b1,b2)
                           |> BBInj.add (b2,b3)
                           |> BBInj.add (b3,b1)
  (* Note that renamings are performed all at once, so the codomain no longer needs to
     be fresh in the domain. *)
  
  
  (* The renamed relation of:
       rel  :: a1 |-> b1, a1 |-> b2, a2 |-> b2, a3 |-> b1, a4 |-> b3
    
     is computed as (rn_cod o rel o rn_dom^-1). It should be:
       rel' :: a5 |-> b2, a5 |-> b3, a2 |-> b3, a1 |-> b2, a3 |-> b1 *)
  
  val rel' = ABRel.compose3 (rn_cod, rel, AAInj.inverse_of rn_dom)
  
  val _ = test "A renamed relation" (fn () => (
      assert "should have domain size 4" (ABRel.dom_size rel' = 4);
      assert "should have codomain size 3" (ABRel.cod_size rel' = 3);
      assert "should return correct image for a5"
        (B.NSet.eq (ABRel.img rel' a5) (B.NSet.of_list [b2,b3]));
      assert "should return correct image for a2"
        (B.NSet.eq (ABRel.img rel' a2) (B.NSet.of_list [b3]));
      assert "should return correct image for a1"
        (B.NSet.eq (ABRel.img rel' a1) (B.NSet.of_list [b2]));
      assert "should return correct image for a3"
        (B.NSet.eq (ABRel.img rel' a3) (B.NSet.of_list [b1]));
    ())) ()
 
end

local
  open TestMapsUtil
  structure Test_ABRel =
    TestNameRelation(
      structure ABRel = ABRel
      structure BCRel = BCRel
      structure ACRel = ACRel)
in val _ = assert_no_failed_tests ()
end
