local
  open TestMapsUtil
  
  
  val tab : int ATab.T = ATab.empty
  val _ = test "An empty table" (fn () => (
      assert "should have size 0" (ATab.size tab = 0);
      assert "should report true for is_empy" (ATab.is_empty tab);
      assert "should return NONE for get_opt" (ATab.get_opt tab a1 = NONE);
    ())) ()
  
  val tab = tab |> ATab.set (a1, 42)
  
  val _ = test "A table with one element" (fn () => (
      assert "should have size 1" (ATab.size tab = 1);
      assert "should return correct element" (ATab.get tab a1 = 42);
      assert "should return SOME (elem) for get_opt" (ATab.get_opt tab a1 = SOME 42);
      assert "should fail on duplicate_exp for duplicate add"
        (catch_dup_fail (fn () => tab |> ATab.add (a1,100)));
    ())) ()
  
  val tab = tab |> ATab.set (a1, 43)
  
  val _ = test "A table after override" (fn () => (
      assert "should still have size 1" (ATab.size tab = 1);
      assert "should return new element" (ATab.get tab a1 = 43);
    ())) ()
  
  val (a',tab) = tab |> ATab.store 44
  val tab = tab |> ATab.store_anon 45
  
  val _ = test "A table after storage" (fn () => (
      assert "should have size 3" (ATab.size tab = 3);
      assert "should return correct element" (ATab.get tab a' = 44);
    ())) ()
  
  val tab = tab |> ATab.delete a1
  
  val _ = test "A table after deletion" (fn () => (
      assert "should have size 2" (ATab.size tab = 2);
      assert "should return NONE for deleted element" (ATab.get_opt tab a1 = NONE);
    ())) ()
  
  fun di x = case A.dest x of "a1" => [1] | "a2" => [2] | _ => []
  fun bad_di x = case A.dest x of "a1" => [1] | "a2" => [2,3] | _ => []
  fun dom () = [a1,a2]
  val abs_tab = Map.mk (Map.mk_graph (dom,di), I)
  val bad_abs_tab = Map.mk (Map.mk_graph (dom,bad_di), I)
  val coerced_tab = ATab.coerce abs_tab
  
  val _ = test "A coerced table" (fn () => (
      assert "should have size 2" (ATab.size coerced_tab = 2);
      assert "should contain the correct values" (
        (ATab.get coerced_tab a1 = 1) andalso
        (ATab.get coerced_tab a2 = 2));
    ())) ()
  
  val _ = test "A bad coercion" (fn () => (
      assert "should raise bad_coercion_exp for multi-valued graph"
        (catch_coerce_fail (fn () => ATab.coerce bad_abs_tab));
    ())) ()
  
  (*
  Keys in a table can be renamed by pre-composition with an injective map.
  
  Consider a table, and an injective function:
    tab :: a1 |-> 1, a2 |-> 2, a3 |-> 3
    rn  :: a1 |-> a2, a2 |-> a1, a3 |-> a4
  
  Then, we can rename the keys using 'rn' with (tab o rn^-1). This yields:
    tab' :: a2 |-> 1, a1 |-> 2, a4 |-> 3
  *)

  val tab = ATab.empty |> ATab.add (a1,1) |> ATab.add (a2,2) |> ATab.add (a3,3)
  val rn = AAInj.empty |> AAInj.add (a1,a2) |> AAInj.add (a2,a1) |> AAInj.add (a3,a4)
  val tab' = ATab.compose (tab, AAInj.inverse_of rn)
  
  val _ = test "A renamed table" (fn () => (
      assert "should have size 3" (ATab.size tab' = 3);
      assert "should return correct elements"
        (ATab.get tab' a2 = 1 andalso ATab.get tab' a1 = 2 andalso ATab.get tab' a4 = 3);
    ())) ()
  
in
  val _ = assert_no_failed_tests ()
end
