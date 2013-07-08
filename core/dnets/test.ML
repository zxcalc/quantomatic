local
  structure Tools = Test_Bang_Graph_Tools(Test_Bang_Graph);
  open Tools;

  
  structure TD = Top_DNet(G)
  structure CL = TD.CL
  structure C = CL.C
  structure L = C.L


  val b = L.boundary;
  val k1 = (vexpr1 "0")
  val k2 = (vexpr1 "1")
  val k3 = (vexpr1 "a")



(******************)
(* BUILDING TESTS *)
(******************)
  
  (* Building the graphs *)
  val pattern1 = G.empty
  
  val p1_v1 = V.mk "p1_v1"
  val p1_v2 = V.mk "p1_v2"
  val p1_v3 = V.mk "p1_v3"
  val p1_v4 = V.mk "p1_v4"
  val p1_v5 = V.mk "p1_v5"

  val p1_e1 = E.mk "p1_e1"
  val p1_e2 = E.mk "p1_e2"
  val p1_e3 = E.mk "p1_e3"
  val p1_e4 = E.mk "p1_e4"
  
  val pattern1 = pattern1 |> G.add_named_vertex p1_v1 k1
                          |> G.add_named_vertex p1_v2 k2
                          |> G.add_named_vertex p1_v3 k2
                          |> G.add_named_vertex p1_v4 k1
                          |> G.add_named_vertex p1_v5 k2
                          |> G.add_named_edge p1_e1 (Directed,eunit1) p1_v1 p1_v2
                          |> G.add_named_edge p1_e2 (Directed,eunit1) p1_v1 p1_v3
                          |> G.add_named_edge p1_e3 (Directed,eunit1) p1_v2 p1_v3
                          |> G.add_named_edge p1_e4 (Directed,eunit1) p1_v5 p1_v4 

  val (bb1,pattern1) = pattern1 |> G.add_bbox
  val pattern1 = pattern1 |> G.add_to_bbox_anon bb1 (V.NSet.single p1_v5)
  


  val pattern2 = G.empty
  
  val p2_v1 = V.mk "p2_v1"
  val p2_v2 = V.mk "p2_v2"
  val p2_v3 = V.mk "p2_v3"
  val p2_v4 = V.mk "p2_v4"

  val p2_e1 = E.mk "p2_e1"
  val p2_e2 = E.mk "p2_e2"
  val p2_e3 = E.mk "p2_e3"
  val p2_e4 = E.mk "p2_e4"
  
  val pattern2 = pattern2 |> G.add_named_vertex p2_v1 k2
                          |> G.add_named_vertex p2_v2 k2
                          |> G.add_named_vertex p2_v3 k2
                          |> G.add_named_vertex p2_v4 k1
                          |> G.add_named_edge p2_e1 (Directed,eunit1) p2_v1 p2_v2
                          |> G.add_named_edge p2_e2 (Directed,eunit1) p2_v1 p2_v3
                          |> G.add_named_edge p2_e3 (Directed,eunit1) p2_v1 p2_v4
                          |> G.add_named_edge p2_e4 (Directed,eunit1) p2_v4 p2_v3

  val (bb2,pattern2) = pattern2 |> G.add_bbox
  val (bb3,pattern2) = pattern2 |> G.add_bbox
  val pattern2 = pattern2 |> G.add_to_bbox_anon bb2 (V.NSet.single p2_v1)
                          |> G.add_to_bbox_anon bb3 (V.NSet.single p2_v4)
  

  val pattern3 = G.empty
  
  val p3_v1 = V.mk "p3_v1"
  val p3_v2 = V.mk "p3_v2"
  val p3_v3 = V.mk "p3_v3"
  val p3_v4 = V.mk "p3_v4"

  val p3_e1 = E.mk "p3_e1"
  val p3_e2 = E.mk "p3_e2"
  val p3_e3 = E.mk "p3_e3"
  val p3_e4 = E.mk "p3_e4"
  
  val pattern3 = pattern3 |> G.add_named_vertex p3_v1 k1
                          |> G.add_named_vertex p3_v2 k1
                          |> G.add_named_vertex p3_v3 k2
                          |> G.add_named_vertex p3_v4 k2
                          |> G.add_named_edge p3_e1 (Directed,eunit1) p3_v1 p3_v2
                          |> G.add_named_edge p3_e2 (Directed,eunit1) p3_v1 p3_v3
                          |> G.add_named_edge p3_e3 (Directed,eunit1) p3_v2 p3_v3
                          |> G.add_named_edge p3_e4 (Directed,eunit1) p3_v3 p3_v4


  val pattern4 = G.empty
  
  val p4_v1 = V.mk "p4_v1"
  val p4_v2 = V.mk "p4_v2"
  val p4_v3 = V.mk "p4_v3"
  val p4_v4 = V.mk "p4_v4"

  val p4_e1 = E.mk "l1"
  val p4_e2 = E.mk "l2"
  val p4_e3 = E.mk "l3"
  val p4_e4 = E.mk "l4"
  
  val pattern4 = pattern4 |> G.add_named_vertex p4_v1 k1
                          |> G.add_named_vertex p4_v2 k1
                          |> G.add_named_vertex p4_v3 k2
                          |> G.add_named_vertex p4_v4 b
                          |> G.add_named_edge p4_e1 (Directed,eunit1) p4_v1 p4_v2
                          |> G.add_named_edge p4_e2 (Directed,eunit1) p4_v1 p4_v3
                          |> G.add_named_edge p4_e3 (Directed,eunit1) p4_v2 p4_v3
                          |> G.add_named_edge p4_e4 (Directed,eunit1) p4_v3 p4_v4



  val pattern5 = G.empty
  
  val p5_v1 = V.mk "p5_v1"
  val p5_v2 = V.mk "p5_v2"
  val p5_v3 = V.mk "p5_v3"
  val p5_v4 = V.mk "p5_v4"

  val p5_e1 = E.mk "p5_e1"
  val p5_e2 = E.mk "p5_e2"
  val p5_e3 = E.mk "p5_e3"
  val p5_e4 = E.mk "p5_e4"
  
  val pattern5 = pattern5 |> G.add_named_vertex p5_v1 k2
                          |> G.add_named_vertex p5_v2 b
                          |> G.add_named_vertex p5_v3 b
                          |> G.add_named_vertex p5_v4 k1
                          |> G.add_named_edge p5_e1 (Directed,eunit1) p5_v1 p5_v2
                          |> G.add_named_edge p5_e2 (Directed,eunit1) p5_v3 p5_v1
                          |> G.add_named_edge p5_e3 (Directed,eunit1) p5_v4 p5_v1


  val pattern6 = G.empty
  
  val p6_v1 = V.mk "p6_v1"
  val p6_v2 = V.mk "p6_v2"
  val p6_v3 = V.mk "p6_v3"
  val p6_v4 = V.mk "p6_v4"

  val p6_e1 = E.mk "p6_e1"
  val p6_e2 = E.mk "p6_e2"
  val p6_e3 = E.mk "p6_e3"
  val p6_e4 = E.mk "p6_e4"
  
  val pattern6 = pattern6 |> G.add_named_vertex p6_v1 k2
                          |> G.add_named_vertex p6_v2 b
                          |> G.add_named_vertex p6_v3 k1
                          |> G.add_named_vertex p6_v4 k1
                          |> G.add_named_edge p6_e1 (Directed,eunit1) p6_v1 p6_v2
                          |> G.add_named_edge p6_e2 (Directed,eunit1) p6_v1 p6_v3
                          |> G.add_named_edge p6_e3 (Directed,eunit1) p6_v3 p6_v4




  val pattern7 = G.empty
  
  val p7_v1 = V.mk "p7_v1"
  val p7_v2 = V.mk "p7_v2"
  val p7_v3 = V.mk "p7_v3"
  val p7_v4 = V.mk "p7_v4"

  val p7_e1 = E.mk "p7_e1"
  val p7_e2 = E.mk "p7_e2"
  val p7_e3 = E.mk "p7_e3"
  val p7_e4 = E.mk "p7_e4"
  val p7_e5 = E.mk "p7_e5"
  
  val pattern7 = pattern7 |> G.add_named_vertex p7_v1 k1
                          |> G.add_named_vertex p7_v2 k1
                          |> G.add_named_vertex p7_v3 k1
                          |> G.add_named_vertex p7_v4 k1
                          |> G.add_named_edge p7_e1 (Directed,eunit1) p7_v1 p7_v2
                          |> G.add_named_edge p7_e2 (Directed,eunit1) p7_v2 p7_v1
                          |> G.add_named_edge p7_e3 (Directed,eunit1) p7_v1 p7_v3
                          |> G.add_named_edge p7_e4 (Directed,eunit1) p7_v2 p7_v4
                          |> G.add_named_edge p7_e5 (Directed,eunit1) p7_v3 p7_v4



  (* Building the contour lists *) 
  val cl1 = CL.mk pattern1 
  val c1_1 = nth cl1 0
  val c1_2 = nth cl1 1  
  val lit1_1 = nth c1_1 0
  val lit1_2 = nth c1_2 0 
  val lit1_3 = nth c1_2 1 
  val lit1_4 = L.mk pattern1 p1_v4
  val lit1_5 = L.mk pattern1 p1_v5


  val cl2 = CL.mk pattern2
  val c2_1 = nth cl2 0
  val c2_2 = nth cl2 1
  val c2_3 = nth cl2 2
  val lit2_1 = nth c2_2 0
  val lit2_2 = nth c2_1 0
  val lit2_3 = nth c2_3 0
  val lit2_4 = nth c2_3 1

 
  val cl3 = CL.mk pattern3
  val c3_1 = nth cl3 0
  val c3_2 = nth cl3 1
  val c3_3 = nth cl3 2
  val lit3_1 = nth c3_1 0
  val lit3_2 = nth c3_2 0
  val lit3_3 = nth c3_2 1
  val lit3_4 = nth c3_3 0


  val cl4 = CL.mk pattern4
  val c4_1 = nth cl4 0
  val c4_2 = nth cl4 1
  val c4_3 = nth cl4 2
  val lit4_1 = nth c4_1 0
  val lit4_2 = nth c4_2 0
  val lit4_3 = nth c4_2 1
  val lit4_4 = nth c4_3 0


  val cl5 = CL.mk pattern5
  val c5_1 = nth cl5 0
  val c5_2 = nth cl5 1
  val lit5_1 = nth c5_1 0
  val lit5_2 = nth c5_2 0
  val lit5_3 = nth c5_2 1
  val lit5_4 = nth c5_2 2


  val cl6 = CL.mk pattern6
  val c6_1 = nth cl6 0
  val c6_2 = nth cl6 1
  val c6_3 = nth cl6 2
  val lit6_1 = nth c6_1 0
  val lit6_2 = nth c6_2 0
  val lit6_3 = nth c6_2 1
  val lit6_4 = nth c6_3 0


  val cl7 = CL.mk pattern7
  val c7_1 = nth cl7 0
  val c7_2 = nth cl7 1
  val c7_3 = nth cl7 2
  val lit7_1 = nth c7_1 0
  val lit7_2 = nth c7_2 0
  val lit7_3 = nth c7_2 1
  val lit7_4 = nth c7_3 0

  
  (* TESTING BASIC OPERATIONS *)

  (* Assertion functions *)
  fun assert_subtraction(c1,c2,c3) =
    if C.eq(C.subtract_eq_contour(c1,c2),c3) then true
    else raise ERROR ("Subtraction is wrong: expected " ^ C.printout(c3) ^ " -- computed: " ^ C.printout(C.subtract_eq_contour(c1,c2)))

  fun assert_intersection(c1,c2,c3) =
    if C.eq(C.intersect_eq_contours(c1,c2),c3) then true
    else raise ERROR ("Intersection is wrong: expected " ^ C.printout(c3) ^ " -- computed: " ^ C.printout(C.intersect_eq_contours(c1,c2)))

  fun assert_complement(c1,c2,c3) =
    if C.eq(C.complement_eq_contour(c1,c2),c3) then true
    else raise ERROR ("Complement is wrong: expected " ^ C.printout(c3) ^ " -- computed: " ^ C.printout(C.complement_eq_contour(c1,c2)))

  fun assert_duplicate_removal(c1,c2) =
    if C.eq(C.remove_eq_duplicate(c1),c2) then true
    else raise ERROR ("Duplicate removal is wrong: expected " ^ C.printout(c2) ^ " -- computed: " ^ C.printout(C.remove_eq_duplicate(c1)))

  (* Tests *)
  val lit_a = L.build((V.mk "a"), k1, L.mult_none, 1, L.mult_none, 1, L.mult_none)
  val lit_b = L.build((V.mk "b"), k2, L.mult_none, 1, L.mult_none, 0, L.mult_none)
  val lit_c = L.build((V.mk "c"), k1, L.mult_star, 0, L.mult_none, 1, L.mult_none)
  val lit_d =  L.build((V.mk "d"), k1, L.mult_none, 1, L.mult_none, 1, L.mult_star)
  val lit_e = L.build((V.mk "e"), G.WVert, L.mult_none, 1, L.mult_none, 0, L.mult_none)
  val lit_f = L.build((V.mk "f"), k2, L.mult_star, 1, L.mult_none, 0, L.mult_none)

  val cont_a = lit_b :: lit_a :: []
  val cont_b = lit_b :: lit_c :: []
  val cont_c = lit_c :: []
  val cont_d = lit_a :: lit_e :: lit_b :: lit_f :: []
  
  val sub_d_a = lit_e :: lit_f :: []
  val sub_b_c = lit_b :: []
  val sub_a_a = []
  val _ = Testing.test "Checking subtraction.." assert_subtraction (cont_d,cont_a,sub_d_a) 
  val _ = Testing.test "Checking subtraction.." assert_subtraction (cont_b,cont_c,sub_b_c) 
  val _ = Testing.test "Checking subtraction.." assert_subtraction (cont_a,cont_a,sub_a_a)
  
  val inters_a_b = lit_b :: []
  val inters_a_c = []
  val inters_b_c = lit_c :: []
  val inters_a_d = lit_b :: lit_a :: []
  val _ = Testing.test "Checking intersection.." assert_intersection (cont_a,cont_b,inters_a_b) 
  val _ = Testing.test "Checking intersection.." assert_intersection (cont_a,cont_c,inters_a_c) 
  val _ = Testing.test "Checking intersection.." assert_intersection (cont_b,cont_c,inters_b_c)
  val _ = Testing.test "Checking intersection.." assert_intersection (cont_a,cont_d,inters_a_d)

  val compl_a_b = lit_c :: []
  val compl_a_c = lit_c :: []
  val compl_b_c = []
  val compl_a_d = lit_e :: lit_f :: []
  val compl_a_d2 = lit_f :: lit_e :: []
  val _ = Testing.test "Checking complement.." assert_complement (cont_a,cont_b,compl_a_b) 
  val _ = Testing.test "Checking complement.." assert_complement (cont_a,cont_c,compl_a_c) 
  val _ = Testing.test "Checking complement.." assert_complement (cont_b,cont_c,compl_b_c)
  val _ = Testing.test "Checking complement.." assert_complement (cont_a,cont_d,compl_a_d)
  val _ = Testing.test "Checking complement.." assert_complement (cont_a,cont_d,compl_a_d2)

  val cont_aa = lit_b :: lit_a :: lit_a :: lit_a :: lit_b :: []
  val cont_bb = lit_c :: lit_b :: []
  val cont_cc = lit_c :: lit_c :: [] 
  val _ = Testing.test "Checking duplicate removal.." assert_duplicate_removal (cont_aa,cont_a)
  val _ = Testing.test "Checking duplicate removal.." assert_duplicate_removal (cont_bb,cont_b)
  val _ = Testing.test "Checking duplicate removal.." assert_duplicate_removal (cont_cc,cont_c)




  (* TESTING LITERALS *)

  (* Assertion functions *)
  fun assert_name_eq(a:V.name,b:V.name) =
    if V.name_eq(a,b) then true
    else raise ERROR ("Literal names do not match: " ^ V.string_of_name(a) ^ " -- " ^ V.string_of_name(b))

  fun assert_kind_eq(a:G.vdata,b:G.vdata) =
    if G.vdata_eq(a,b) then true
    else raise ERROR ("Literal kinds do not match!")

  fun assert_mult_eq(a:L.multiplicity,b:L.multiplicity,s) =
    if L.mult_eq(a,b) then true
    else raise ERROR ("Literal " ^ s ^ "do not match!")

  fun assert_int_eq(a,b,s) =
    if (a=b) then true
    else raise ERROR ("Literal " ^ s ^ "do not match!")

  fun assert_lit_eq(l1,l2) =
    if L.eq(l1,l2) then true
    else raise ERROR ("Literals are not equal")

  fun assert_lit_equiv(l1,l2) =
    if L.equiv(l1,l2) then true
    else raise ERROR ("Literals are not equivalent")

  fun assert_lit_match(l1,l2) =
    if L.match(l1,l2) then true
    else raise ERROR ("Literals do not match")

  (* Tests *)
  val name1 = L.get_name lit1_1
  val name2 = L.get_name lit1_2
  val _ = Testing.test "Checking literal name.." assert_name_eq (name1,p1_v1)
  val _ = Testing.test "Checking literal name.." assert_name_eq (name2,p1_v2)
    
  val kind1 = L.get_kind lit1_1
  val kind2 = L.get_kind lit1_2
  val _ = Testing.test "Checking literal kind.." assert_kind_eq (kind1,k1)
  val _ = Testing.test "Checking literal kind.." assert_kind_eq (kind2,k2)

  val k_m1 = L.get_kind_mult lit1_1
  val k_m2 = L.get_kind_mult lit1_2
  val _ = Testing.test "Checking literal kind-mult.." assert_mult_eq (k_m1,L.mult_none,"kind-mult")
  val _ = Testing.test "Checking literal kind-mult.." assert_mult_eq (k_m2,L.mult_none,"kind-mult")

  val i_a1 = L.get_input_arity lit1_1
  val i_a2 = L.get_input_arity lit1_2
  val _ = Testing.test "Checking literal input-arity.." assert_int_eq (i_a1,0,"input-arity")
  val _ = Testing.test "Checking literal input-arity.." assert_int_eq (i_a2,1,"input-arity")
  
  val i_m1 = L.get_input_mult lit1_1
  val i_m2 = L.get_input_mult lit1_2
  val _ = Testing.test "Checking literal input-mult.." assert_mult_eq (i_m1,L.mult_none,"input-mult")
  val _ = Testing.test "Checking literal input-mult.." assert_mult_eq (i_m2,L.mult_none,"input-mult")

  val o_a1 = L.get_output_arity lit1_1
  val o_a2 = L.get_output_arity lit1_2
  val _ = Testing.test "Checking literal output-arity.." assert_int_eq (o_a1,2,"output-arity")
  val _ = Testing.test "Checking literal output-arity.." assert_int_eq (o_a2,1,"output-arity")

  val o_m1 = L.get_output_mult lit1_1
  val o_m2 = L.get_output_mult lit1_2
  val _ = Testing.test "Checking literal output-mult.." assert_mult_eq (o_m1,L.mult_none,"output-mult")
  val _ = Testing.test "Checking literal output-mult.." assert_mult_eq (o_m2,L.mult_none,"output-mult")

  val _ = Testing.test "Checking literal equality.." assert_lit_eq (lit2_1, L.build ( (V.mk "p2_v1"), k2, L.mult_star, 0, L.mult_none, 3, L.mult_star) )
  val _ = Testing.test "Checking literal equality.." assert_lit_eq (lit2_2, L.build ( (V.mk "p2_v2"), k2, L.mult_none, 1, L.mult_star, 0, L.mult_none) )
  val _ = Testing.test "Checking literal equality.." assert_lit_eq (lit2_3, L.build ( (V.mk "p2_v3"), k2, L.mult_qm, 2, L.mult_star, 0, L.mult_none) )
  val _ = Testing.test "Checking literal equality.." assert_lit_eq (lit2_4, L.build ( (V.mk "p2_v4"), k1, L.mult_star, 1, L.mult_star, 1, L.mult_none) )

  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit2_1, L.build ( (V.mk "p2_v1"), k2, L.mult_star, 0, L.mult_none, 2, L.mult_star) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit2_2, L.build ( (V.mk "p2_v2"), k2, L.mult_none, 0, L.mult_star, 0, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit2_3, L.build ( (V.mk "p2_v3"), k2, L.mult_qm, 0, L.mult_star, 0, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit2_4, L.build ( (V.mk "p2_v4"), k1, L.mult_star, 0, L.mult_star, 1, L.mult_none) )

  val _ = Testing.test "Checking literal equality.." assert_lit_eq (lit4_1, L.build ( (V.mk "p4_v1"), k2, L.mult_none, 0, L.mult_none, 3, L.mult_none) )
  val _ = Testing.test "Checking literal equality.." assert_lit_eq (lit4_2, L.build ( (V.mk "p4_v2"), k2, L.mult_none, 1, L.mult_none, 0, L.mult_none) )
  val _ = Testing.test "Checking literal equality.." assert_lit_eq (lit4_3, L.build ( (V.mk "p4_v3"), k1, L.mult_none, 2, L.mult_none, 0, L.mult_none) )
  val _ = Testing.test "Checking literal equality.." assert_lit_eq (lit4_4, L.build ( (V.mk "p4_v4"), b, L.mult_none, 1, L.mult_none, 1, L.mult_none) )

  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit4_1, L.build ( (V.mk "p4_v1"), k1, L.mult_none, 0, L.mult_none, 2, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit4_2, L.build ( (V.mk "p4_v2"), k1, L.mult_none, 1, L.mult_none, 1, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit4_3, L.build ( (V.mk "p4_v3"), k2, L.mult_none, 2, L.mult_none, 1, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit4_4, L.build ( (V.mk "p4_v4"), b, L.mult_none, 1, L.mult_none, 0, L.mult_none) )

  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit5_1, L.build ( (V.mk "p5_v1"), k2, L.mult_none, 2, L.mult_none, 1, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit5_2, L.build ( (V.mk "p5_v2"), b, L.mult_star, 1, L.mult_none, 0, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit5_3, L.build ( (V.mk "p5_v3"), b, L.mult_star, 0, L.mult_none, 1, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit5_4, L.build ( (V.mk "p5_v4"), k1, L.mult_none, 0, L.mult_none, 1, L.mult_none) )

  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit6_1, L.build ( (V.mk "p6_v1"), k2, L.mult_none, 0, L.mult_none, 2, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit6_2, L.build ( (V.mk "p6_v2"), b, L.mult_none, 1, L.mult_none, 0, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit6_3, L.build ( (V.mk "p6_v3"), k1, L.mult_none, 1, L.mult_none, 1, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit6_4, L.build ( (V.mk "p6_v4"), k1, L.mult_none, 1, L.mult_none, 0, L.mult_none) )

  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit7_1, L.build ( (V.mk "p7_v1"), k1, L.mult_none, 1, L.mult_none, 2, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit7_2, L.build ( (V.mk "p7_v2"), k1, L.mult_none, 1, L.mult_none, 2, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit7_3, L.build ( (V.mk "p7_v3"), k1, L.mult_none, 1, L.mult_none, 1, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit7_4, L.build ( (V.mk "p7_v4"), k1, L.mult_none, 2, L.mult_none, 0, L.mult_none) )
  

  (* TESTING CONTOURS *)

  (* Assertion functions *)
  fun assert_contour_length(i,c) = 
    if (length c = i) then true 
    else raise ERROR ("Error in contour length " ^ CL.printout(c)) 

  fun is_one(x,[]) = false
    | is_one(x,c::[]) = if C.equiv(x,c) then true else false
    | is_one(x,c::cs) = if C.equiv(x,c) then true else is_one(x,cs)
     
  fun assert_tgt_fn(c,cc) =
    if (is_one(c,cc)) then true
    else raise ERROR ("Error in target function")

  fun assert_contour_length(i,c) = 
    if (length c = i) then true 
    else raise ERROR ("Error in contour length")

  fun assert_is_eq_contained(l,c) = 
    if (C.is_eq_literal_contained(l,c)) then true
    else raise ERROR ("Error in the elements of the contour")
  
  fun assert_is_eq_not_contained(l,c) = 
    if (C.is_eq_literal_contained(l,c)) then raise ERROR ("Error in the elements of the contour")
    else true
  
  fun assert_contour_eq(c1,c2) = 
    if C.eq(c1,c2) then true 
    else raise ERROR ("Contours are not equal")

  fun assert_contour_equiv(c1,c2) = 
    if C.equiv(c1,c2) then true 
    else raise ERROR ("Contours are not equivalent")

  fun assert_matching_contour_containing(c1,c2) = 
    if C.is_matching_contour_contained(c1,c2) then true 
    else raise ERROR ("Error in contour list")

  fun assert_strong_compatibility(c1,c2) = 
    if C.check_strong_compatibility(c1,c2) then true 
    else raise ERROR ("Contours are not strongly compatible")  

  fun assert_no_strong_compatibility(c1,c2) = 
    if C.check_strong_compatibility(c1,c2) then raise ERROR ("Contours are strongly compatible") 
    else true
  
  fun assert_weak_compatibility(c1,c2) = 
    if C.check_weak_compatibility(c1,c2) then true 
    else raise ERROR ("Contours are not weakly compatible")  

  fun assert_no_weak_compatibility(c1,c2) = 
    if C.check_weak_compatibility(c1,c2) then raise ERROR ("Contours are weakly compatible") 
    else true


  (* Tests *)
  val e = C.empty
  val trg1 = C.target_function pattern1  
  val ok1 = C.add_literal e lit1_1
  val ok2 = C.add_literal e lit1_2
  val ok3 = C.add_literal e lit1_3
  val ok4 = C.add_literal e lit1_4
  val ok1list = ok1::ok2::ok3::ok4::[] 

  val trg2 = C.target_function pattern2
  val ok2 = C.add_literal e lit2_2
  val ok3 = C.add_literal e lit2_3
  val ok2list = ok2::ok3::[]

  val _ = Testing.test "Checking target function.." assert_tgt_fn (trg1,ok1list)
  val _ = Testing.test "Checking target function.." assert_tgt_fn (trg2,ok2list) 


  val c1 = C.mk pattern1 trg1
  val _ = Testing.test "Checking number of literals.." assert_contour_length (2,c1)
  val _ = Testing.test "Checking element in contour.." assert_is_eq_contained (lit1_2,c1)
  val _ = Testing.test "Checking element in contour.." assert_is_eq_contained (lit1_3,c1)
  val _ = Testing.test "Checking element in contour.." assert_is_eq_not_contained (lit1_4,c1)
  val _ = Testing.test "Checking element in contour.." assert_is_eq_not_contained (lit1_5,c1)

  val c2 = C.mk pattern1 (lit1_2::lit1_3::[])
  val _ = Testing.test "Checking number of literals.." assert_contour_length (1,c2)


  val lit_1 = L.build((V.mk "a"), k1, L.mult_none, 1, L.mult_none, 1, L.mult_none)
  val lit_2 = L.build((V.mk "b"), k2, L.mult_none, 1, L.mult_none, 0, L.mult_none)
  val target = lit_1 :: lit_2 :: lit_2 :: []

  val lit_3 = L.build((V.mk "c"), k1, L.mult_star, 0, L.mult_none, 1, L.mult_none)
  val pattern_1 = lit_1 :: lit_2 :: lit_3 :: lit_2 :: []
  val lit_4 =  L.build((V.mk "d"), k1, L.mult_none, 1, L.mult_none, 1, L.mult_star)
  val pattern_2 = lit_2 :: lit_4 :: lit_2 :: []
  val lit_5 = L.build((V.mk "e"), G.WVert, L.mult_none, 1, L.mult_none, 0, L.mult_none)
  val lit_6 = L.build((V.mk "f"), k2, L.mult_star, 1, L.mult_none, 0, L.mult_none)
  val pattern_3 = lit_5 :: lit_6 :: []

  val pattern_2_concrete = C.get_contour_mult_none(pattern_2)
  val pattern_2_abstract = C.get_contour_mult_star_or_qm(pattern_2)
  val pattern_3_concrete = C.get_contour_mult_none(pattern_3)
  val pattern_3_abstract = C.get_contour_mult_star_or_qm(pattern_3)
  val _ = Testing.test "Checking abstract and concrete contour.." assert_contour_equiv(pattern_2_concrete, pattern_2)
  val _ = Testing.test "Checking abstract and concrete contour.." assert_contour_equiv(pattern_2_abstract, C.empty)
  val _ = Testing.test "Checking abstract and concrete contour.." assert_contour_equiv(pattern_3_concrete, (lit_5::[]))
  val _ = Testing.test "Checking abstract and concrete contour.." assert_contour_equiv(pattern_3_abstract, (lit_6::[]))
 
  val _ = Testing.test "Checking containment.." assert_matching_contour_containing(pattern_2_concrete,target)
  val _ = Testing.test "Checking containment.." assert_matching_contour_containing((lit_1 :: lit_2 :: lit_2 :: []),target)

  val _ = Testing.test "Checking strong compatibility.." assert_strong_compatibility(target,pattern_1)
  val _ = Testing.test "Checking strong compatibility.." assert_strong_compatibility(target,pattern_2)
  val _ = Testing.test "Checking strong compatibility.." assert_strong_compatibility(target,pattern_3)

  val lit_7 = L.build((V.mk "g"), k2, L.mult_qm, 1, L.mult_none, 0, L.mult_none)
  val pattern_4 = lit_5 :: lit_7 :: []
  val lit_8 = L.build((V.mk "h"), k2, L.mult_none, 1, L.mult_star, 0, L.mult_none)
  val pattern_5 = lit_8 :: []
  val _ = Testing.test "Checking strong compatibility.." assert_no_strong_compatibility(target,pattern_4)
  val _ = Testing.test "Checking strong compatibility.." assert_no_strong_compatibility(target,pattern_5)
  val _ = Testing.test "Checking weak compatibility.." assert_weak_compatibility(target,pattern_4)
  val _ = Testing.test "Checking weak compatibility.." assert_weak_compatibility(target,pattern_5)

  val lit_9 = L.build((V.mk "j"), k2, L.mult_qm, 1, L.mult_none, 1, L.mult_none)
  val pattern_6 = lit_1 :: lit_1 :: lit_9 :: lit_6 :: []
  val pattern_7 = lit_1 :: lit_2 :: lit_8 :: lit_5 :: []
  val lit_10 = L.build((V.mk "k"), k1, L.mult_star, 1, L.mult_none, 1, L.mult_none)
  val lit_11 = L.build((V.mk "l"), k2, L.mult_none, 1, L.mult_none, 1, L.mult_none)
  val lit_12 = L.build((V.mk "m"), k2, L.mult_none, 2, L.mult_none, 1, L.mult_none)
  val pattern_8 = lit_10 :: lit_11 :: lit_12 :: []
  val lit_13 = L.build((V.mk "n"), k1, L.mult_qm, 1, L.mult_none, 0, L.mult_none)
  val pattern_9 = lit_13 :: lit_11 :: []
  val _ = Testing.test "Checking strong compatibility.." assert_no_strong_compatibility(target,pattern_6)
  val _ = Testing.test "Checking strong compatibility.." assert_no_strong_compatibility(target,pattern_7)
  val _ = Testing.test "Checking strong compatibility.." assert_no_strong_compatibility(target,pattern_8)
  val _ = Testing.test "Checking strong compatibility.." assert_no_strong_compatibility(target,pattern_9)
  val _ = Testing.test "Checking weak compatibility.." assert_no_weak_compatibility(target,pattern_6)
  val _ = Testing.test "Checking weak compatibility.." assert_no_weak_compatibility(target,pattern_7)
  val _ = Testing.test "Checking weak compatibility.." assert_no_weak_compatibility(target,pattern_8)
  val _ = Testing.test "Checking weak compatibility.." assert_no_weak_compatibility(target,pattern_9)

   

  (* TESTING CONTOUR LISTS *)

  (* Assertion functions *)
  fun assert_contour_list_length(i,cl) = 
    if (length cl = i) then true 
    else raise ERROR ("Error in contour list length") 

  fun assert_contour_list_equiv(cl1,cl2) = 
    if CL.equiv(cl1,cl2) then true
    else raise ERROR ("Error in contour list")


  (* Tests *)   

  val hand_cl1 = (lit1_1::[]) :: (lit1_2::lit1_3::[]) :: []
  val _ = Testing.test "Checking number of contours in contour list.." assert_contour_list_length (2,cl1) 
  val _ = Testing.test "Checking the contours.." assert_contour_list_equiv (hand_cl1,cl1)

  val hand_cl2_1 = (lit2_2::[]) :: (lit2_1::[]) :: (lit2_4::lit2_3::[]) :: []
  val hand_cl2_2 = (lit2_2::[]) :: (lit2_1::[]) :: (lit2_3::lit2_4::[]) :: []
  val _ = Testing.test "Checking number of contours in contour list.." assert_contour_list_length (3,cl2)  
  val _ = Testing.test "Checking the contours.." assert_contour_list_equiv (hand_cl2_1,cl2)
  val _ = Testing.test "Checking the contours.." assert_contour_list_equiv (hand_cl2_2,cl2)

  val hand_cl3 = (lit3_1::[]) :: (lit3_2::lit3_3::[]) :: (lit3_4::[]) :: []
  val _ = Testing.test "Checking number of contours in contour list.." assert_contour_list_length (3,cl3)
  val _ = Testing.test "Checking the contours.." assert_contour_list_equiv (hand_cl3,cl3)

  val hand_cl4 = (lit4_1::[]) :: (lit4_2::lit4_3::[]) :: (lit4_4::[]) :: []
  val _ = Testing.test "Checking number of contours in contour list.." assert_contour_list_length (3,cl4)
  val _ = Testing.test "Checking the contours.." assert_contour_list_equiv (hand_cl4,cl4)

  val hand_cl5 = (lit5_1::[]) :: (lit5_2::lit5_3::lit5_4::[]) :: []
  val _ = Testing.test "Checking number of contours in contour list.." assert_contour_list_length (2,cl5)
  val _ = Testing.test "Checking the contours.." assert_contour_list_equiv (hand_cl5,cl5)

  val hand_cl6 = (lit6_1::[]) :: (lit6_2::lit6_3::[]) :: (lit6_4::[]) :: []
  val _ = Testing.test "Checking number of contours in contour list.." assert_contour_list_length (3,cl6)
  val _ = Testing.test "Checking the contours.." assert_contour_list_equiv (hand_cl6,cl6)

  val hand_cl7 = (lit7_1::[]) :: (lit7_2::lit7_3::[]) :: (lit7_4::[]) :: []
  val _ = Testing.test "Checking number of contours in contour list.." assert_contour_list_length (3,cl7)
  val _ = Testing.test "Checking the contours.." assert_contour_list_equiv (hand_cl7,cl7)



  (* TESTING DNET *)

  (* Assertion functions *) 
  fun assert_graph_name_eq(n1,n2) = 
     if GraphName.name_eq(n1,n2) then true
     else raise ERROR ("Graph names do not match")

  fun assert_children_length(i,j) = 
    if i=j then true
    else raise ERROR ("Children number do not match")

  
  (* Tests *)
  val gn1 = GraphName.mk "pattern1"
  val gn2 = GraphName.mk "pattern2"
  val gn3 = GraphName.mk "pattern3"
  val gn4 = GraphName.mk "pattern4"
  val gn5 = GraphName.mk "pattern5"
  val gn6 = GraphName.mk "pattern6"
  val gn7 = GraphName.mk "pattern7"
  val gnl = gn1 :: gn2 :: gn3 :: gn4 :: gn5 :: gn6 :: gn7 :: []
  val gl = pattern1 :: pattern2 :: pattern3 :: pattern4 :: pattern5 :: pattern6 :: pattern7 :: []

  val tab = GraphName.NTab.empty
  val tab = TD.fold GraphName.NTab.doadd gnl gl tab

  val tree = TD.mk tab
 
  val level_1 = tree
  val node_1 = tree
  val contour_1_1 = TD.get_contour(level_1)
  val children_1_1 = TD.get_children(level_1)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_1_1,C.empty)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_1_1),5)

  val level_2 = children_1_1
  val node_2_1 = nth level_2 0
  val node_2_2 = nth level_2 1
  val node_2_3 = nth level_2 2
  val node_2_4 = nth level_2 3
  val node_2_5 = nth level_2 4
  val contour_2_1 = TD.get_contour(node_2_1)
  val contour_2_2 = TD.get_contour(node_2_2)
  val contour_2_3 = TD.get_contour(node_2_3)
  val contour_2_4 = TD.get_contour(node_2_4)
  val contour_2_5 = TD.get_contour(node_2_5)
  val children_2_1 = TD.get_children(node_2_1)
  val children_2_2 = TD.get_children(node_2_2)
  val children_2_3 = TD.get_children(node_2_3)
  val children_2_4 = TD.get_children(node_2_4)
  val children_2_5 = TD.get_children(node_2_5)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_2_1,(lit1_1::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_2_1,(lit3_1::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_2_1,(lit4_1::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_2_2,(lit2_2::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_2_3,(lit5_1::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_2_4,(lit6_1::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_2_5,(lit7_1::[]))
  val _ = Testing.test "Checking the children.." assert_children_length((length children_2_1),2)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_2_2),1)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_2_3),1)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_2_4),1)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_2_5),1)

  val level_3 = children_2_1 :: children_2_2 :: children_2_3 :: children_2_4 :: children_2_5 :: []
  val node_3_1 = nth children_2_1 0
  val node_3_2 = nth children_2_1 1
  val node_3_3 = nth children_2_2 0
  val node_3_4 = nth children_2_3 0
  val node_3_5 = nth children_2_4 0
  val node_3_6 = nth children_2_5 0
  val contour_3_1 = TD.get_contour(node_3_1)
  val contour_3_2 = TD.get_contour(node_3_2)
  val contour_3_3 = TD.get_contour(node_3_3)
  val contour_3_4 = TD.get_contour(node_3_4)
  val contour_3_5 = TD.get_contour(node_3_5)
  val contour_3_6 = TD.get_contour(node_3_6)
  val children_3_1 = TD.get_children(node_3_1)
  val children_3_2 = TD.get_children(node_3_2)
  val children_3_3 = TD.get_children(node_3_3)
  val children_3_4 = TD.get_children(node_3_4)
  val children_3_5 = TD.get_children(node_3_5)
  val children_3_6 = TD.get_children(node_3_6)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_3_1,(lit1_2::lit1_3::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_3_2,(lit3_2::lit3_3::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_3_3,(lit2_1::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_3_4,(lit5_2::lit5_3::lit5_4::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_3_5,(lit6_2::lit6_3::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_3_6,(lit7_2::lit7_3::[]))
  val _ = Testing.test "Checking the children.." assert_children_length((length children_3_1),1)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_3_2),2)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_3_3),1)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_3_4),1)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_3_5),1)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_3_6),1)

  val level_4 = children_3_1 :: children_3_2 :: children_3_3 :: children_3_4 :: children_3_5 :: children_3_6 :: []
  val node_4_1 = nth children_3_1 0
  val node_4_2 = nth children_3_2 0
  val node_4_3 = nth children_3_2 1
  val node_4_4 = nth children_3_3 0
  val node_4_5 = nth children_3_4 0
  val node_4_6 = nth children_3_5 0
  val node_4_7 = nth children_3_6 0
  val graph_4_1 = TD.get_graph(node_4_1)
  val contour_4_2 = TD.get_contour(node_4_2)
  val children_4_2 = TD.get_children(node_4_2)
  val contour_4_3 = TD.get_contour(node_4_3)
  val children_4_3 = TD.get_children(node_4_3)
  val contour_4_4 = TD.get_contour(node_4_4)
  val children_4_4 = TD.get_children(node_4_4)
  val graph_4_5 = TD.get_graph(node_4_5)
  val contour_4_6 = TD.get_contour(node_4_6)
  val children_4_6 = TD.get_children(node_4_6) 
  val contour_4_7 = TD.get_contour(node_4_7)
  val children_4_7 = TD.get_children(node_4_7)
  val _ = Testing.test "Checking the graph.." assert_graph_name_eq(graph_4_1, gn1)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_4_2,(lit3_4::[]))
  val _ = Testing.test "Checking the children.." assert_children_length((length children_4_2),1)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_4_3,(lit4_4::[]))
  val _ = Testing.test "Checking the children.." assert_children_length((length children_4_3),1)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_4_4,(lit2_4::lit2_3::[]))
  val _ = Testing.test "Checking the children.." assert_children_length((length children_4_4),1)
  val _ = Testing.test "Checking the graph.." assert_graph_name_eq(graph_4_5, gn5)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_4_6,(lit6_4::[]))
  val _ = Testing.test "Checking the children.." assert_children_length((length children_4_6),1)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_4_7,(lit7_4::[]))
  val _ = Testing.test "Checking the children.." assert_children_length((length children_4_7),1)
 
  val level_5 = children_4_2 :: children_4_3 :: children_4_4 :: children_4_6 :: children_4_7 :: []
  val node_5_1 = nth children_4_2 0
  val node_5_2 = nth children_4_3 0
  val node_5_3 = nth children_4_4 0
  val node_5_4 = nth children_4_6 0
  val node_5_5 = nth children_4_7 0
  val graph_5_1 = TD.get_graph(node_5_1)
  val graph_5_2 = TD.get_graph(node_5_2)
  val graph_5_3 = TD.get_graph(node_5_3)
  val graph_5_4 = TD.get_graph(node_5_4)
  val graph_5_5 = TD.get_graph(node_5_5)
  val _ = Testing.test "Checking the graph.." assert_graph_name_eq(graph_5_2, gn4)
  val _ = Testing.test "Checking the graph.." assert_graph_name_eq(graph_5_1, gn3)
  val _ = Testing.test "Checking the graph.." assert_graph_name_eq(graph_5_3, gn2)
  val _ = Testing.test "Checking the graph.." assert_graph_name_eq(graph_5_4, gn6)
  val _ = Testing.test "Checking the graph.." assert_graph_name_eq(graph_5_5, gn7)






(******************)
(* PRUNING TESTS *)
(******************)
  
  (* Building the graphs *)
  val target1 = G.empty
  val t1_v1 = V.mk "t1v1" 
  val target1 = target1 |> G.add_named_vertex t1_v1 k1            
  val t1_lit1 = L.mk target1 t1_v1



  val target2 = G.empty
  
  val t2_v1 = V.mk "t2v1"
  val t2_v2 = V.mk "t2v2"
  val t2_v3 = V.mk "t2v3"

  val t2_e1 = E.mk "t2e1"
  val t2_e2 = E.mk "t2e2"
  val t2_e3 = E.mk "t2e3"
  
  val target2 = target2 |> G.add_named_vertex t2_v1 k1
                        |> G.add_named_vertex t2_v2 k2
                        |> G.add_named_vertex t2_v3 k2
                        |> G.add_named_edge t2_e1 (Directed,eunit1) t2_v1 t2_v2
                        |> G.add_named_edge t2_e2 (Directed,eunit1) t2_v1 t2_v3
                        |> G.add_named_edge t2_e3 (Directed,eunit1) t2_v2 t2_v3

  val t2_cl = CL.mk target2
  val t2_c1_1 = nth t2_cl 0
  val t2_c1_2 = nth t2_cl 1
  val t2_lit1 = nth t2_c1_1 0
  val t2_lit2 = nth t2_c1_2 0
  val t2_lit3 = nth t2_c1_2 1



  val target3 = G.empty
  
  val t3_v1 = V.mk "t3v1"
  
  val target3 = target3 |> G.add_named_vertex t3_v1 k2

  val t3_cl = CL.mk target3
  val t3_c1_1 = nth t3_cl 0


  
  val target4 = G.empty
  
  val t4_v1a = V.mk "t4v1a"
  val t4_v1b = V.mk "t4v1b"
  val t4_v2 = V.mk "t4v2"
  val t4_v3 = V.mk "t4v3"
  val t4_v4 = V.mk "t4v4"

  val t4_e1a = E.mk "t4e1a"
  val t4_e2a = E.mk "t4e2a"
  val t4_e3a = E.mk "t4e3a"
  val t4_e1b = E.mk "t4e1b"
  val t4_e2b = E.mk "t4e2b"
  val t4_e3b = E.mk "t4e3b"
  val t4_e4 = E.mk "t4e4"
  
  val target4 = target4 |> G.add_named_vertex t4_v1a k2
                        |> G.add_named_vertex t4_v1b k2
                        |> G.add_named_vertex t4_v2 k2
                        |> G.add_named_vertex t4_v3 k2
                        |> G.add_named_vertex t4_v4 k1
                        |> G.add_named_edge t4_e1a (Directed,eunit1) t4_v1a t4_v2
                        |> G.add_named_edge t4_e2a (Directed,eunit1) t4_v1a t4_v3
                        |> G.add_named_edge t4_e3a (Directed,eunit1) t4_v1a t4_v4
                        |> G.add_named_edge t4_e1b (Directed,eunit1) t4_v1b t4_v2
                        |> G.add_named_edge t4_e2b (Directed,eunit1) t4_v1b t4_v3
                        |> G.add_named_edge t4_e3b (Directed,eunit1) t4_v1b t4_v4
                        |> G.add_named_edge t4_e4 (Directed,eunit1) t4_v4 t4_v3

  val t4_cl = CL.mk target4
  val t4_c1_1 = nth t4_cl 0
  val t4_c1_2 = nth t4_cl 1
  val t4_c1_3 = nth t4_cl 1
  val t4_lit1_1 = nth t4_c1_1 0
  val t4_lit2_1 = nth t4_c1_2 0
  val t4_lit2_2 = nth t4_c1_2 1
  val t4_lit3_1 = nth t4_c1_3 0
  val t4_lit3_2 = nth t4_c1_3 1

 


  val target5 = G.empty
  
  val t5_v1 = V.mk "t5v1"
  val t5_v2 = V.mk "t5v2"
  val t5_v3 = V.mk "t5v3"
  val t5_v4 = V.mk "t5v4"
  val t5_v5 = V.mk "t5v5"

  val t5_e1 = E.mk "t5e1"
  val t5_e2 = E.mk "t5e2"
  val t5_e3 = E.mk "t5e3"
  val t5_e4 = E.mk "t5e4"
  val t5_e5 = E.mk "t5e5"
  
  val target5 = target5 |> G.add_named_vertex t5_v1 k1
                        |> G.add_named_vertex t5_v2 k1
                        |> G.add_named_vertex t5_v3 k2
                        |> G.add_named_vertex t5_v4 k3
                        |> G.add_named_vertex t5_v5 k2
                        |> G.add_named_edge t5_e1 (Directed,eunit1) t5_v1 t5_v2
                        |> G.add_named_edge t5_e2 (Directed,eunit1) t5_v1 t5_v3
                        |> G.add_named_edge t5_e3 (Directed,eunit1) t5_v2 t5_v3
                        |> G.add_named_edge t5_e4 (Directed,eunit1) t5_v3 t5_v4
                        |> G.add_named_edge t5_e5 (Directed,eunit1) t5_v4 t5_v5

  val t5_cl = CL.mk target5
  val t5_c1_1 = nth t5_cl 0
  val t5_c1_2 = nth t5_cl 1
  val t5_c1_3 = nth t5_cl 1
  val t5_c1_4 = nth t5_cl 2
  val t5_lit1 = nth t5_c1_1 0
  val t5_lit2 = nth t5_c1_2 0
  val t5_lit3 = nth t5_c1_2 1
  val t5_lit4 = nth t5_c1_3 0
  val t5_lit5 = nth t5_c1_4 0




  val target6 = G.empty
  
  val t6_v1 = V.mk "t6v1"
  val t6_v2 = V.mk "t6v2"
  val t6_v3 = V.mk "t6v3"
  val t6_v4 = V.mk "t6v4"

  val t6_e1 = E.mk "t6e1"
  val t6_e2 = E.mk "t6e2"
  val t6_e3 = E.mk "t6e3"
  val t6_e4 = E.mk "t6e4"
  val t6_e5 = E.mk "t6e5"
  
  val target6 = target6 |> G.add_named_vertex t6_v1 k1
                        |> G.add_named_vertex t6_v2 k1
                        |> G.add_named_vertex t6_v3 k2
                        |> G.add_named_vertex t6_v4 k2
                        |> G.add_named_edge t6_e1 (Directed,eunit1) t6_v1 t6_v2
                        |> G.add_named_edge t6_e2 (Directed,eunit1) t6_v1 t6_v3
                        |> G.add_named_edge t6_e3 (Directed,eunit1) t6_v2 t6_v3
                        |> G.add_named_edge t6_e4 (Directed,eunit1) t6_v3 t6_v4

  val t6_cl = CL.mk target6
  val t6_c1_1 = nth t6_cl 0
  val t6_c1_2 = nth t6_cl 1
  val t6_c1_3 = nth t6_cl 1
  val t6_lit1 = nth t6_c1_1 0
  val t6_lit2 = nth t6_c1_2 0
  val t6_lit3 = nth t6_c1_2 1
  val t6_lit4 = nth t6_c1_3 0



  val target7 = G.empty
  
  val t7_v1 = V.mk "t7v1"
  val t7_v2 = V.mk "t7v2"
  val t7_v3 = V.mk "t7v3"

  val t7_e1 = E.mk "t7e1"
  val t7_e2 = E.mk "t7e2"
  val t7_e3 = E.mk "t7e3"
  
  val target7 = target7 |> G.add_named_vertex t7_v1 k2
                        |> G.add_named_vertex t7_v2 k1
                        |> G.add_named_vertex t7_v3 k1
                        |> G.add_named_edge t7_e1 (Directed,eunit1) t7_v1 t7_v2
                        |> G.add_named_edge t7_e2 (Directed,eunit1) t7_v3 t7_v1
                        |> G.add_named_edge t7_e3 (Directed,eunit1) t7_v2 t7_v1

  val t7_cl = CL.mk target7
  val t7_c1_1 = nth t7_cl 0
  val t7_c1_2 = nth t7_cl 1
  val t7_lit1 = nth t7_c1_1 0
  val t7_lit2 = nth t7_c1_2 0
  val t7_lit3 = nth t7_c1_2 1



  val target8 = G.empty
  
  val t8_v1 = V.mk "t8v1"
  val t8_v2 = V.mk "t8v2"
  val t8_v3 = V.mk "t8v3"
  val t8_v4 = V.mk "t8v4"
  val t8_v5 = V.mk "t8v5"

  val t8_e1 = E.mk "t8e1"
  val t8_e2 = E.mk "t8e2"
  val t8_e3 = E.mk "t8e3"
  val t8_e4 = E.mk "t8e4"
  
  val target8 = target8 |> G.add_named_vertex t8_v1 k2
                        |> G.add_named_vertex t8_v2 k1
                        |> G.add_named_vertex t8_v3 k1
                        |> G.add_named_vertex t8_v4 k2
                        |> G.add_named_vertex t8_v5 k1
                        |> G.add_named_edge t8_e1 (Directed,eunit1) t8_v1 t8_v2
                        |> G.add_named_edge t8_e2 (Directed,eunit1) t8_v1 t8_v3
                        |> G.add_named_edge t8_e3 (Directed,eunit1) t8_v2 t8_v4
                        |> G.add_named_edge t8_e4 (Directed,eunit1) t8_v3 t8_v5

  val t8_cl = CL.mk target8
  val t8_c1_1 = nth t8_cl 0
  val t8_c1_2 = nth t8_cl 1
  val t8_c1_3 = nth t8_cl 2
  val t8_lit1 = nth t8_c1_1 0
  val t8_lit2 = nth t8_c1_2 0
  val t8_lit3 = nth t8_c1_2 1
  val t8_lit4 = nth t8_c1_3 0
  val t8_lit5 = nth t8_c1_3 1




  val target9 = G.empty
  
  val t9_v1 = V.mk "t9v1"
  val t9_v2 = V.mk "t9v2"
  val t9_v3 = V.mk "t9v3"
  val t9_v4 = V.mk "t9v4"
  val t9_v5 = V.mk "t9v5"

  val t9_e1 = E.mk "t9e1"
  val t9_e2 = E.mk "t9e2"
  val t9_e3 = E.mk "t9e3"
  val t9_e4 = E.mk "t9e4"
  val t9_e5 = E.mk "t9e5"
  val t9_e6 = E.mk "t9e6"
  
  val target9 = target9 |> G.add_named_vertex t9_v1 k1
                        |> G.add_named_vertex t9_v2 k1
                        |> G.add_named_vertex t9_v3 k1
                        |> G.add_named_vertex t9_v4 k1
                        |> G.add_named_vertex t9_v5 k1
                        |> G.add_named_edge t9_e1 (Directed,eunit1) t9_v1 t9_v2
                        |> G.add_named_edge t9_e2 (Directed,eunit1) t9_v1 t9_v3
                        |> G.add_named_edge t9_e3 (Directed,eunit1) t9_v3 t9_v1
                        |> G.add_named_edge t9_e4 (Directed,eunit1) t9_v2 t9_v4
                        |> G.add_named_edge t9_e5 (Directed,eunit1) t9_v2 t9_v5
                        |> G.add_named_edge t9_e6 (Directed,eunit1) t9_v5 t9_v4

  val t9_cl = CL.mk target9
  val t9_c1_1 = nth t9_cl 0
  val t9_c1_2 = nth t9_cl 1
  val t9_c1_3 = nth t9_cl 2
  val t9_lit1 = nth t9_c1_1 0
  val t9_lit2 = nth t9_c1_2 0
  val t9_lit3 = nth t9_c1_2 1
  val t9_lit4 = nth t9_c1_3 0
  val t9_lit5 = nth t9_c1_3 1




  (* TESTING PRUNING *)

  fun printout([]) = " "
    | printout(g::[]) = GraphName.string_of_name(g) 
    | printout(g::gs) = GraphName.string_of_name(g) ^ printout(gs)

  (* Assertion functions *) 
  fun assert_graphs_eq(g1,g2) = 
    if TD.is_eq_graphs(g1,g2) then true
    else raise ERROR ("Graphs do not match " ^ printout(g2) ^ "  " ^ printout(g1))

  fun assert_string_eq(a,b) = 
    if a = b then true 
    else raise ERROR ("Graphs do n")


  (* Tests *)
  val gn1 = GraphName.mk "pattern1"
  val gn2 = GraphName.mk "pattern2"
  val gn3 = GraphName.mk "pattern3"
  val gn4 = GraphName.mk "pattern4"
  val gn5 = GraphName.mk "pattern5"
  val gn6 = GraphName.mk "pattern6"
  val gn7 = GraphName.mk "pattern7"
  val gnl = gn1 :: gn2 :: gn3 :: gn4 :: gn5 :: gn6 :: gn7 :: []
  val gl = pattern1 :: pattern2 :: pattern3 :: pattern4 :: pattern5 :: pattern6 :: pattern7 :: []

  val tab = GraphName.NTab.empty
  val tab = TD.fold GraphName.NTab.doadd gnl gl tab

  val tree = TD.mk tab
  

  val tree_graphs = TD.graphs tree
  val _ = Testing.test "Checking the graphs in the tree.." assert_graphs_eq(gnl,tree_graphs)


  val pruned_tree1 = TD.extended_prune t1_v1 target1 tree
  val pruned_tree1_grpahs = TD.graphs pruned_tree1
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq([],pruned_tree1_grpahs)

  (*val root = pruned_tree1
  val contour_1 = TD.get_contour(root)
  val children_1 = TD.get_children(root)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_1,C.empty)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_1),0)*)
 
  val pruned_tree2 = TD.extended_prune t2_v1 target2 tree
  val pruned_tree2_graphs = TD.graphs pruned_tree2
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq(gn1::[],pruned_tree2_graphs)

  (*val target_contour_list = CL.mk_from target2 t2_v1
  val _ = Testing.test "Checking the contours.." assert_contour_list_equiv (target_contour_list, (t2_lit1::[]) :: (t2_lit2::t2_lit3::[]) :: [] )

  val level_1 = pruned_tree2
  val contour_1_1 = TD.get_contour(level_1)
  val children_1_1 = TD.get_children(level_1)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_1_1,C.empty)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_1_1),1)

  val level_2 = children_1_1
  val node_2_1 = nth level_2 0
  val contour_2_1 = TD.get_contour(node_2_1)
  val children_2_1 = TD.get_children(node_2_1)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_2_1,(lit1_1::[]))
  val _ = Testing.test "Checking t he contour.." assert_contour_equiv(contour_2_1,(lit3_1::[]))
  val _ = Testing.test "Checking the children.." assert_children_length((length children_2_1),1)

  val level_3 = children_2_1
  val node_3_1 = nth children_2_1 0
  val contour_3_1 = TD.get_contour(node_3_1)
  val children_3_1 = TD.get_children(node_3_1)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_3_1,(lit1_2::lit1_3::[]))
  val _ = Testing.test "Checking the children.." assert_children_length((length children_3_1),1)
  
  val level_4 = children_3_1
  val node_4_1 = nth children_3_1 0
  val graph_4_1 = TD.get_graph(node_4_1)
  val _ = Testing.test "Checking the graph.." assert_graph_name_eq(graph_4_1, gn1)*)

  val pruned_tree3 = TD.extended_prune t3_v1 target3 tree
  val pruned_tree3_graphs = TD.graphs pruned_tree3
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq(gn2::[],pruned_tree3_graphs)

  val pruned_tree4 = TD.extended_prune t4_v1a target4 tree
  val pruned_tree4_graphs = TD.graphs pruned_tree4
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq([],pruned_tree4_graphs)

  val pruned_tree4 = TD.extended_prune t4_v2 target4 tree
  val pruned_tree4_graphs = TD.graphs pruned_tree4
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq(gn2::[],pruned_tree4_graphs)

  (*val t4_cl = CL.mk_from target4 t4_v2
  val t4_c1_1 = nth t4_cl 0
  val t4_c1_2 = nth t4_cl 1
  val t4_c1_3 = nth t4_cl 2
  val t4_lit1_1 = nth t4_c1_1 0
  val t4_lit2_1 = nth t4_c1_2 0
  val t4_lit2_2 = nth t4_c1_2 1
  val t4_lit3_1 = nth t4_c1_3 0
  val t4_lit3_2 = nth t4_c1_3 1

  val _ = Testing.test "Checking the literals.." assert_lit_equiv(t4_lit1_1,L.build( V.mk "t4v2", k2, L.mult_none, 2, L.mult_none, 0, L.mult_none ))
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(t4_lit2_1,L.build( V.mk "t4v1a", k2, L.mult_none, 0, L.mult_none, 3, L.mult_none ))
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(t4_lit2_2,L.build( V.mk "t4v1b", k2, L.mult_none, 0, L.mult_none, 3, L.mult_none ))
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(t4_lit3_1,L.build( V.mk "t4v3", k2, L.mult_none, 3, L.mult_none, 0, L.mult_none ))
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(t4_lit3_2,L.build( V.mk "t4v4", k1, L.mult_none, 2, L.mult_none, 1, L.mult_none )) *)

  val pruned_tree5 = TD.extended_prune t5_v1 target5 tree
  val pruned_tree5_graphs = TD.graphs pruned_tree5
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq(gn4::[],pruned_tree5_graphs)

  val pruned_tree6 = TD.extended_prune t6_v1 target6 tree
  val pruned_tree6_graphs = TD.graphs pruned_tree6
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq(gn3 :: gn4:: [],pruned_tree6_graphs)

  val pruned_tree7 = TD.extended_prune t7_v1 target7 tree
  val pruned_tree7_graphs = TD.graphs pruned_tree7
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq(gn5::[],pruned_tree7_graphs)

  (*val target_contour_list = CL.mk_from target7 t7_v1
  val t7_c1 = nth target_contour_list 0
  val t7_c2 = nth target_contour_list 1
  val lit7_1 = nth t7_c1 0
  val lit7_2 = nth t7_c2 0
  val lit7_3 = nth t7_c2 1
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(lit7_1,lit5_1)
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(lit7_2,L.build( V.mk "t4v2", k1, L.mult_none, 1, L.mult_none, 1, L.mult_none ))
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(lit7_3,L.build( V.mk "t4v2", k1, L.mult_none, 0, L.mult_none, 1, L.mult_none ))
  val _ = Testing.test "Checking strong compatibility.." assert_strong_compatibility(t7_c1,c5_1)
  val _ = Testing.test "Checking strong compatibility.." assert_weak_compatibility(t7_c2,c5_2)*)  

  val pruned_tree8 = TD.extended_prune t8_v1 target8 tree
  val pruned_tree8_graphs = TD.graphs pruned_tree8
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq(gn6::[],pruned_tree8_graphs)

  (*val target_contour_list = CL.mk_from target8 t8_v1
  val t8_c1 = nth target_contour_list 0
  val t8_c2 = nth target_contour_list 1
  val t8_c3 = nth target_contour_list 2
  val lit8_1 = nth t8_c1 0
  val lit8_2 = nth t8_c2 0
  val lit8_3 = nth t8_c2 1
  val lit8_4 = nth t8_c3 0
  val lit8_5 = nth t8_c3 1
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(lit8_1,lit6_1)
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(lit8_3,lit6_3)
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(lit8_5,lit6_4)
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(lit8_2,L.build( V.mk "t4v2", k1, L.mult_none, 1, L.mult_none, 1, L.mult_none ))
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(lit8_4,L.build( V.mk "t4v2", k2, L.mult_none, 1, L.mult_none, 0, L.mult_none ))
  val _ = Testing.test "Checking strong compatibility.." assert_strong_compatibility(t8_c1,c6_1)
  val _ = Testing.test "Checking strong compatibility.." assert_weak_compatibility(t8_c2,c6_2)
  val _ = Testing.test "Checking strong compatibility.." assert_weak_compatibility(t8_c3,c6_3)*)

  val pruned_tree9 = TD.extended_prune t9_v1 target9 tree
  val pruned_tree9_graphs = TD.graphs pruned_tree9
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq([],pruned_tree9_graphs)

  val pruned_tree9 = TD.standard_prune t9_v1 target9 tree
  val pruned_tree9_graphs = TD.graphs pruned_tree9
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq(gn7::[],pruned_tree9_graphs) 

in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR DNETS PASSED!"
end


(*
  val target_contour_list = CL.mk_from target3 t3_v1
  val t3_cl = nth target_contour_list 0
  val lit3_1 = nth t3_cl 0
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(lit3_1,L.build( V.mk "t4v2", k2, L.mult_none, 0, L.mult_none, 0, L.mult_none ))
  
  val children = TD.get_children(tree)
  val child = nth children 1

  val cont = TD.get_contour(child)
  val _ = Testing.test "X" assert_contour_equiv(cont, (L.build( V.mk "t4v2", k2, L.mult_none, 0, L.mult_star, 0, L.mult_none) :: []))

  val is_weak = C.contains_boundary(cont)
  val t = if is_weak then 1 else 0
  val _ = Testing.test "X" assert_int_eq(t,0,"s")

  val is_saved = C.check_strong_compatibility(t3_cl,cont)
  val t = if is_saved then 1 else 0
  val _ = Testing.test "X" assert_int_eq(t,1,"s")*)
