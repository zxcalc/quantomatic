local
  open Test_MkG
  open Test_MatchUtil

  
  structure TD = Top_DNet(G)
  structure CL = TD.CL
  structure C = CL.C
  structure L = C.L

  val b = Test_Graph.OVData.WVert;
  val k1 = (mkX zero_angle)
  val k2 = (mkX (LinratAngleExpr.parse "1"))
  val k3 = (mkX (LinratAngleExpr.parse "2"))



(******************)
(* BUILDING TESTS *)
(******************)
  
  (* Building the graphs *)
  val g = G.empty
  
  val v1 = V.mk "v1"
  val v2 = V.mk "v2"
  val v3 = V.mk "v3"
  val v4 = V.mk "v4"
  val v5 = V.mk "v5"

  val e1 = E.mk "e1"
  val e2 = E.mk "e2"
  val e3 = E.mk "e3"
  val e4 = E.mk "e4"
  
  val g = g |> G.add_named_vertex v1 k1
            |> G.add_named_vertex v2 k2
            |> G.add_named_vertex v3 k2
            |> G.add_named_vertex v4 k1
            |> G.add_named_vertex v5 k2
            |> G.add_named_edge e1 dir_edge v1 v2
            |> G.add_named_edge e2 dir_edge v1 v3
            |> G.add_named_edge e3 dir_edge v2 v3
            |> G.add_named_edge e4 dir_edge v5 v4 

  val (bb1,g) = g |> G.add_bbox
  val g = g |> G.doadd_to_bbox bb1 (V.NSet.single v5)
  


  val h = G.empty
  
  val w1 = V.mk "w1"
  val w2 = V.mk "w2"
  val w3 = V.mk "w3"
  val w4 = V.mk "w4"

  val o1 = E.mk "o1"
  val o2 = E.mk "o2"
  val o3 = E.mk "o3"
  val o4 = E.mk "o4"
  
  val h = h |> G.add_named_vertex w1 k2
            |> G.add_named_vertex w2 k2
            |> G.add_named_vertex w3 k2
            |> G.add_named_vertex w4 k1
            |> G.add_named_edge o1 dir_edge w1 w2
            |> G.add_named_edge o2 dir_edge w1 w3
            |> G.add_named_edge o3 dir_edge w1 w4
            |> G.add_named_edge o4 dir_edge w4 w3

  val (bb2,h) = h |> G.add_bbox
  val (bb3,h) = h |> G.add_bbox
  val h = h |> G.doadd_to_bbox bb2 (V.NSet.single w1)
            |> G.doadd_to_bbox bb3 (V.NSet.single w4)
  

  val j = G.empty
  
  val u1 = V.mk "u1"
  val u2 = V.mk "u2"
  val u3 = V.mk "u3"
  val u4 = V.mk "u4"

  val i1 = E.mk "i1"
  val i2 = E.mk "i2"
  val i3 = E.mk "i3"
  val i4 = E.mk "i4"
  
  val j = j |> G.add_named_vertex u1 k1
            |> G.add_named_vertex u2 k1
            |> G.add_named_vertex u3 k2
            |> G.add_named_vertex u4 k2
            |> G.add_named_edge i1 dir_edge u1 u2
            |> G.add_named_edge i2 dir_edge u1 u3
            |> G.add_named_edge i3 dir_edge u2 u3
            |> G.add_named_edge i4 dir_edge u3 u4


  val k = G.empty
  
  val y1 = V.mk "y1"
  val y2 = V.mk "y2"
  val y3 = V.mk "y3"

  val l1 = E.mk "l1"
  val l2 = E.mk "l2"
  val l3 = E.mk "l3"
  
  val k = k |> G.add_named_vertex y1 k1
            |> G.add_named_vertex y2 k1
            |> G.add_named_vertex y3 b
            |> G.add_named_edge l1 dir_edge y1 y2
            |> G.add_named_edge l2 dir_edge y1 y3
            |> G.add_named_edge l3 dir_edge y2 y3

  val (bb4,k) = k |> G.add_bbox
  val k = k |> G.doadd_to_bbox bb4 (V.NSet.single y2)



  (* Building the contour lists *) 
  val cl1 = CL.mk g 
  val c1_1 = nth cl1 0
  val c1_2 = nth cl1 1  
  val lit1_1 = nth c1_1 0
  val lit1_2 = nth c1_2 0 
  val lit1_3 = nth c1_2 1 
  val lit1_4 = L.mk g v4
  val lit1_5 = L.mk g v5


  val cl2 = CL.mk h
  val c2_1 = nth cl2 0
  val c2_2 = nth cl2 1
  val c2_3 = nth cl2 2
  val lit2_1 = nth c2_2 0
  val lit2_2 = nth c2_1 0
  val lit2_3 = nth c2_3 0
  val lit2_4 = nth c2_3 1

 
  val cl3 = CL.mk j
  val c3_1 = nth cl3 0
  val c3_2 = nth cl3 1
  val c3_3 = nth cl3 2
  val lit3_1 = nth c3_1 0
  val lit3_2 = nth c3_2 0
  val lit3_3 = nth c3_2 1
  val lit3_4 = nth c3_3 0


  
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
  val lit_e = L.build((V.mk "e"), bvert, L.mult_none, 1, L.mult_none, 0, L.mult_none)
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

  fun assert_kind_eq(a:G.VData.data,b:G.VData.data) =
    if G.VData.data_eq(a,b) then true
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
  val _ = Testing.test "Checking literal name.." assert_name_eq (name1,v1)
  val _ = Testing.test "Checking literal name.." assert_name_eq (name2,v2)
    
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
 
  val _ = Testing.test "Checking literal equality.." assert_lit_eq (lit2_1, L.build ( (V.mk "w1"), k2, L.mult_star, 0, L.mult_none, 3, L.mult_star) )
  val _ = Testing.test "Checking literal equality.." assert_lit_eq (lit2_2, L.build ( (V.mk "w2"), k2, L.mult_none, 1, L.mult_star, 0, L.mult_none) )
  val _ = Testing.test "Checking literal equality.." assert_lit_eq (lit2_3, L.build ( (V.mk "w3"), k2, L.mult_qm, 2, L.mult_star, 0, L.mult_none) )
  val _ = Testing.test "Checking literal equality.." assert_lit_eq (lit2_4, L.build ( (V.mk "w4"), k1, L.mult_star, 1, L.mult_star, 1, L.mult_none) )

  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit2_1, L.build ( (V.mk "w1"), k2, L.mult_star, 0, L.mult_none, 2, L.mult_star) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit2_2, L.build ( (V.mk "w2"), k2, L.mult_none, 0, L.mult_star, 0, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit2_3, L.build ( (V.mk "w3"), k2, L.mult_qm, 0, L.mult_star, 0, L.mult_none) )
  val _ = Testing.test "Checking literal equivalence.." assert_lit_equiv (lit2_4, L.build ( (V.mk "w4"), k1, L.mult_star, 0, L.mult_star, 1, L.mult_none) )
 
 


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
  val trg1 = C.target_function g  
  val ok1 = C.add_literal e lit1_1
  val ok2 = C.add_literal e lit1_2
  val ok3 = C.add_literal e lit1_3
  val ok4 = C.add_literal e lit1_4
  val ok1list = ok1::ok2::ok3::ok4::[] 

  val trg2 = C.target_function h
  val ok2 = C.add_literal e lit2_2
  val ok3 = C.add_literal e lit2_3
  val ok2list = ok2::ok3::[]

  val _ = Testing.test "Checking target function.." assert_tgt_fn (trg1,ok1list)
  val _ = Testing.test "Checking target function.." assert_tgt_fn (trg2,ok2list) 


  val c1 = C.mk g trg1
  val _ = Testing.test "Checking number of literals.." assert_contour_length (2,c1)
  val _ = Testing.test "Checking element in contour.." assert_is_eq_contained (lit1_2,c1)
  val _ = Testing.test "Checking element in contour.." assert_is_eq_contained (lit1_3,c1)
  val _ = Testing.test "Checking element in contour.." assert_is_eq_not_contained (lit1_4,c1)
  val _ = Testing.test "Checking element in contour.." assert_is_eq_not_contained (lit1_5,c1)

  val c2 = C.mk g (lit1_2::lit1_3::[])
  val _ = Testing.test "Checking number of literals.." assert_contour_length (1,c2)

 
  val lit_1 = L.build((V.mk "a"), k1, L.mult_none, 1, L.mult_none, 1, L.mult_none)
  val lit_2 = L.build((V.mk "b"), k2, L.mult_none, 1, L.mult_none, 0, L.mult_none)
  val target = lit_1 :: lit_2 :: lit_2 :: []

  val lit_3 = L.build((V.mk "c"), k1, L.mult_star, 0, L.mult_none, 1, L.mult_none)
  val pattern_1 = lit_1 :: lit_2 :: lit_3 :: lit_2 :: []
  val lit_4 =  L.build((V.mk "d"), k1, L.mult_none, 1, L.mult_none, 1, L.mult_star)
  val pattern_2 = lit_2 :: lit_4 :: lit_2 :: []
  val lit_5 = L.build((V.mk "e"), bvert, L.mult_none, 1, L.mult_none, 0, L.mult_none)
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




  (* TESTING DNET *)

  (* Assertion functions *) 
  fun assert_graph_name_eq(n1,n2) = 
     if GraphName.name_eq(n1,n2) then true
     else raise ERROR ("Graph names do not match")

  fun assert_children_length(i,j) = 
    if i=j then true
    else raise ERROR ("Children number do not match")

  (* Tests *)
  val e = TD.empty
  val cl1 = (lit1_1::[]) :: (lit1_2::lit1_3::[]) :: []
  val cl2 = (lit2_2::[]) :: (lit2_1::[]) :: (lit2_4::lit2_3::[]) :: []
  val cl3 = (lit3_1::[]) :: (lit3_2::lit3_3::[]) :: (lit3_4::[]) :: []
  val cll = cl1 :: cl2 :: cl3 :: []
  val gn1 = GraphName.mk "g"
  val gn2 = GraphName.mk "h"
  val gn3 = GraphName.mk "j"
  val gnl = gn1 :: gn2 :: gn3 :: []

  val tree = TD.add_cl_list_to_dnet e cll gnl
  
  val level_1 = tree
  val node_1 = tree
  val contour_1_1 = TD.get_contour(level_1)
  val children_1_1 = TD.get_children(level_1)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_1_1,C.empty)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_1_1),2)

  val level_2 = children_1_1
  val node_2_1 = nth level_2 0
  val node_2_2 = nth level_2 1
  val contour_2_1 = TD.get_contour(node_2_1)
  val contour_2_2 = TD.get_contour(node_2_2)
  val children_2_1 = TD.get_children(node_2_1)
  val children_2_2 = TD.get_children(node_2_2)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_2_2,(lit2_2::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_2_1,(lit1_1::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_2_1,(lit3_1::[]))
  val _ = Testing.test "Checking the children.." assert_children_length((length children_2_1),2)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_2_2),1)

  val level_3 = children_2_1 :: children_2_2 :: []
  val node_3_1 = nth children_2_1 0
  val node_3_2 = nth children_2_1 1
  val node_3_3 = nth children_2_2 0
  val contour_3_1 = TD.get_contour(node_3_1)
  val contour_3_2 = TD.get_contour(node_3_2)
  val contour_3_3 = TD.get_contour(node_3_3)
  val children_3_1 = TD.get_children(node_3_1)
  val children_3_2 = TD.get_children(node_3_2)
  val children_3_3 = TD.get_children(node_3_3)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_3_3,(lit2_1::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_3_2,(lit1_2::lit1_3::[]))
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_3_1,(lit3_2::lit3_3::[]))
  val _ = Testing.test "Checking the children.." assert_children_length((length children_3_1),1)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_3_2),1)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_3_3),1)

  val level_4 = children_3_1 :: children_3_2 :: children_3_3 :: []
  val node_4_1 = nth children_3_1 0
  val node_4_2 = nth children_3_2 0
  val node_4_3 = nth children_3_3 0
  val contour_4_1 = TD.get_contour(node_4_1)
  val children_4_1 = TD.get_children(node_4_1)
  val graph_4_2 = TD.get_graph(node_4_2)
  val contour_4_3 = TD.get_contour(node_4_3)
  val children_4_3 = TD.get_children(node_4_3)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_4_1,(lit3_4::[]))
  val _ = Testing.test "Checking the children.." assert_children_length((length children_4_1),1)
  val _ = Testing.test "Checking the graph.." assert_graph_name_eq(graph_4_2, gn1)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_4_3,(lit2_4::lit2_3::[]))
  val _ = Testing.test "Checking the children.." assert_children_length((length children_4_3),1)
  
  val level_5 = children_4_1 :: children_4_3 :: []
  val node_5_1 = nth children_4_1 0
  val node_5_3 = nth children_4_3 0
  val graph_5_1 = TD.get_graph(node_5_1)
  val graph_5_3 = TD.get_graph(node_5_3)
  val _ = Testing.test "Checking the graph.." assert_graph_name_eq(graph_5_1, gn3)
  val _ = Testing.test "Checking the graph.." assert_graph_name_eq(graph_5_3, gn2) 





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
                        |> G.add_named_edge t2_e1 dir_edge t2_v1 t2_v2
                        |> G.add_named_edge t2_e2 dir_edge t2_v1 t2_v3
                        |> G.add_named_edge t2_e3 dir_edge t2_v2 t2_v3

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
                        |> G.add_named_edge t4_e1a dir_edge t4_v1a t4_v2
                        |> G.add_named_edge t4_e2a dir_edge t4_v1a t4_v3
                        |> G.add_named_edge t4_e3a dir_edge t4_v1a t4_v4
                        |> G.add_named_edge t4_e1b dir_edge t4_v1b t4_v2
                        |> G.add_named_edge t4_e2b dir_edge t4_v1b t4_v3
                        |> G.add_named_edge t4_e3b dir_edge t4_v1b t4_v4
                        |> G.add_named_edge t4_e4 dir_edge t4_v4 t4_v3

  val t4_cl = CL.mk target4
  val t4_c1_1 = nth t4_cl 0
  val t4_c1_2 = nth t4_cl 1
  val t4_c1_3 = nth t4_cl 1
  val t4_lit1_1 = nth t4_c1_1 0
  val t4_lit2_1 = nth t4_c1_2 0
  val t4_lit2_2 = nth t4_c1_2 1
  val t4_lit3_1 = nth t4_c1_3 0
  val t4_lit3_2 = nth t4_c1_3 1

 

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
  val e = TD.empty
  val cl1 = (lit1_1::[]) :: (lit1_2::lit1_3::[]) :: []
  val cl2 = (lit2_2::[]) :: (lit2_1::[]) :: (lit2_4::lit2_3::[]) :: []
  val cl3 = (lit3_1::[]) :: (lit3_2::lit3_3::[]) :: (lit3_4::[]) :: []
  val cll = cl1 :: cl2 :: cl3 :: []
  val gn1 = GraphName.mk "g"
  val gn2 = GraphName.mk "h"
  val gn3 = GraphName.mk "j"
  val gnl = gn1 :: gn2 :: gn3 :: []

  
  val tree = TD.add_cl_list_to_dnet e cll gnl
  val tree_graphs = TD.graphs tree
  val _ = Testing.test "Checking the graphs in the tree.." assert_graphs_eq(gnl,tree_graphs)
 

  val pruned_tree1 = TD.prune t1_v1 target1 tree

  val root = pruned_tree1
  val contour_1 = TD.get_contour(root)
  val children_1 = TD.get_children(root)
  val _ = Testing.test "Checking the contour.." assert_contour_equiv(contour_1,C.empty)
  val _ = Testing.test "Checking the children.." assert_children_length((length children_1),0)

  val pruned_tree1_grpahs = TD.graphs pruned_tree1
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq([],pruned_tree1_grpahs)


  val pruned_tree2 = TD.prune t2_v1 target2 tree

  val target_contour_list = CL.mk_from target2 t2_v1
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
  val _ = Testing.test "Checking the graph.." assert_graph_name_eq(graph_4_1, gn1)
  
  val pruned_tree2_graphs = TD.graphs pruned_tree2
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq(gn1::[],pruned_tree2_graphs)


  val pruned_tree3 = TD.prune t3_v1 target3 tree
  val pruned_tree3_graphs = TD.graphs pruned_tree3
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq(gn2::[],pruned_tree3_graphs)


  val pruned_tree4 = TD.prune t4_v1a target4 tree
  val pruned_tree4_graphs = TD.graphs pruned_tree4
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq([],pruned_tree4_graphs)

  val pruned_tree4 = TD.prune t4_v2 target4 tree
  val pruned_tree4_graphs = TD.graphs pruned_tree4
  val _ = Testing.test "Checking the graphs.." assert_graphs_eq(gn2::[],pruned_tree4_graphs)

  val t4_cl = CL.mk_from target4 t4_v2
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
  val _ = Testing.test "Checking the literals.." assert_lit_equiv(t4_lit3_2,L.build( V.mk "t4v4", k1, L.mult_none, 2, L.mult_none, 1, L.mult_none ))


in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR DNETS PASSED!"
end