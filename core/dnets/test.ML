local
  open Test_MkG
  open Test_MatchUtil

  
  structure TD = Top_DNet(G)
  structure CL = TD.CL
  structure C = CL.C
  structure L = C.L

  val bvert = Test_Graph.OVData.EVert;
  
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
  
  val g = g |> G.add_named_vertex v1 bvert
            |> G.add_named_vertex v2 bvert
            |> G.add_named_vertex v3 bvert
            |> G.add_named_vertex v4 bvert
            |> G.add_named_vertex v5 bvert
            |> G.add_named_edge e1 dir_edge v1 v2
            |> G.add_named_edge e2 dir_edge v1 v3
            |> G.add_named_edge e3 dir_edge v2 v3
            |> G.add_named_edge e4 dir_edge v5 v4

  val (bb1,g) = g |> G.add_bbox
  val g = g |> G.add_to_bbox bb1 (V.NSet.single v5)

  val lit1_1 = L.mk g v1
  val lit1_2 = L.mk g v2
  val lit1_3 = L.mk g v3
  val lit1_4 = L.mk g v4
  val lit1_5 = L.mk g v5
  


  val h = G.empty
  
  val w1 = V.mk "w1"
  val w2 = V.mk "w2"
  val w3 = V.mk "w3"
  val w4 = V.mk "w4"

  val o1 = E.mk "o1"
  val o2 = E.mk "o2"
  val o3 = E.mk "o3"
  val o4 = E.mk "o4"
  
  val h = h |> G.add_named_vertex w1 bvert
            |> G.add_named_vertex w2 bvert
            |> G.add_named_vertex w3 bvert
            |> G.add_named_vertex w4 bvert
            |> G.add_named_edge o1 dir_edge w1 w2
            |> G.add_named_edge o2 dir_edge w1 w3
            |> G.add_named_edge o3 dir_edge w1 w4
            |> G.add_named_edge o4 dir_edge w4 w3

  val (bb2,h) = h |> G.add_bbox
  val h = h |> G.add_to_bbox bb2 (V.NSet.single w1)
            |> G.add_to_bbox bb2 (V.NSet.single w4)

  val lit2_1 = L.mk h w1
  val lit2_2 = L.mk h w2
  val lit2_3 = L.mk h w3
  val lit2_4 = L.mk h w4



  val j = G.empty
  
  val u1 = V.mk "u1"
  val u2 = V.mk "u2"
  val u3 = V.mk "u3"
  val u4 = V.mk "u4"

  val i1 = E.mk "i1"
  val i2 = E.mk "i2"
  val i3 = E.mk "i3"
  val i4 = E.mk "i4"
  
  val j = j |> G.add_named_vertex u1 bvert
            |> G.add_named_vertex u2 bvert
            |> G.add_named_vertex u3 bvert
            |> G.add_named_vertex u4 bvert
            |> G.add_named_edge i1 dir_edge u1 u2
            |> G.add_named_edge i2 dir_edge u1 u3
            |> G.add_named_edge i3 dir_edge u2 u3
            |> G.add_named_edge i4 dir_edge u3 u4

  val lit3_1 = L.mk j u1
  val lit3_2 = L.mk j u2
  val lit3_3 = L.mk j u3
  val lit3_4 = L.mk j u4



  (* TESTING LITERALS *)

  (* Assertion functions *)
  fun assert_name_eq(a:V.name,b:V.name) =
    if V.name_eq(a,b) then true
    else raise ERROR ("Literal names do not match!")

  fun assert_kind_eq(a:G.VData.data,b:G.VData.data) =
    if G.VData.data_eq(a,b) then true
    else raise ERROR ("Literal kinds do not match!")

  fun assert_int_eq(a,b,s) =
    if (a=b) then true
    else raise ERROR ("Literal " ^ s ^ "do not match!")

  fun assert_mult_eq(a:L.multiplicity,b:L.multiplicity,s) =
    if L.mult_eq(a,b) then true
    else raise ERROR ("Literal " ^ s ^ "do not match!")


  (* Tests *)
  val name1 = L.get_name lit1_1
  val name2 = L.get_name lit1_2
  val _ = Testing.test "Checking literal name.." assert_name_eq (name1,v1)
  val _ = Testing.test "Checking literal name.." assert_name_eq (name2,v2)
  
  val kind1 = L.get_kind lit1_1
  val kind2 = L.get_kind lit1_2
  val _ = Testing.test "Checking literal kind.." assert_kind_eq (kind1,bvert)
  val _ = Testing.test "Checking literal kind.." assert_kind_eq (kind2,bvert)

  val k_m1 = L.get_kind_mult lit1_1
  val k_m2 = L.get_kind_mult lit1_2
  val k_m4 = L.get_kind_mult lit1_4
  val k_m5 = L.get_kind_mult lit1_5
  val _ = Testing.test "Checking literal kind-mult" assert_mult_eq (k_m1,L.mult_none,"kind-mult")
  val _ = Testing.test "Checking literal kind-mult" assert_mult_eq (k_m2,L.mult_none,"kind-mult")
  val _ = Testing.test "Checking literal kind-mult" assert_mult_eq (k_m4,L.mult_qm,"kind-mult") 
  val _ = Testing.test "Checking literal kind-mult" assert_mult_eq (k_m5,L.mult_star,"kind-mult")  

  val i_a1 = L.get_input_arity lit1_1
  val i_a2 = L.get_input_arity lit1_2
  val _ = Testing.test "Checking literal input-arity" assert_int_eq (i_a1,0,"input-arity")
  val _ = Testing.test "Checking literal input-arity" assert_int_eq (i_a2,1,"input-arity")

  val i_m1 = L.get_input_mult lit1_1
  val i_m2 = L.get_input_mult lit1_2
  val i_m4 = L.get_input_mult lit1_4
  val _ = Testing.test "Checking literal input-mult" assert_mult_eq (i_m1,L.mult_none,"input-mult")
  val _ = Testing.test "Checking literal input-mult" assert_mult_eq (i_m2,L.mult_none,"input-mult")
  val _ = Testing.test "Checking literal input-mult" assert_mult_eq (i_m4,L.mult_star,"input-mult")

  val o_a1 = L.get_output_arity lit1_1
  val o_a2 = L.get_output_arity lit1_2
  val _ = Testing.test "Checking literal output-arity" assert_int_eq (o_a1,2,"output-arity")
  val _ = Testing.test "Checking literal output-arity" assert_int_eq (o_a2,1,"output-arity")

  val o_m1 = L.get_output_mult lit1_1
  val o_m2 = L.get_output_mult lit1_2
  val o_m4 = L.get_output_mult lit1_4
  val _ = Testing.test "Checking literal output-mult" assert_mult_eq (o_m1,L.mult_none,"output-mult")
  val _ = Testing.test "Checking literal output-mult" assert_mult_eq (o_m2,L.mult_none,"output-mult")
  val _ = Testing.test "Checking literal output-mult" assert_mult_eq (o_m4,L.mult_none,"output-mult")


  (* TESTING CONTOURS *)

  (* Assertion functions *)
  fun is_one(x,[]) = false
    | is_one(x,c::[]) = if C.equiv(x,c) then true else false
    | is_one(x,c::cs) = if C.equiv(x,c) then true else is_one(x,cs)
     

  fun assert_tgt_fn(c,cc) =
    if (is_one(c,cc)) then true
    else raise ERROR ("Error in target function")

  fun assert_contour_length(i,c) = if (length c = i) then true 
    else raise ERROR ("Error in contour length")

  fun assert_is_in_contour(l,[]) = raise ERROR ("Error in the elements of the contour")
    | assert_is_in_contour(l,c::[]) = if L.eq(l,c) then true else raise ERROR ("Error in the elements of the contour")
    | assert_is_in_contour(l,c::cs) = if L.eq(l,c) then true else assert_is_in_contour(l,cs)

     fun assert_is_not_in_contour(l,[]) = true
    | assert_is_not_in_contour(l,c::[]) = if L.eq(l,c) then raise ERROR ("Error in the elements of the contour") else true
    | assert_is_not_in_contour(l,c::cs) = if L.eq(l,c) then raise ERROR ("Error in the elements of the contour") else assert_is_not_in_contour(l,cs)

  
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

  val _ = Testing.test "Checking target function" assert_tgt_fn (trg1,ok1list)
  val _ = Testing.test "Checking target function" assert_tgt_fn (trg2,ok2list) 


  val c1 = C.mk g trg1
  val _ = Testing.test "Checking number of literals" assert_contour_length (2,c1)
  val _ = Testing.test "Checking element in contour" assert_is_in_contour (lit1_2,c1)
  val _ = Testing.test "Checking element in contour" assert_is_in_contour (lit1_3,c1)
  val _ = Testing.test "Checking element in contour" assert_is_not_in_contour (lit1_4,c1)
  val _ = Testing.test "Checking element in contour" assert_is_not_in_contour (lit1_5,c1)

  val c2 = C.mk g (lit1_2::lit1_3::[])
  val _ = Testing.test "Checking number of literals" assert_contour_length (1,c2)


  (* TESTING CONTOUR LISTS *)

  (* Assertion functions *)
  fun assert_contour_list_length(i,cl) = if (length cl = i) then true 
    else raise ERROR ("Error in contour list length")

  fun assert_contour_list_eq(cl1,cl2) = if CL.equiv(cl1,cl2) then true
    else raise ERROR ("Error in contour list")

  (* Tests *)
  val cl1 = CL.mk g
  val hand_cl1 = (lit1_1::[]) :: (lit1_2::lit1_3::[]) :: []
  val _ = Testing.test "Checking number of contours in contour list" assert_contour_list_length (2,cl1)
  val _ = Testing.test "Checking the contours" assert_contour_list_eq (hand_cl1,cl1)

  val cl2 = CL.mk h
  val hand_cl2_1 = (lit2_2::[]) :: (lit2_1::[]) :: (lit2_4::lit2_3::[]) :: []
  val hand_cl2_2 = (lit2_2::[]) :: (lit2_1::[]) :: (lit2_3::lit2_4::[]) :: []
  val _ = Testing.test "Checking number of contours in contour list" assert_contour_list_length (3,cl2)
  val _ = Testing.test "Checking the contours" assert_contour_list_eq (hand_cl2_1,cl2)
  val _ = Testing.test "Checking the contours" assert_contour_list_eq (hand_cl2_2,cl2)

  val cl3 = CL.mk j
  val hand_cl3 = (lit3_1::[]) :: (lit3_2::lit3_3::[]) :: (lit3_4::[]) :: []
  val _ = Testing.test "Checking number of contours in contour list" assert_contour_list_length (3,cl3)
  val _ = Testing.test "Checking the contours" assert_contour_list_eq (hand_cl3,cl3)



  (* TESTING DNET *)

  (* Assertion functions *)
  fun assert_contour_eq([],[]) = true
    | assert_contour_eq(c1,c2) = if C.equiv(c1,c2) then true
     else raise ERROR ("Contours do not match")
    | assert_contour_eq(_,_) = false

  fun assert_graph_name_eq(n1,n2) = if GraphName.name_eq(n1,n2) then true
     else raise ERROR ("Graph names do not match")

  fun assert_children_length(i,j) = if i=j then true
    else raise ERROR ("Children number do not match")

  (* Tests *)
  (*val dnet = Test_Graph.NTab.empty
  val gname = Test_Graph.mk "g"
  val dnet = Test_Graph.NTab.ins (gname g) dnet*)

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
  val _ = Testing.test "Checking the contour" assert_contour_eq(contour_1_1,C.empty)
  val _ = Testing.test "Checking the children" assert_children_length((length children_1_1),2)

  (* val _ = Testing.test "Checking the contour" assert_contour_eq((lit1_1::[]),(lit3_1::[])) *)

  val level_2 = children_1_1
  val node_2_1 = nth level_2 0
  val node_2_2 = nth level_2 1
  val contour_2_1 = TD.get_contour(node_2_1)
  val contour_2_2 = TD.get_contour(node_2_2)
  val children_2_1 = TD.get_children(node_2_1)
  val children_2_2 = TD.get_children(node_2_2)
  val _ = Testing.test "Checking the contour" assert_contour_eq(contour_2_2,(lit2_2::[]))
  val _ = Testing.test "Checking the contour" assert_contour_eq(contour_2_1,(lit1_1::[]))
  val _ = Testing.test "Checking the contour" assert_contour_eq(contour_2_1,(lit3_1::[]))
  val _ = Testing.test "Checking the children" assert_children_length((length children_2_1),2)
  val _ = Testing.test "Checking the children" assert_children_length((length children_2_2),1)

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
  val _ = Testing.test "Checking the contour" assert_contour_eq(contour_3_3,(lit2_1::[]))
  val _ = Testing.test "Checking the contour" assert_contour_eq(contour_3_2,(lit1_2::lit1_3::[]))
  val _ = Testing.test "Checking the contour" assert_contour_eq(contour_3_1,(lit3_2::lit3_3::[]))
  val _ = Testing.test "Checking the children" assert_children_length((length children_3_1),1)
  val _ = Testing.test "Checking the children" assert_children_length((length children_3_2),1)
  val _ = Testing.test "Checking the children" assert_children_length((length children_3_3),1)

  val level_4 = children_3_1 :: children_3_2 :: children_3_3 :: []
  val node_4_1 = nth children_3_1 0
  val node_4_2 = nth children_3_2 0
  val node_4_3 = nth children_3_3 0
  val contour_4_1 = TD.get_contour(node_4_1)
  val children_4_1 = TD.get_children(node_4_1)
  val graph_4_2 = TD.get_graph(node_4_2)
  val contour_4_3 = TD.get_contour(node_4_3)
  val children_4_3 = TD.get_children(node_4_3)
  val _ = Testing.test "Checking the contour" assert_contour_eq(contour_4_1,(lit3_4::[]))
  val _ = Testing.test "Checking the children" assert_children_length((length children_4_1),1)
  val _ = Testing.test "Checking the graph" assert_graph_name_eq(graph_4_2, gn1)
  val _ = Testing.test "Checking the contour" assert_contour_eq(contour_4_3,(lit2_4::lit2_3::[]))
  val _ = Testing.test "Checking the children" assert_children_length((length children_4_3),1)
  

  val level_5 = children_4_1 :: children_4_3 :: []
  val node_5_1 = nth children_4_1 0
  val node_5_3 = nth children_4_3 0
  val graph_5_1 = TD.get_graph(node_5_1)
  val graph_5_3 = TD.get_graph(node_5_3)
  val _ = Testing.test "Checking the graph" assert_graph_name_eq(graph_5_1, gn3)
  val _ = Testing.test "Checking the graph" assert_graph_name_eq(graph_5_3, gn2)

in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR DNETS PASSED!"
end