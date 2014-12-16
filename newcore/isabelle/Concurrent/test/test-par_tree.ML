(*This file contains correctness and speed tests for the parallel
(partially lazy) tree!
As always, the speed tests are in a separate structure*)
local
  open Test_ParallelSeqSpeed.Help; (*Useful list functions*)
  (*************************************************)
  (****************CORRECTNESS TESTS****************)
  (*************************************************)
  
  (*Some fake future data*)
  fun fd() = Future_Wrapper.new_future_data NONE;
  (*For simpler reference*)
  structure Par_Tree = Parallel_Tree;
    
  (*This datatype allows me to construct trees manually. A function will then take trees
  of this type, and construct a function which produces the given tree*)
  datatype 'a tree = Node of 'a tree List.list | Leaf of 'a
  
  (*This datatype is used by the algorithm when marking nodes...*)
  datatype 'a marked_tree = Marked_Node of int * ('a marked_tree List.list) | Marked_Leaf of 'a
  
  (*The function which constructs the function to produce the tree!
  It gives you the function to be applied to nodes, and the root
  of the tree*)
  fun tree_conv (Leaf x) = (Par_Tree.Leaf x,fn _ => []) (*Function not used*) 
    | tree_conv t =
      let
        (*Label all of the nodes with distinct integers...*)
        fun mark_tree n (Leaf x) = ((n-1),Marked_Leaf x)
          | mark_tree n (Node (l::ls)) = (*Mark the children first...*)
            let
              fun recur_mark (n,t) [] = [(n,t)]
                | recur_mark (n,t) (x::xs) = let
                  val (new_n,new_t) = mark_tree (n+1) x;
                in
                  (n,t)::(recur_mark (new_n,new_t) xs)
                end; 
              (*Dummy first result passed in*)
              val marked_children = List.tl (recur_mark (n,Marked_Node (0,[])) (l::ls));
              (*Find the last element in the list which contains the largest n used*)
              val largest = fst (List.last marked_children);
              (*Now mark my node!*)
              val new_node = Marked_Node (n,List.map (fn x => snd x) marked_children)
            in
              (largest,new_node)
            end
          | mark_tree _ _ = raise ERROR "Node must not have no leaves"
          (*Retrieve the marked tree!*)
          val m_tree = snd (mark_tree 0 t)
          (*The function will be constructed to recognise the integers
          on the tree and construct the relevant nodes accordingly.
          This produces a list of pairs of integers and children*)
          fun construct_map (Marked_Node (n,children)) =
            let
              fun extract_children [] = []
                | extract_children (c::cs) = case c of
                    Marked_Leaf v => (Par_Tree.Leaf v)::(extract_children cs)
                  | Marked_Node (m,_) => (Par_Tree.Node m)::(extract_children cs);
              val to_prod = extract_children children;
              fun apply_to_all map [] = map
                | apply_to_all map (c::cs) = case c of
                    Marked_Node _ => apply_to_all (map@(construct_map c)) cs
                  | _ => apply_to_all map cs;
              val mapped_children = apply_to_all [] children;
              val result = (n,to_prod)::(mapped_children);
            in
              result
            end
            | construct_map _ = raise ERROR "test-par_tree: recurred on leaf fault!"
          val official_map = construct_map m_tree;
          (*The function to use the map*)
          fun f n = (snd (List.hd (List.filter (fn (x,_) => (x=n)) official_map)))
      in
        (*The root will be marked with a 0*)
        (Par_Tree.Node 0,f)
      end
      
  (*A simplistic function to process the leaves according to the
  above function...*)
  fun simple_compute _ (Par_Tree.Leaf v) = [v]
    | simple_compute f (Par_Tree.Node v) = flat (List.map (simple_compute f) (f v))
  
  (*Compute the leaves directly from the simple tree*)
  fun compute_from_abstract (Leaf v) = [v]
    | compute_from_abstract (Node []) = raise ERROR "test-par_tree: node cannot have no children 2"
    | compute_from_abstract (Node cs) = flat (List.map compute_from_abstract cs)
    
  fun printIntList_aux [] = "]"
  | printIntList_aux [x] = (Int.toString x) ^ "]"
  | printIntList_aux (x::xs) = (Int.toString x) ^ "," ^ (printIntList_aux xs)
  (*Utility function for printing integer lists*)
  fun printIntListString xs = ("[" ^ (printIntList_aux xs))
  
  (*Check that the functions above work...*)
  val _ = Testing.test "Par_Tree: testing test functions" (fn () =>
    let
      (*Just a leaf case*)
      val mytree = Leaf 1;
      val expected = [1];
      val _ = if ((compute_from_abstract mytree)=expected) then () else
              raise ERROR "Par_Tree: failed test function 1";
      val (root,f) = tree_conv mytree;
      val _ = if ((simple_compute f root)=expected) then () else
              raise ERROR ("Par_Tree: failed test function 2 " ^ (printIntListString (simple_compute f root)));
      (*First node case*)
      val mytree = Node [Leaf 1];
      val expected = [1];
      val _ = if ((compute_from_abstract mytree)=expected) then () else
              raise ERROR "Par_Tree: failed test function 1";
      val (root,f) = tree_conv mytree;
      val _ = if ((simple_compute f root)=expected) then () else
              raise ERROR ("Par_Tree: failed test function 2 " ^ (printIntListString (simple_compute f root)));
      (*Deep node case*)
      val mytree = Node [Node [Node [Leaf 1]]];
      val expected = [1];
      val _ = if ((compute_from_abstract mytree)=expected) then () else
              raise ERROR "Par_Tree: failed test function 1";
      val (root,f) = tree_conv mytree;
      val _ = if ((simple_compute f root)=expected) then () else
              raise ERROR ("Par_Tree: failed test function 2 " ^ (printIntListString (simple_compute f root)));
      (*Multiple leaves case*)
      val mytree = Node [Node [Leaf 1,Leaf 2],Leaf 3,Leaf 4,Leaf 5];
      val expected = [1,2,3,4,5];
      val _ = if ((compute_from_abstract mytree)=expected) then () else
              raise ERROR "Par_Tree: failed test function 1";
      val (root,f) = tree_conv mytree;
      val _ = if ((simple_compute f root)=expected) then () else
              raise ERROR ("Par_Tree: failed test function 2 " ^ (printIntListString (simple_compute f root)));
      (*A general example*)
      val mytree = Node [Node [Leaf 1],Node [Leaf 2,Node [Leaf 3,Leaf 4]],Node [Leaf 5]];
      val expected = [1,2,3,4,5];
      val _ = if ((compute_from_abstract mytree)=expected) then () else
              raise ERROR "Par_Tree: failed test function 1";
      val (root,f) = tree_conv mytree;
      val _ = if ((simple_compute f root)=expected) then () else
              raise ERROR ("Par_Tree: failed test function 2 " ^ (printIntListString (simple_compute f root)));
    in
      ()
    end) ()
    
  (*Test the setting and getting of values. The parallel tree is not
  tested for various combinations of these, since these are fed to the
  compactor and so their correct use depends on that*)
  
  (*Test the future data...*)
  val _ = Testing.test "Par_Tree: testing future data setting" (fn () =>
    let
      val fd = Future_Wrapper.new_future_data (SOME "Hello world")
      val par_tree = Par_Tree.new fd;
      val _ = if ((Future_Wrapper.get_message (Par_Tree.get_future_data par_tree))=(SOME "Hello world")) then () else
              raise ERROR "Par_Tree: failed future data setting";
    in
      ()
    end) ()
  
  (*Test the group number*)
  val _ = Testing.test "Par_Tree: testing group number setting" (fn () =>
    let
      val par_tree = Par_Tree.new (fd());
      val par_tree = Par_Tree.set_no_groups par_tree (SOME 50);
      val _ = if ((Par_Tree.get_no_groups par_tree)=50) then () else
              raise ERROR "Par_Tree: failed group number setting";
    in
      ()
    end) ()
    
  (*Test the setting of the compactor*)
  val _ = Testing.test "Par_Tree: testing group number setting" (fn () =>
    let
      val par_tree = Par_Tree.new (fd());
      val par_tree = Par_Tree.set_no_groups par_tree (SOME 50);
      val _ = if ((Par_Tree.get_no_groups par_tree)=50) then () else
              raise ERROR "Par_Tree: failed group number setting";
    in
      ()
    end) ()
    
  (*Test the node limit*)
  val _ = Testing.test "Par_Tree: testing the compactor" (fn () =>
    let
      val par_tree = Par_Tree.new (fd());
      val comp = Compactor.new_dynamic (Time.fromMicroseconds 2000);
      val comp = Compactor.set_scaling comp (3,19);
      val par_tree = Par_Tree.set_compactor par_tree comp;
      val _ = if ((Compactor.get_scaling (Par_Tree.get_compactor par_tree))=(3,19)) then () else
              raise ERROR "Par_Tree: failed compactor setting";
    in
      ()
    end) ()
    
  (*Test the estimator setting*)
  val _ = Testing.test "Par_Tree: testing the estimator" (fn () =>
    let
      val par_tree = Par_Tree.new (fd());
      fun estimator _ = 37;
      val par_tree = Par_Tree.set_estimator par_tree (SOME estimator);
      val _ = if (((the (Par_Tree.get_estimator par_tree)) (Seq.empty,0,Seq.empty))=37) then () else
              raise ERROR "Par_Tree: failed estimator setting";
    in
      ()
    end) ()
    
  (*Test the order matters*)
  val _ = Testing.test "Par_Tree: testing the order matters" (fn () =>
    let
      val par_tree = Par_Tree.new (fd());
      val par_tree = Par_Tree.set_order_matters par_tree false;
      val _ = if (false=(Par_Tree.get_order_matters par_tree)) then () else
              raise ERROR "Par_Tree: failed order matters setting";
    in
      ()
    end) ()
  
  (*Test the computation of the tree itself - order important*)
  val _ = Testing.test "Par_Tree: testing compute tree" (fn () =>
    let
      val par_tree = Par_Tree.new (fd());
      (*The order is important here*)
      val par_tree = Par_Tree.set_order_matters par_tree true;
      fun test_tree mytree error_message = let
        val expected = compute_from_abstract mytree;
        val (root,f) = tree_conv mytree;
        val result = Seq.list_of (Par_Tree.compute_tree par_tree root f);
        val _ = if (result=expected) then () else
                raise ERROR error_message;
        in () end
      (*Just a leaf*)
      val mytree = Leaf 1;
      val _ = test_tree mytree "Par_Tree: compute tree test failed 1";
      (*One node case*)
      val mytree = Node [Leaf 1];
      val _ = test_tree mytree "Par_Tree: compute tree test failed 2";
      (*Deep node case*)
      val mytree = Node [Node [Node [Leaf 1]]];
      val _ = test_tree mytree "Par_Tree: compute tree test failed 3";
      (*Multiple leaves case*)
      val mytree = Node [Leaf 1,Leaf 2,Leaf 3];
      val _ = test_tree mytree "Par_Tree: compute tree test failed 4";
      (*Varying depth, multiple leaves case*)
      val mytree = Node [Leaf 1,Leaf 2,Node [Leaf 5, Node [Leaf 6], Leaf 7], Leaf 3,
                   Node [Leaf 8, Leaf 9]];
      val _ = test_tree mytree "Par_Tree: compute tree test failed 5";
      (*Just a larger complicated example*)
      val mytree = Node [Node [Node [Leaf 1,Node [Leaf 2, Leaf 3]],Node [Leaf 4,Leaf 5]],
                   Node [Leaf 6, Node [Node [Leaf 8],Node [Leaf 9],Leaf 10],Leaf 11],Leaf 12];
      val _ = test_tree mytree "Par_Tree: compute tree test failed 6";
    in
      ()
    end) ()
    
  (*This test is rather special in that if it is failed, rather
  than return an error it will run forever. This test ensures
  that the tree is truly lazy. The tree constructed has an infinitely
  deep right hand side of the root node.
  Note that the tree is only lazy in depth. It expects to be able to
  compute all of the children of a node when it asks for them, since it
  will parallelise this computation and so does not wish to receive
  a lazy sequence, as that would reduce parallelism (the type signature
  enforces this requirement to a degree).*)
  val _ = Testing.test "Par_Tree: testing tree laziness" (fn () =>
    let
      val par_tree = Par_Tree.new (fd());
      (*The order is important here*)
      val par_tree = Par_Tree.set_order_matters par_tree true;
      fun f 1 = [Par_Tree.Node 1] (*Causes an infinite number of nodes*)
        | f _ = [Par_Tree.Leaf 1,Par_Tree.Leaf 2,Par_Tree.Node 1];
      val root = Par_Tree.Node 0; (*Start of the tree*)
      val res = Par_Tree.compute_tree par_tree root f; (*Prepare it*)
      val (e1,rest) = case (Seq.pull res) of
            NONE => raise ERROR "Par_Tree: laziness failed pull 1"
          | (SOME x) => x; (*Pull the first two elements...*)
      val (e2,_) = case (Seq.pull rest) of
            NONE => raise ERROR "Par_Tree: laziness failed pull 2"
          | (SOME x) => x;
      val _ = if ([e1,e2]=[1,2]) then () else
              raise ERROR "Par_Tree: laziness test failed although the tree was lazy..."
    in
      ()
    end) ()
  
  (*Test nodes which produce no children*)
  val _ = Testing.test "Par_Tree: testing empty nodes" (fn () =>
    let
      (*Test an entirely empty tree*)
      val par_tree = Par_Tree.new (fd());
      (*The order is important here*)
      val par_tree = Par_Tree.set_order_matters par_tree true;
      val result = Seq.list_of (Par_Tree.compute_tree par_tree (Par_Tree.Node 1) (fn _ => []));
      val _ = if (result=[]) then () else
              raise ERROR "Par_Tree: failed empty node 1";
      (*Test a partially empty tree*)
      fun myfunc 1 = [Par_Tree.Leaf 1,Par_Tree.Node 2]
        | myfunc _ = [];
      val result = Seq.list_of (Par_Tree.compute_tree par_tree (Par_Tree.Node 1) myfunc);
      val _ = if (result=[1]) then () else
              raise ERROR "Par_Tree: failed empty node 2";
      (*Another test, this time with a node containing empty nodes*)
      fun myfunc 1 = [Par_Tree.Node 2,Par_Tree.Node 2]
        | myfunc 2 = [Par_Tree.Node 3,Par_Tree.Node 3]
        | myfunc _ = [];
      val result = Seq.list_of (Par_Tree.compute_tree par_tree (Par_Tree.Node 1) myfunc);
      val _ = if (result=[]) then () else
              raise ERROR "Par_Tree: failed empty node 3";
      (*Surround an empty node containing empty nodes with leaves*)
      fun myfunc 1 = [Par_Tree.Leaf 1,Par_Tree.Node 2,Par_Tree.Leaf 2,Par_Tree.Node 2,Par_Tree.Leaf 3]
        | myfunc 2 = [Par_Tree.Node 3,Par_Tree.Node 3]
        | myfunc _ = [];
      val result = Seq.list_of (Par_Tree.compute_tree par_tree (Par_Tree.Node 1) myfunc);
      val _ = if (result=[1,2,3]) then () else
              raise ERROR "Par_Tree: failed empty node 4";
      (*A more complex example...*)
      fun myfunc 1 = [Par_Tree.Node 2,Par_Tree.Leaf 1,Par_Tree.Node 3,Par_Tree.Leaf 2]
        | myfunc 2 = [Par_Tree.Leaf 4,Par_Tree.Node 4,Par_Tree.Node 3,Par_Tree.Node 4,Par_Tree.Leaf 7]
        | myfunc 3 = [Par_Tree.Leaf 5,Par_Tree.Leaf 6]
        | myfunc _ = []
      val result = Seq.list_of (Par_Tree.compute_tree par_tree (Par_Tree.Node 1) myfunc);
      val _ = if (result=[4,5,6,7,1,5,6,2]) then () else
              raise ERROR "Par_Tree: failed empty node 5";
    in
      ()
    end) ()
    
    (****************************************************************)
    (*These tests are for when the order of the leaves returned is not
    important. They use a simple set structure stored here.*)
    (****************************************************************)
    
    structure Set =
    struct
      type ''a set = ''a list (*Very basic...*)
      (*Check to see if an element exists in the set*)
      fun contains _ [] = false
        | contains x (y::ys) = if (x=y) then true else contains x ys
      (*Add an element to the set*)
      fun add x s = if (contains x s) then s else x::s
      (*Remove an element from a set, if it exists*)
      fun remove x s = filter (fn y => not (x=y)) s
      (*See if two sets are equal*)
      fun equal [] s2 = if (s2=[]) then true else false
        | equal (x::xs) s = if (contains x s) then
          equal xs (remove x s)
          else false
      (*Create a set from a list*)
      fun of_list [] = []
        | of_list (x::xs) = add x (of_list xs)
      (*Turn a set into a list*)
      fun list_of s = s
    end;
    
    (*Test the computation of the tree again. Note that the
    tests need to be redone, because the implementation is significantly
    different.*)
    val _ = Testing.test "Par_Tree: testing compute tree" (fn () =>
    let
      val par_tree = Par_Tree.new (fd());
      (*The order is NOT important here*)
      val par_tree = Par_Tree.set_order_matters par_tree false;
      fun test_tree mytree error_message = let
        val expected = compute_from_abstract mytree;
        val (root,f) = tree_conv mytree;
        val result = Seq.list_of (Par_Tree.compute_tree par_tree root f);
        val _ = if (Set.equal (Set.of_list result) (Set.of_list expected)) then () else
                raise ERROR error_message;
        in () end
      (*Just a leaf*)
      val mytree = Leaf 1;
      val _ = test_tree mytree "Par_Tree: compute tree test failed 1";
      (*One node case*)
      val mytree = Node [Leaf 1];
      val _ = test_tree mytree "Par_Tree: compute tree test failed 2";
      (*Deep node case*)
      val mytree = Node [Node [Node [Leaf 1]]];
      val _ = test_tree mytree "Par_Tree: compute tree test failed 3";
      (*Multiple leaves case*)
      val mytree = Node [Leaf 1,Leaf 2,Leaf 3];
      val _ = test_tree mytree "Par_Tree: compute tree test failed 4";
      (*Varying depth, multiple leaves case*)
      val mytree = Node [Leaf 1,Leaf 2,Node [Leaf 5, Node [Leaf 6], Leaf 7], Leaf 3,
                   Node [Leaf 8, Leaf 9]];
      val _ = test_tree mytree "Par_Tree: compute tree test failed 5";
      (*Just a larger complicated example*)
      val mytree = Node [Node [Node [Leaf 1,Node [Leaf 2, Leaf 3]],Node [Leaf 4,Leaf 5]],
                   Node [Leaf 6, Node [Node [Leaf 8],Node [Leaf 9],Leaf 10],Leaf 11],Leaf 12];
      val _ = test_tree mytree "Par_Tree: compute tree test failed 6";
    in
      ()
    end) ()
    
  (*This test is rather special in that if it is failed, rather
  than return an error it will run forever. This test ensures
  that the tree is truly lazy. The tree constructed has an infinitely
  deep right hand side of the root node.
  Note that the tree is only lazy in depth. It expects to be able to
  compute all of the children of a node when it asks for them, since it
  will parallelise this computation and so does not wish to receive
  a lazy sequence, as that would reduce parallelism (the type signature
  enforces this requirement to a degree).*)
  val _ = Testing.test "Par_Tree: testing tree laziness" (fn () =>
    let
      val par_tree = Par_Tree.new (fd());
      (*The order is important here*)
      val par_tree = Par_Tree.set_order_matters par_tree true;
      fun f 1 = [Par_Tree.Node 1] (*Causes an infinite number of nodes*)
        | f _ = [Par_Tree.Leaf 1,Par_Tree.Leaf 2,Par_Tree.Node 1];
      val root = Par_Tree.Node 0; (*Start of the tree*)
      val res = Par_Tree.compute_tree par_tree root f; (*Prepare it*)
      val (e1,rest) = case (Seq.pull res) of
            NONE => raise ERROR "Par_Tree: laziness failed pull 1"
          | (SOME x) => x; (*Pull the first two elements...*)
      val (e2,_) = case (Seq.pull rest) of
            NONE => raise ERROR "Par_Tree: laziness failed pull 2"
          | (SOME x) => x;
      val _ = if (Set.equal (Set.of_list [e1,e2]) (Set.of_list [1,2])) then () else
              raise ERROR "Par_Tree: laziness test failed although the tree was lazy..."
    in
      ()
    end) ()
    
  (*Test the recursive node function at various depths to
  ensure that it is computing children correctly...*)
  val _ = Testing.test "Par_Tree: testing recursive node function" (fn () =>
    let
      val root = 1;
      (*The original node function*)
      fun node_func 1 = [Par_Tree.Node 2,Par_Tree.Node 3, Par_Tree.Leaf 1]
        | node_func 2 = [Par_Tree.Node 4,Par_Tree.Node 4]
        | node_func 3 = [Par_Tree.Leaf 2]
        | node_func _ = [Par_Tree.Leaf 3];
      (*For printing lists of leaves and nodes...*)
      fun toString' [] = "]"
        | toString' [Par_Tree.Leaf x] = "Leaf " ^ (Int.toString(x)) ^ "]"
        | toString' [Par_Tree.Node x] = "Node " ^ (Int.toString(x)) ^ "]"
        | toString' ((Par_Tree.Leaf x)::ls) = "Leaf " ^ (Int.toString(x)) ^ "," ^ (toString' ls)
        | toString' ((Par_Tree.Node x)::ls) = "Node " ^ (Int.toString(x)) ^ "," ^ (toString' ls)
      fun toString ls = "[" ^ (toString' ls)
      (*Now test the various coarseness levels*)
      (*Coarseness of 1*)
      fun f n = (Par_Tree.recur_node_function node_func 1) n;
      val _ = if ((f root)=[Par_Tree.Node 2,Par_Tree.Node 3, Par_Tree.Leaf 1]) then () else
              raise ERROR ("Par_Tree: node recursion failed 1 with " ^ (toString (f root)));
      (*Coarseness of 2*)
      fun f n = (Par_Tree.recur_node_function node_func 2) n;
      val _ = if ((f root)=[Par_Tree.Node 4,Par_Tree.Node 4,Par_Tree.Leaf 2,Par_Tree.Leaf 1]) then () else
              raise ERROR ("Par_Tree: node recursion failed 2 with " ^ (toString (f root)));
      (*Coarseness of 3*)
      fun f n = (Par_Tree.recur_node_function node_func 3) n;
      val _ = if ((f root)=[Par_Tree.Leaf 3,Par_Tree.Leaf 3,Par_Tree.Leaf 2,Par_Tree.Leaf 1]) then () else
              raise ERROR ("Par_Tree: node recursion failed 3 with " ^ (toString (f root)));
    in
      ()
    end) ()
  
  (*Test nodes which produce no children*)
  val _ = Testing.test "Par_Tree: testing empty nodes" (fn () =>
    let
      (*Test an entirely empty tree*)
      val par_tree = Par_Tree.new (fd());
      (*The order is NOT important here*)
      val par_tree = Par_Tree.set_order_matters par_tree false;
      val result = Seq.list_of (Par_Tree.compute_tree par_tree (Par_Tree.Node 1) (fn _ => []));
      val _ = if (result=[]) then () else
              raise ERROR "Par_Tree: failed empty node 1";
      (*Test a partially empty tree*)
      fun myfunc 1 = [Par_Tree.Leaf 1,Par_Tree.Node 2]
        | myfunc _ = [];
      val result = Seq.list_of (Par_Tree.compute_tree par_tree (Par_Tree.Node 1) myfunc);
      val _ = if (Set.equal (Set.of_list result) (Set.of_list [1])) then () else
              raise ERROR "Par_Tree: failed empty node 2";
      (*Another test, this time with a node containing empty nodes*)
      fun myfunc 1 = [Par_Tree.Node 2,Par_Tree.Node 2]
        | myfunc 2 = [Par_Tree.Node 3,Par_Tree.Node 3]
        | myfunc _ = [];
      val result = Seq.list_of (Par_Tree.compute_tree par_tree (Par_Tree.Node 1) myfunc);
      val _ = if (result=[]) then () else
              raise ERROR "Par_Tree: failed empty node 3";
      (*Surround an empty node containing empty nodes with leaves*)
      fun myfunc 1 = [Par_Tree.Leaf 1,Par_Tree.Node 2,Par_Tree.Leaf 2,Par_Tree.Node 2,Par_Tree.Leaf 3]
        | myfunc 2 = [Par_Tree.Node 3,Par_Tree.Node 3]
        | myfunc _ = [];
      val result = Seq.list_of (Par_Tree.compute_tree par_tree (Par_Tree.Node 1) myfunc);
      val _ = if (Set.equal (Set.of_list result) (Set.of_list [1,2,3])) then () else
              raise ERROR "Par_Tree: failed empty node 4";
      (*A more complex example...*)
      fun myfunc 1 = [Par_Tree.Node 2,Par_Tree.Leaf 1,Par_Tree.Node 3,Par_Tree.Leaf 2]
        | myfunc 2 = [Par_Tree.Leaf 4,Par_Tree.Node 4,Par_Tree.Node 3,Par_Tree.Node 4,Par_Tree.Leaf 7]
        | myfunc 3 = [Par_Tree.Leaf 5,Par_Tree.Leaf 6]
        | myfunc _ = []
      val result = Seq.list_of (Par_Tree.compute_tree par_tree (Par_Tree.Node 1) myfunc);
      val _ = if (Set.equal (Set.of_list result) (Set.of_list [4,5,6,7,1,5,6,2])) then () else
              raise ERROR "Par_Tree: failed empty node 5";
    in
      ()
    end) ()
    
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR PARALLEL TREE PASSED!"
  
  structure Test_ParallelTreeSpeed =
  struct
    structure Par_Tree = Parallel_Tree;
    structure Future = Future_Wrapper;
    structure File = SimpleFileIO;
    
    (*A structure for handling the file used during testing*)
    structure File_Aux =
    struct
      (*The default file to use*)
      val file = Unsynchronized.ref "../../isaplib/Concurrent/test/par_tree-results.txt";
      (*Open the file and be ready to append it...*)
      val is_open = Unsynchronized.ref NONE;
      (*Get the file*)
      fun get_file() = case (!is_open) of
          NONE => raise ERROR "cannot get the file"
        | (SOME f) => f
      (*Test to see if the file is currently open*)
      fun is_file_open() = not (is_none (!is_open))
      (*Open and return the file for appending*)
      fun append() = if (is_file_open()) then get_file()
        else let val ans = File.append (!file) in
          (is_open:=SOME ans;ans) end
      (*Return the file for writing. Wipes out everything
      previously in the file*)
      fun write() = if (is_file_open()) then
        (File.close (get_file());let val ans = File.write (!file) in
          (is_open:=SOME ans;ans) end) else
          let val ans = File.write (!file) in (is_open:=SOME ans;ans) end
      (*Close the file*)
      fun close() = if (is_file_open()) then
          (File.close (get_file());is_open:=NONE;())
        else ()
      (*Remove all the contents in the file.
      This leaves the file closed.*)
      fun renew() =
        (
        if (is_file_open()) then close() else ();
        write();
        close()
        )
    end;
    
    structure Help =
    struct
      (*This function produces trees. All of the leaves are the same
      which makes it unfortunate for test cases, but
      it enables the production of large trees!
      The number of branches at each level is specified by x,
      and it will continue to produce that many children
      until it reaches 0.*)
      fun tree_producer x n =
          if (n=0) then [(Par_Tree.Leaf true)]
          else let fun repeat 0 = []
                     | repeat x = (Par_Tree.Node (n-1))::(repeat (x-1))
               in (repeat x) end
      
      (*Returns the amount of time it took f to run
      in microseconds (LargeInt)*)
      fun time f =
        let
          val sw = Stopwatch.new;
          val sw = Stopwatch.start sw;
          val _ = f();
          val (t,_) = Stopwatch.stop sw
        in
          Time.toMicroseconds t
        end
    end;
    
    structure Tests =
    struct
      (*These parameters control which algorithm is allowed to run*)
      val both = 0;
      val parallel_only = 1;
      val sequential_only = 2;
      (*This is an equivalent sequential algorithm to process the tree.
      This will traverse the tree left first, in order, to find leaves one
      at a time.*)
      fun sequential_traverse f root = Seq.make (fn () =>
        case root of (Par_Tree.Leaf v) => SOME (v,Seq.empty)
        | (Par_Tree.Node v) => Seq.pull (Seq.maps (sequential_traverse f) (Seq.of_list (f v))))
      
      (*This times the sequential and parallel algorithms in the most
      generic way.
      
      est = estimate for the number of nodes which need to be combined
      into a group.
      time = the time a single group assigned to a core should take
      no_groups = the number of groups to try and create
      hardness = the difficulty in computing the elements of a node
      branches = the number of branches every node should have
      depth = how deep the tree should be (a depth of zero will create a leaf) 
      *)
      fun generic_test ordermatters comp no_groups hardness branches depth who_runs =
        let
          val par_tree = Par_Tree.new (fd());
          val par_tree = Par_Tree.set_order_matters par_tree ordermatters;
          val par_tree = Par_Tree.set_no_groups par_tree no_groups;
          val par_tree = Par_Tree.set_compactor par_tree comp;
          val par_tree = Par_Tree.set_node_limit par_tree NONE;
          fun func x = (gen_func hardness ();Help.tree_producer branches x);
          val root = (Par_Tree.Node depth);
          val parallel = fn () => Seq.list_of (Par_Tree.compute_tree par_tree root func);
          val sequential = fn () => Seq.list_of (sequential_traverse func root);
          (*Time the two functions*)
          val t_par = if (not (sequential_only=who_runs)) then (Help.time parallel) else 0;
          val t_seq = if (not (parallel_only=who_runs)) then (Help.time sequential) else 0;
          (*Print the results*)
          val file = File_Aux.append();
        in
          ((*Now print the results...*)
          File.writeStr file
          (" Group No: " ^ (Int.toString(Par_Tree.get_no_groups par_tree)) ^
           " Hardness: " ^ (Int.toString(hardness)) ^
           " Branches: " ^ (Int.toString(branches)) ^
           " Depth: " ^ (Int.toString(depth)) ^
           (if (not (parallel_only=who_runs)) then (" Sequential: " ^ (LargeInt.toString(t_seq))) else "") ^
           (if (not (sequential_only=who_runs)) then (" Parallel: " ^ (LargeInt.toString(t_par))) else "") ^ "\n")
              ;())
        end;
        
      (*Run a series of tests on the tree using the given depth, number of branches
      and varying the hardness of each run. Hardness values are extracted
      from the given list*)
      fun hardness_test ordermatters branches depth hardnesses who_runs =
        (
        File_Aux.renew();
        List.map (fn h => generic_test ordermatters (Par_Tree.get_compactor (Par_Tree.new (fd()))) NONE h branches depth who_runs) hardnesses;
        File_Aux.close()
        )
      infix ---;
      val hardness_list = (1---5)@(1---10)@(gen_list 9 20 (fn x => x + 10))@
          (gen_list 9 200 (fn x => x + 100))@(gen_list 9 2000 (fn x => x + 1000))
          
      fun run_hardness_test() = hardness_test false 2 18 hardness_list both;
      
      (*Similar to the hardness test except that this time the depth
      varies, allowing for more varying numbers of leaves*)
      fun depth_test ordermatters branches depths hardness who_runs =
        (
        File_Aux.renew();
        List.map (fn d => generic_test ordermatters (Par_Tree.get_compactor (Par_Tree.new (fd()))) NONE hardness branches d who_runs) depths;
        File_Aux.close()
        )
      
      (*This is used for a test against a tree with a random number of children
      per node, up to a fixed depth! Uses fixed hardness*)
      fun test_random_children_tree2 (low,high) depth (low_h,high_h) =
        let
          (*To do this, I will need to construct the random tree first,
          and then process it in parallel and then sequentially...*)
          fun gen_tree d = if (0=d) then [Par_Tree.Leaf 1]
            else repeat_ls (Par_Tree.Node (d-1)) (random low high);
          (*Now create the tree...
          The tree will be constructed using pointers so that the node function
          can figure out where it is...*)
          datatype ('a,'b) thing = Next of ('a * ((('a,'b) thing) List.list)) | Stop of 'b
          fun create_tree (Par_Tree.Node d) = Next (Par_Tree.Node (random low_h high_h),(map create_tree (gen_tree d)))
            | create_tree (Par_Tree.Leaf v) = Stop (Par_Tree.Leaf v)
          (*The tree to be computed...*)  
          val my_tree = create_tree (Par_Tree.Node depth);
          (*Now to work out the node function...
          Each node produced must have a reference to the next node in the above tree.
          Otherwise it won't be possible to figure out how to compute the next element
          *)
          fun to_map (Next (Par_Tree.Node d,next_kids)) = Par_Tree.Node (d,next_kids)
            | to_map (Stop (Par_Tree.Leaf v)) = Par_Tree.Leaf v
            | to_map _ = raise ERROR "Tree mess!";
          
          fun apply_to_node (chosen_h,children) = (gen_func chosen_h ();map to_map (children))
          (*The root function...*)
          val root = case (my_tree) of
              Stop (Par_Tree.Leaf v) => Par_Tree.Leaf v
            | Next (Par_Tree.Node d,children) => Par_Tree.Node (d,children)
            | _ => raise ERROR "Tree mess!";
          (*The parallel tree*)
          val par_tree = Par_Tree.new (fd());
          val par_tree = Par_Tree.set_node_limit par_tree NONE;
          (*Now apply the parallel and sequential algorithms!*)
          val _ = PolyML.print("Running parallel. (" ^ (Int.toString(low_h)) ^ "," ^ (Int.toString(high_h)) ^ ")");
          val t_par = time (fn () => Seq.list_of (Par_Tree.compute_tree par_tree root apply_to_node));
          val _ = PolyML.print("Running sequential. (" ^ (Int.toString(low_h)) ^ "," ^ (Int.toString(high_h)) ^ ")");
          val t_seq = time (fn () => Seq.list_of (sequential_traverse apply_to_node root));
          val file = File_Aux.append();
        in
          (File.writeStr file
            ("Low hardness: " ^ (Int.toString(low_h)) ^
            " High hardness: " ^ (Int.toString(high_h)) ^
            " Depth: " ^ (Int.toString(depth)) ^
            " Low: " ^ (Int.toString(low)) ^
            " High: " ^ (Int.toString(high)) ^
            " Sequential: " ^ (LargeInt.toString(t_seq)) ^
            " Parallel: " ^ (LargeInt.toString(t_par)) ^ "\n");())
        end
      
      (*Apply the random test over nodes with varying hardness
      over the randomly constructed trees*)
      fun test_random_children_tree (low,high) depth hardnesses =(
        File_Aux.renew();
        List.map (fn h => test_random_children_tree2 (low,high) depth h) hardnesses;
        File_Aux.close())
      
      (*Run the random children test tree!*)
      fun run_random_children () = test_random_children_tree (1,5) 10 (
      map (fn x => (x,x)) [1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300,400,500,
      600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000])
      
      fun run_random_all () = test_random_children_tree (1,5) 10 (
      map (fn x => (1,x * 2)) [1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300,400,500,
      600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000])
      
      fun run_constant () = test_random_children_tree (2,2) 10 (
      map (fn x => (x,x)) [1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300,400,500,
      600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000])
    end;
  end;
end;
