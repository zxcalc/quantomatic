signature CONTOUR =
sig
  type T
  structure G : BANG_GRAPH
  structure L : LITERAL

  (* CONSTRUCTORS *)
  val empty : T
  val mk : G.T -> T -> T
  val mk_first_contour : G.T -> V.name -> T
  val target_function : G.T -> T
  val add_literal : T -> L.T -> T
  
  (* GETTERS *)
  val get_first_literal : T -> L.T 
  
  val get_contour_mult_none : T -> T
  val get_contour_mult_star : T -> T
  val get_contour_mult_qm : T -> T
  val get_contour_mult_star_or_qm : T -> T

  (* COMPARISON FUNCTIONS *)
  val eq : T * T -> bool
  val equiv : T * T -> bool
  val check_strong_compatibility : T * T -> bool
  val check_weak_compatibility : T * T -> bool

  (* COMPARISON-EQUALITY FUNCTIONS *)
  val subtract_eq_contour: T * T -> T
  val intersect_eq_contours: T * T -> T
  val complement_eq_contour: T * T -> T
  val remove_eq_duplicate : T -> T
  val is_eq_literal_contained : L.T * T -> bool
  
  (* COMPARISON-MATCHING FUNCTIONS *)
  val is_matching_contour_contained : T * T -> bool
  
  (* PRINT FUNCTION *)
  val printout : T -> string

end



functor Contour ( G : BANG_GRAPH ) : CONTOUR =
struct

  structure G = G
  structure L = Literal(G)

  type T = L.T list;

  val empty = [];


  (* Generic library helper function *)
  fun maps2 _ [] [] = []
    | maps2 f (x::xs) (y::ys) = f x y @ maps2 f xs ys
    | maps2 _ _ _ = raise ListPair.UnequalLengths;


 
  (* This function gets the first node in a contour *)
  fun get_first_literal(l::[]) = l
    | get_first_literal(l::literals) = l

  (* This function returns a contout containing only the literal with multiplicity equal to None *)
  fun get_contour_mult_none([]) = empty
    | get_contour_mult_none(l::[]) = if L.is_kind_mult_none(l) then l::[] else []
    | get_contour_mult_none(l::ls) = if L.is_kind_mult_none(l) then l::get_contour_mult_none(ls) else get_contour_mult_none(ls)

  (* This function returns a contout containing only the literal with multiplicity equal to Star *)
  fun get_contour_mult_star([]) = empty
    | get_contour_mult_star(l::[]) = if L.is_kind_mult_star(l) then l::[] else []
    | get_contour_mult_star(l::ls) = if L.is_kind_mult_star(l) then l::get_contour_mult_star(ls) else get_contour_mult_star(ls)

  (* This function returns a contout containing only the literal with multiplicity equal to QM *)
  fun get_contour_mult_qm([]) = empty
    | get_contour_mult_qm(l::[]) = if L.is_kind_mult_qm(l) then l::[] else []
    | get_contour_mult_qm(l::ls) = if L.is_kind_mult_qm(l) then l::get_contour_mult_qm(ls) else get_contour_mult_qm(ls)     

  (* This function returns a contout containing only the literal with multiplicity equal to Star or QM *)
  fun get_contour_mult_star_or_qm(c) = flat (get_contour_mult_star(c) :: get_contour_mult_qm(c) :: [])

  


  
  (* This function finds if a literal l is contained in a contour c *)
  fun is_eq_literal_contained(l,[]) = false
    | is_eq_literal_contained(l,c::[]) = if L.eq(l,c) then true else false
    | is_eq_literal_contained(l,c::cs) = if L.eq(l,c) then true else is_eq_literal_contained(l,cs)

  (* This function removes a literal l from a contour c, given that l is contained in c *)
  (* PRECONDITION: is_eq_literal_contained(l,c) = true *)
  fun remove_eq_literal_in_contour(l,c::[]) = []
    | remove_eq_literal_in_contour(l,c::cs) = if L.eq(l,c) then cs else c::remove_eq_literal_in_contour(l,cs)

  (* This function subtracts c2 from c1, c1-c2, given that c2 is contained in c1 *)
  (* PRECONDITION: c2 is contained in c1 *)
  fun subtract_eq_contour([],[]) = []
    | subtract_eq_contour(c1::[],[]) = c1::[]
    | subtract_eq_contour(c1::c1s,[]) = c1::c1s
    | subtract_eq_contour(c1::[],c2::[]) = []
    | subtract_eq_contour(c1::c1s,c2::c2s) = subtract_eq_contour(remove_eq_literal_in_contour(c2,c1::c1s),c2s)

  (* This function finds the contour given by the intersection of two contours c1 and c2 *)
  (* PRECONDITION: remove_eq_duplicate() has been executed *)
  fun intersect_eq_contours([],_) = []
    | intersect_eq_contours(_,[]) = []
    | intersect_eq_contours(c1::[],c2::[]) = if L.eq(c1,c2) then c1::[] else []
    | intersect_eq_contours(c1::[],c2::c2s) = if is_eq_literal_contained(c1,c2::c2s) then c1::[] else []
    | intersect_eq_contours(c1::c1s,c2::[]) = if L.eq(c1,c2) then c1::[] else intersect_eq_contours(c1s,c2::[])
    | intersect_eq_contours(c1::c1s,c2::c2s) = if is_eq_literal_contained(c1,c2::c2s) then c1::intersect_eq_contours(c1s,remove_eq_literal_in_contour(c1,c2::c2s)) else intersect_eq_contours(c1s,c2::c2s)  

  (* This function computes the complement of c1 wrt c2, c2 - (c1 intersection c2) *)
  fun complement_eq_contour(c1,c2) = subtract_eq_contour(c2, intersect_eq_contours(c1,c2))

  (* This function removes duplicates in a contour c *)
  fun remove_eq_duplicate([]) = []
    | remove_eq_duplicate(c::[]) = c::[]
    | remove_eq_duplicate(c::cs) = if is_eq_literal_contained(c,cs) then remove_eq_duplicate(cs) else c::remove_eq_duplicate(cs)

  (* This function checks if a literal l is contained in the contour c*)
  fun find_eq_literal(l,[]) = false
    | find_eq_literal(l,c::[]) = if L.eq(l,c) then true else false
    | find_eq_literal(l,c::cs) = if L.eq(l,c) then true else find_eq_literal(l,cs)

  (* This function checks if two contours c1 and c2 are equal*)
  fun is_eq_contour([],[]) = true
    | is_eq_contour(c1::[],[]) = false
    | is_eq_contour([],c2::[]) = false
    | is_eq_contour(c1::c1s,[]) = false
    | is_eq_contour([],c2::c2s) = false
    | is_eq_contour(c1::[],c2::[]) = if L.eq(c1,c2) then true else false
    | is_eq_contour(c1::c1s,c2::[]) = false
    | is_eq_contour(c1::[],c2::c2s) = false
    | is_eq_contour(c1::c1s,c2::c2s) = 
      if L.eq(c1,c2) then is_eq_contour (c1s,c2s)
      else if find_eq_literal(c1,c2s) then is_eq_contour(c1s,c2::remove_eq_literal_in_contour(c1,c2s)) else false

  (* This function checks for equality between c1 and c2, that is if c1 and c2 are equal contours *)
  (* TRUE iff c1 and c2 contains equal literals in any order; they must be generated by the same node-vertices *)
  (* EQUALITY is used when building contours in order to build a single literal for every node-vertex *)    
  fun eq (c1,c2) =
    if (length c1 = length c2) then is_eq_contour(c1,c2)
    else false





  (* This function finds if a literal l is contained in a contour c *)
  fun is_equiv_literal_contained(l, []) = false
    | is_equiv_literal_contained(l,c::[]) = if L.equiv(l,c) then true else false
    | is_equiv_literal_contained(l,c::cs) = if L.equiv(l,c) then true else is_equiv_literal_contained(l,cs)

  (* This function removes a literal l from a contour c, given that l is contained in c *)
  (* PRECONDITION: is_equiv_literal_contained(l,c) = true *)
  fun remove_equiv_literal_in_contour(l,c::[]) = []
    | remove_equiv_literal_in_contour(l,c::cs) = if L.equiv(l,c) then cs else c::remove_equiv_literal_in_contour(l,cs)

  (* This function checks if a literal l is contained in the contour c*)
  fun find_equiv_literal(c1,[]) = false
    | find_equiv_literal(c1,c2::[]) = if L.equiv(c1,c2) then true else false
    | find_equiv_literal(c1,c2::c2s) = if L.equiv(c1,c2) then true else find_equiv_literal(c1,c2s)

  (* This function checks if two contours c1 and c2 are equivalent *)
  fun is_equiv_contour([],[]) = true
    | is_equiv_contour(c1::[],[]) = false
    | is_equiv_contour([],c2::[]) = false
    | is_equiv_contour(c1::c1s,[]) = false
    | is_equiv_contour([],c2::c2s) = false
    | is_equiv_contour(c1::[],c2::[]) = if L.equiv(c1,c2) then true else false
    | is_equiv_contour(c1::c1s,c2::[]) = false
    | is_equiv_contour(c1::[],c2::c2s) = false
    | is_equiv_contour(c1::c1s,c2::c2s) = 
      if L.equiv(c1,c2) then is_equiv_contour (c1s,c2s)
      else if find_equiv_literal(c1,c2s) then is_equiv_contour(c1s,c2::remove_equiv_literal_in_contour(c1,c2s)) else false

  (* This function checks for equivalence between c1 and c2, that is if c1 and c2 are equivalent contours *)
  (* TRUE iff c1 and c2 contains equivalent literals in any order; they can be generated by different node-vertices *)
  (* EQUIVALENCE is used when building the tree in order identify contours which are equivalent and share a node in the tree *)    
  fun equiv (c1,c2) =
    if (length c1 = length c2) then is_equiv_contour(c1,c2)
    else false









  (* HELPER FUNCTIONS to build a literal *)
  (* This functions checks if all the literals l in a contour are in a bang-box *)
  fun all_bang(g, []) = false
      | all_bang(g, l::[]) = G.is_bboxed g (L.get_name(l))
      | all_bang(g, l::ls) =
        if G.is_bboxed g (L.get_name(l)) then all_bang(g, ls)
        else false

  (* This functions checks if there is a literals l in a contour which is in a bang-box *)
    fun exists_bang(g, []) = false
      | exists_bang (g, l::[]) = G.is_bboxed g (L.get_name(l))
      | exists_bang (g, l::ls) =
        if G.is_bboxed g (L.get_name(l)) then true
        else exists_bang(g,ls)

  (* This function recomputes the multiplicity of a literal l in a graph g given the previous contour pc *)
  fun set_kind_mult(g,pc,l) = 
      let
        val adj = L.get_adj g l
        val adj_in_contour = intersect_eq_contours(pc,adj)
      in
        if
          G.is_bboxed g (L.get_name(l)) then L.mult_star
          else if all_bang(g, adj_in_contour) then L.mult_qm
               else L.mult_none
      end

  (* This function recomputes the input arity evaluating the number of bang-boxes in the graph g having out-edges going to the literal l*)
  fun set_input_arity(g,l) =
      let
        val pred = L.get_pred g l

        val bboxed = G.get_bboxed(g)
        val bboxed = V.NSet.list_of(bboxed)  
        val g_list = replicate (length bboxed) g
        val bboxed = map2 L.mk g_list bboxed

        val pred = complement_eq_contour(bboxed,pred)
      in
        length pred
      end

  (* This function recomputes the input multiplicity of a literal l in a graph g given the previous contour pc *)
  fun set_input_mult(g,pc,l) = 
      let
        val prec = L.get_pred g l
      in
        if exists_bang(g,prec) then L.mult_star
        else L.mult_none
      end

  (* This function recomputes the output arity evaluating the number of bang-boxes in the graph g having in-edges coming from the literal l*)
  fun set_output_arity(g,l) =
      let
        val succ = L.get_succ g l

        val bboxed = G.get_bboxed(g)
        val bboxed = V.NSet.list_of(bboxed)  
        val g_list = replicate (length bboxed) g
        val bboxed = map2 L.mk g_list bboxed

        val succ = complement_eq_contour(bboxed,succ)
      in
        length succ
      end

  (* This function recomputes the input multiplicity of a literal l in a graph g given the previous contour pc *)
  fun set_output_mult(g,pc,l) =
      let
        val succ = L.get_succ g l
      in
        if exists_bang(g,succ) then L.mult_star
        else L.mult_none
     end

  (* This function rebuilds the literals ls in a graph g recomputing arities and multiplicities according to the previous contour pc *)
  fun rebuild_literals(_,_,[]) = []
    | rebuild_literals(g,pc,l::[]) = 
       L.build ( L.get_name(l), L.get_kind(l), set_kind_mult(g,pc,l), set_input_arity(g,l), set_input_mult(g,pc,l), set_output_arity(g,l), set_output_mult(g,pc,l) ) :: [] 
    | rebuild_literals(g,pc,l::ls) =
       L.build ( L.get_name(l), L.get_kind(l), set_kind_mult(g,pc,l), set_input_arity(g,l), set_input_mult(g,pc,l), set_output_arity(g,l), set_output_mult(g,pc,l) ) :: rebuild_literals(g,pc,ls)

  

  (* This function builds the first contour given a graph g *)
  fun target_function(g) =
      let
        val vnames = G.get_unbboxed(g)
        val v::vlist = V.NSet.list_of(vnames)        
        val l = L.mk g v
        val ls = rebuild_literals(g,[],l::[])
      in
        ls
      end

  (* This function build the first contour using the node-vertex v *)
  fun mk_first_contour g v =
      let
        val l = L.mk g v
        val ls = rebuild_literals(g,[],l::[])
      in
        ls
      end 

  (* This function builds a contour starting from another contour c *)
  fun mk g c = 
        let
          val g_list = replicate (length c) g
          val gl_list = g_list ~~ c

          val contour = maps2 L.get_adj g_list c
          val contour = remove_eq_duplicate(contour)
          val contour = complement_eq_contour(c,contour)
        in
          rebuild_literals(g,c,contour)
        end


  (* This function adds the literal l to the contour c *)
  fun add_literal c l = l::c

  



  (* This function finds if a literal l is contained in a contour c *)
  fun is_matching_literal_contained(l, []) = false
    | is_matching_literal_contained(l,c::[]) = if L.match(l,c) then true else false
    | is_matching_literal_contained(l,c::cs) = if L.match(l,c) then true else is_matching_literal_contained(l,cs)

  (* This function removes a literal l from a contour c, given that l is contained in c *)
  (* PRECONDITION: is_matching_literal_contained(l,c) = true *)
  fun remove_matching_literal_in_contour(l,c::[]) = []
    | remove_matching_literal_in_contour(l,c::cs) = if L.match(l,c) then cs else c::remove_matching_literal_in_contour(l,cs)

  (* This function removes a matching literal from a contour considering its multiplicity *)
  (* PRECONDITION: is_matching_literal_contained(l,c) = true *)
  fun remove_matching_literal_in_contour_according_to_mult(l,c::[]) = 
      if(L.is_kind_mult_star(c)) then c::[] else []
    | remove_matching_literal_in_contour_according_to_mult(l,c::cs) = 
      if L.match(l,c) then 
        if(L.is_kind_mult_star(c)) then c::cs else cs 
      else c::remove_matching_literal_in_contour_according_to_mult(l,cs)  

  (* This function subtracts c2 from c1, c1-c2, given that c2 is contained in c1 *)
  (* PRECONDITION: c2 is contained in c1 *)
  fun subtract_matching_contour([],[]) = []
    | subtract_matching_contour(c1::[],[]) = c1::[]
    | subtract_matching_contour(c1::c1s,[]) = c1::c1s
    | subtract_matching_contour(c1::[],c2::[]) = []
    | subtract_matching_contour(c1::c1s,c2::c2s) = subtract_matching_contour(remove_matching_literal_in_contour(c2,c1::c1s),c2s)

  (* This function checks if every literal in c1 is matched in c2 *)
  fun is_matching_contour_contained([], []) = true
    | is_matching_contour_contained(c1::[], []) = false
    | is_matching_contour_contained([], c2::[]) = true
    | is_matching_contour_contained([], c2::c2s) = true
    | is_matching_contour_contained(c1::[],c2::[]) = if L.match(c1,c2) then true else false
    | is_matching_contour_contained(c1::[],c2::c2s) = if is_matching_literal_contained(c1,c2::c2s) then true else false
    | is_matching_contour_contained(c1::c1s,[]) = false
    | is_matching_contour_contained(c1::c1s,c2::[]) = false
    | is_matching_contour_contained(c1::c1s,c2::c2s) =
      if (length c1s > length c2s) then false
      else if is_matching_literal_contained(c1,c2::c2s) then is_matching_contour_contained(c1s,remove_matching_literal_in_contour(c1,c2::c2s))
           else false  

  (* This function checks if if c1 is generated from c2 *)
  fun is_contour_generated([],[]) = true
    | is_contour_generated(c1::[],[]) = false
    | is_contour_generated(c1::c1s,[]) = false
    | is_contour_generated([],c2::[]) = true
    | is_contour_generated([],c2::c2s) = true
    | is_contour_generated(c1::[],c2::[]) = if L.match(c1,c2) then true else false
    | is_contour_generated(c1::[],c2::c2s) = if is_matching_literal_contained(c1,c2::c2s) then true else false
    | is_contour_generated(c1::c1s,c2::[]) = 
      if L.is_kind_mult_star(c2)
      then if L.match(c1,c2) then is_contour_generated(c1s,c2::[]) else false
      else false
    | is_contour_generated(c1::c1s,c2::c2s) = 
      if is_matching_literal_contained(c1,c2::c2s)
      then is_contour_generated(c1s,remove_matching_literal_in_contour_according_to_mult(c1,c2::c2s))
      else false

  (* This function checks if c1 is strongly compatible with c2, which may be different from checking if c2 is strongly compatible with c1 *)
  (* STRONG COMPATIBILITY is a form of MATCHING, used when pruning the tree in order to identify contours that are strongly or weakly compatible *)
  fun check_strong_compatibility (c1,c2) = 
    let
      val c2_concrete = get_contour_mult_none(c2)
      val c2_abstract = get_contour_mult_star_or_qm(c2)      
    in
      if (is_matching_contour_contained(c2_concrete,c1))
      then 
        let
          val c1_remaining = subtract_matching_contour(c1,c2_concrete)
        in
          if (is_contour_generated(c1_remaining,c2_abstract)) then true else false
        end
      else false
    end

  (* This function checks if c1 is weakly compatible with c2, which may be different from checking if c2 is weakly compatible with c1 *)
  (* WEAK COMPATIBILITY is a form of MATCHING, used when pruning the tree in order to identify contours that are strongly or weakly compatible *)  
  fun check_weak_compatibility (c1,c2) = 
    let
      val c2_concrete = get_contour_mult_none(c2)
    in
      if (is_matching_contour_contained(c2_concrete,c1)) then true else false
    end





  (* This function prints the contour c*)
  fun printout([]) = ""
    | printout(l::[]) = L.printout(l)
    | printout(l::ls) = L.printout(l) ^ " " ^ printout(ls) ^ " "

end