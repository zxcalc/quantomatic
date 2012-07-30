signature CONTOUR =
sig
  type T
  structure G : BANG_GRAPH
  structure L : LITERAL

  val empty : T
  val add_literal : T -> L.T -> T
  val get_first_literal : T -> L.T 
  val get_contour_mult_none : T -> T
  val get_contour_mult_star : T -> T
  val get_contour_mult_qm : T -> T
  val get_contour_mult_star_or_qm : T -> T

  val mk : G.T -> T -> T
  val target_function : G.T -> T

  val is_eq_literal_contained : L.T * T -> bool
  val is_equiv_literal_contained : L.T * T -> bool
  val is_matching_literal_contained : L.T * T -> bool
  
  val is_matching_contour_contained : T * T -> bool
  
  val equiv : T * T -> bool
  val check_strong_compatibility : T * T -> bool
  val check_weak_compatibility : T * T -> bool

  val fprintf : T -> string

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



  (* Add a node to a contour *)
  fun add_literal c l = l::c



  (* Get the first node in a contour *)
  fun get_first_literal(l::[]) = l
    | get_first_literal(l::literals) = l

  (* Get only the literal with multiplicity equal to None *)
  fun get_contour_mult_none([]) = empty
    | get_contour_mult_none(l::[]) = if L.is_kind_mult_none(l) then l::[] else []
    | get_contour_mult_none(l::ls) = if L.is_kind_mult_none(l) then l::get_contour_mult_none(ls) else get_contour_mult_none(ls)

  (* Get only the literal with multiplicity equal to Star *)
  fun get_contour_mult_star([]) = empty
    | get_contour_mult_star(l::[]) = if L.is_kind_mult_star(l) then l::[] else []
    | get_contour_mult_star(l::ls) = if L.is_kind_mult_star(l) then l::get_contour_mult_star(ls) else get_contour_mult_star(ls)

  (* Get only the literal with multiplicity equal to QM *)
  fun get_contour_mult_qm([]) = empty
    | get_contour_mult_qm(l::[]) = if L.is_kind_mult_qm(l) then l::[] else []
    | get_contour_mult_qm(l::ls) = if L.is_kind_mult_qm(l) then l::get_contour_mult_qm(ls) else get_contour_mult_qm(ls)     

  (* Get the literal with multiplicity equal to Star or QM *)
  fun get_contour_mult_star_or_qm(c) = flat (get_contour_mult_star(c) :: get_contour_mult_qm(c) :: [])



  (* Helper function to find if an equivalent literal is contained in a list of literals *)
  fun is_eq_literal_contained(x, []) = false
    | is_eq_literal_contained(x,l::[]) = if L.eq(x,l) then true else false
    | is_eq_literal_contained(x,l::literals) = if L.eq(x,l) then true else is_eq_literal_contained(x,literals)

  (* Helper function to find if a equiv literal is contained in a list of literals *)
  fun is_equiv_literal_contained(x, []) = false
    | is_equiv_literal_contained(x,l::[]) = if L.equiv(x,l) then true else false
    | is_equiv_literal_contained(x,l::literals) = if L.equiv(x,l) then true else is_equiv_literal_contained(x,literals)

  (* Helper function to find if a matching literal is contained in a list of literals *)
  fun is_matching_literal_contained(x, []) = false
    | is_matching_literal_contained(x,l::[]) = if L.match(x,l) then true else false
    | is_matching_literal_contained(x,l::literals) = if L.match(x,l) then true else is_matching_literal_contained(x,literals)



  (* Helper function to remove matching literal from a contour considering its multiplicity *)
  (* PRECONDITION: is_matching_literal_contained(l,c) = true *)
  fun remove_matching_literal_in_contour_according_to_mult(l,c::[]) = 
      if(L.is_kind_mult_star(c)) then c::[] else []
    | remove_matching_literal_in_contour_according_to_mult(l,c::cs) = 
      if L.match(l,c) then 
        if(L.is_kind_mult_star(c)) then c::cs else cs 
      else c::remove_matching_literal_in_contour_according_to_mult(l,cs)

  (* Helper function to remove matching literal from a contour *)
  (* PRECONDITION: is_matching_literal_contained(l,c) = true *)
  fun remove_matching_literal_in_contour(l,c::[]) = []
    | remove_matching_literal_in_contour(l,c::cs) = if L.match(l,c) then cs else c::remove_matching_literal_in_contour(l,cs)

  (* Helper function to remove equiv literal from a contour *)
  (* PRECONDITION: is_equiv_literal_contained(l,c) = true *)
  fun remove_equiv_literal_in_contour(l,c::[]) = []
    | remove_equiv_literal_in_contour(l,c::cs) = if L.equiv(l,c) then cs else c::remove_equiv_literal_in_contour(l,cs)

  (* Helper function to remove eq literal from a contour *)
  (* PRECONDITION: is_eq_literal_contained(l,c) = true *)
  fun remove_equiv_literal_in_contour(l,c::[]) = []
    | remove_equiv_literal_in_contour(l,c::cs) = if L.eq(l,c) then cs else c::remove_equiv_literal_in_contour(l,cs)



  (* Helper function to remove literals from a contour corresponding to the eq nodes *)
  fun remove_eq_duplicate(e,l::[]) = if is_eq_literal_contained(l,e) then e else l::e
    | remove_eq_duplicate(e,l::literals) = if is_eq_literal_contained(l,e) then remove_eq_duplicate(e,literals) else remove_eq_duplicate(l::e,literals)

  (* Helper function to remove eq literals from a contour belonging to a list *)
  (* PRECONDITION: no null lists as checked in remove_eq_duplicate() *)
  fun remove_eq_literals_in_contour(adj::[],c) = if is_eq_literal_contained(adj,c) then [] else adj::[] 
    | remove_eq_literals_in_contour(adj::adjs,c) = 
        if is_eq_literal_contained(adj,c) 
        then remove_eq_literals_in_contour(adjs,c)
        else adj::remove_eq_literals_in_contour(adjs,c)




  (* Build the first contour given a graph *)
  fun target_function(g) =
      let
        val vnames = G.get_unbboxed(g)
        val v::vlist = V.NSet.list_of(vnames)
        val l = L.mk g v
        val c = empty
      in
        l::c
      end

  (* Build a contour starting from another contour *)
  fun mk g c = 
        let
          val g_list = replicate (length c) g
          val gl_list = g_list ~~ c

          val adj_nodes = maps2 L.get_adj g_list c
          val adj_nodes = remove_eq_literals_in_contour(adj_nodes,c)
          val e = empty
        in
          remove_eq_duplicate(e,adj_nodes)
        end

  

  (* Helper function to find if every literal in c1 is matched in c2 *)
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
  
  (* Helper function to subtract c2 from c1 *)
  (* PRECONDITION: is_matching_contour(c1,c2) = true *)
  fun subtract_contour(c1::[],[]) = c1::[]
    | subtract_contour(c1::c1s,[]) = c1::c1s
    | subtract_contour(c1::[],c2::[]) = []
    | subtract_contour(c1::c1s,c2::c2s) = subtract_contour(remove_matching_literal_in_contour(c2,c1::c1s),c2s)

  (* Helper function to check if c1 is generated from c2 *)
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


  (* Helper functions to check if two contours are the equivalent *)
  fun find_equiv_literal(c1,[]) = false
    | find_equiv_literal(c1,c2::[]) = if L.equiv(c1,c2) then true else false
    | find_equiv_literal(c1,c2::c2s) = if L.equiv(c1,c2) then true else find_equiv_literal(c1,c2s)

  fun is_equiv_contour(c1::[],c2::[]) = if L.equiv(c1,c2) then true else false
    | is_equiv_contour(c1::c1s,c2::[]) = if L.equiv(c1,c2) then true else false
    | is_equiv_contour(c1::c1s,c2::c2s) = 
      if L.equiv(c1,c2) then is_equiv_contour (c1s,c2s)
      else if find_equiv_literal(c1,c2s) then is_equiv_contour(c1s,c2::remove_equiv_literal_in_contour(c1,c2s)) else false



  (* This function checks for equivalence between c1 and c2, that is if c1 and c2 are equivalent contours *)
  (* TRUE iff c1 and c2 contains equivalent literals in any order; they can be generated by different node-vertices *)
  (* EQUIVALENCE is used when building the tree in order identify contours which are equivalent and share a node in the tree *)    
  fun equiv (c1,c2) =
    if (length c1 = length c2) then is_equiv_contour(c1,c2)
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
          val c1_remaining = subtract_contour(c1,c2_concrete)
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



  fun fprintf([]) = ""
    | fprintf(l::[]) = L.fprintf(l)
    | fprintf(l::ls) = L.fprintf(l) ^ " " ^ fprintf(ls) ^ " "

end