signature CONTOUR_LIST =
sig
  type T
  structure G : BANG_GRAPH
  structure C : CONTOUR

  val empty : T

  val mk : G.T -> T

  val eq : T * T -> bool 
  
  val fprintf : T -> string
end


functor Contour_List (G : BANG_GRAPH) : CONTOUR_LIST =
struct

  structure G = G
  structure C = Contour(G)

  type T = C.T list

  val empty = []

  (* Helper function to erase from a contour all the nodes which appear in previous contours *) 
  fun filter_remaining(c,f,[]) = []
    | filter_remaining(c,f,r::[]) =
          if (C.is_contained(r,c)) then r::f
          else f
    | filter_remaining(c,f,r::rs) = 
          if (C.is_contained(r,c)) then filter_remaining (c,r::f,rs) 
          else filter_remaining(c,f,rs)

  fun reassess_remaining(c,f,[]) = f 
    | reassess_remaining(c,f,r::[]) =
          if (C.is_contained(r,c)) then f
          else r::f
    | reassess_remaining(c,f,r::rs) = 
          if (C.is_contained(r,c)) then reassess_remaining (c,f,rs)
          else reassess_remaining(c,r::f,rs)

  
  (* Helper function to recursively build contours *)
  fun build_contours(_,_,cl,[]) = rev cl
    | build_contours(_,[],cl,_) = rev cl
    | build_contours(g,c,cl,r) =
    let
      val new_contour = C.mk g c
      val final_contour = filter_remaining(new_contour,[],r)
      val r = reassess_remaining(final_contour,[],r)
    in
      if null(final_contour) then build_contours(g,final_contour,cl,r)
      else build_contours(g,final_contour,(final_contour::cl),r)      
    end

  (* Build a contour list *)
  fun mk(g) = 
    let 
      val contour_list = empty

      val first_contour = C.target_function(g)
      val contour_list = first_contour::contour_list

      val remaining_list = G.get_vnames g
      val remaining_list = V.NSet.list_of remaining_list
      val g_list = replicate (length remaining_list) g
      val remaining_list = map2 C.L.mk g_list remaining_list      

      val remaining_list = reassess_remaining(first_contour,[],remaining_list)  
    in
      build_contours (g,first_contour,contour_list,remaining_list)
    end



  (* Helper functions to check if two contour lists are the same *)
  fun find_contour(c1,[]) = false
    | find_contour(c1,c2::[]) = if C.eq(c1,c2) then true else false
    | find_contour(c1,c2::cl2) = if C.eq(c1,c2) then true else find_contour(c1,cl2)

  fun remove_contour(c1,c2::[]) = []
    | remove_contour(c1,c2::cl2) = if C.eq(c1,c2) then cl2 else c2::remove_contour(c1,cl2)

  fun is_same_contour_list(c1::[],c2::[]) = if C.eq(c1,c2) then true else false
    | is_same_contour_list(c1::cl1,c2::[]) = if C.eq(c1,c2) then true else false
    | is_same_contour_list(c1::cl1,c2::cl2) = 
      if C.eq(c1,c2) then is_same_contour_list (cl1,cl2)
      else if find_contour(c1,cl2) then is_same_contour_list(cl1,c2::remove_contour(c1,cl2)) else false

  (* This function checks for physical equality between cl1 and cl2, that is if cl1 and cl2 are the same contour list*)
  fun eq (cl1,cl2) =
    if (length cl1 = length cl2) then is_same_contour_list(cl1,cl2)
    else false



  fun fprintf ([]) = ""
    | fprintf (c::[]) = C.fprintf(c)
    | fprintf (c::cs) = C.fprintf(c) ^ " " ^ fprintf(cs) ^ " "

end

