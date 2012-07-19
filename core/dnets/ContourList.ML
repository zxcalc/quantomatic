signature CONTOUR_LIST =
sig
  type T
  structure G : BANG_GRAPH
  structure C : CONTOUR

  val empty : T

  val mk : G.T -> T

end


functor Contour_List (G : BANG_GRAPH) : CONTOUR_LIST =
struct

  structure G = G
  structure C = Contour(G)

  type T = C.T list

  val empty = []

  (* Helper function to erase from a contour all the nodes which appear in previous contours *) 
  fun filter_remaining(c,f,r::remainings) = 
          if (C.is_contained(r,c)) then filter_remaining (c,r::f,remainings) 
          else filter_remaining(c,f,remainings)
    | filter_remaining(c,f,r::[]) =
          if (C.is_contained(r,c)) then r::f
          else f
    | filter_remaining(c,f,[]) = []

  fun reassess_remaining(c,f,r::remainings) = 
          if (C.is_contained(r,c)) then reassess_remaining (c,f,remainings)
          else reassess_remaining(c,r::f,remainings)
    | reassess_remaining(c,f,r::[]) =
          if (C.is_contained(r,c)) then f
          else r::f
    | reassess_remaining(c,f,[]) = []      

  (* Helper function to recursively build contours *)
  fun build_contours(g,c,cl,r) =
    let
      val new_contour = C.mk(g,c)
      val final_contour = filter_remaining(new_contour,[],r)
      val r = reassess_remaining(final_contour,[],r)
      val cl = final_contour::cl
    in
      build_contours(g,final_contour,cl,r)
    end
    | build_contours(g,c,cl,[]) = cl

  (* Build a contour list *)
  fun mk(g) = 
    let 
      val contour_list = empty

      val first_contour = C.target_function(g)
      val contour_list = first_contour::contour_list

      val remaining_list = G.get_vnames g
      val remaining_list = V.NSet.list_of remaining_list
      val g_list = replicate (length remaining_list) g
      val gr_list = g_list ~~ remaining_list
      val remaining_list = map C.L.mk gr_list      

      val remaining_list = reassess_remaining(first_contour,[],remaining_list)  
    in
      build_contours(g,first_contour,contour_list,remaining_list)
    end

end

