(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for graph distance matrices. *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

signature DISTANCE_MATRIX =
sig
  type T;
  type graph;
  type name;
  type dist;

  (* calculate the distance matrix of a graph *)
  val calculate : graph -> T
  
  (* set distance between two vertices *)
  val set_distance : name -> name -> dist -> T -> T
  val set_distance_if_shorter : name -> name -> dist -> T -> T
  
  (* get distance between two vertices *)
  val get_distance : T -> name -> name -> dist
  
  (* distance order *)
  val dist_ord : dist*dist -> order
  
  (* pretty printing *)
  val pretty_dist : dist -> Pretty.T
  val pretty : T -> Pretty.T
  val print : T -> unit
  
end;

functor DistanceMatrixFun(
  structure Graph : GRAPH
  ) : DISTANCE_MATRIX
= struct

  datatype dist = Dist of int | Infinity;
  
  type graph = Graph.T;
  type name = Graph.Vertex.name;
  type T  = dist Graph.Vertex.NTab.T Graph.Vertex.NTab.T;
  
  structure NTab = Graph.Vertex.NTab;
  
  (* Distance ordering. *)
  fun dist_ord (Dist(i),Dist(j)) = Int.compare (i,j)
    | dist_ord (Dist(_),Infinity) = LESS
    | dist_ord (Infinity,Dist(_)) = GREATER
    | dist_ord (Infinity,Infinity) = EQUAL;
  
  (* Get the distance between two vertices. *)
  fun get_distance t v1 v2 = NTab.get (NTab.get t v1) v2
                             handle NTab.Tab.UNDEF(_) => Infinity;
  
  (* Set the distance between two vertices. *)
  fun set_distance v1 v2 d t = NTab.update (v1,
                                (NTab.update (v2,d)
                                  (NTab.get t v1
                                    handle NTab.Tab.UNDEF(_) => NTab.empty)
                                )) t;             
  
  (* Set the distance between two vertices if the new distance is less *)
  fun set_distance_if_shorter v1 v2 d t = case dist_ord(d,get_distance t v1 v2) of
        LESS => set_distance v1 v2 d t
      | _    => t;
 
  fun distplusone d = case d of (Dist(i)) => Dist(i+1) | Infinity => Infinity;
      
  (* Calculate a distance matrix given a graph. *)
  fun calculate g = let
        (* Keep track of list of names = indexing set of matrix *)
        val l = Graph.get_vnames g
        fun calc g v t = let
              val (_,(efrom,eto)) = Graph.get_vertex g v
              val gnext = g |> Graph.delete_vertex v
              (* update distances to/from vwith around directed edge vfrom->vto *)
              fun update_dist vfrom vto vwith t =
                      (* distance to target <- distance to source + 1 *)
                    t |> set_distance_if_shorter vwith vto
                          (distplusone (get_distance t vwith vfrom))
                      (* distance from source <- distance from target + 1 *)
                      |> set_distance_if_shorter vfrom vwith
                          (distplusone (get_distance t vto vwith))
              (* update distances around edge named en *)
              fun calc_edge en t = let
                    val ((dir,_),(vs,vt)) = Graph.get_edge g en
                  in t |> Graph.Vertex.NSet.fold (update_dist vs vt) l
                       |> (if dir=Graph.Directed then I else
                           Graph.Vertex.NSet.fold (update_dist vt vs) l)
                  end;
              (* update distances around edge set es *)
              fun calc_edges es t = t |> Graph.Edge.NSet.fold calc_edge es 
            in t |> set_distance v v (Dist(0))
                 |> calc_edges efrom
                 |> calc_edges eto
                 |> (calc gnext (the (Graph.Vertex.NSet.get_local_bot
                                      (Graph.get_vnames gnext)))
                     handle Option => I)
            end;
      in calc g (the (Graph.Vertex.NSet.get_local_bot l)) NTab.empty end;
  
  (* Pretty printing of distance *)
  fun pretty_dist (Dist(i))  = Pretty.str (Int.toString i)
    | pretty_dist (Infinity) = Pretty.str "X";
  
  (* Pretty printing of matrix *)
  fun pretty t = Pretty.chunks
                   [Pretty.str "DistanceMatrix{",
                    Pretty.block
                      [Pretty.str "  ", (* indent *)
                       Pretty.chunks
                         (map
                            (fn v1 => 
                              Pretty.block
                                [Graph.Vertex.pretty_name v1,
                                 Pretty.str " | ",
                                 Pretty.list "" "" (map 
                                  (fn v2 => pretty_dist (get_distance t v1 v2))
                                  (NTab.NSet.list_of (NTab.get_nameset t))
                                  )
                                ])
                            (NTab.NSet.list_of (NTab.get_nameset t)))
                      ],
                    Pretty.str "}"
                   ];
      
  val print = Pretty.writeln o pretty;
end;