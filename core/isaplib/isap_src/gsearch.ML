(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      generic/gsearch.ML
    Author:     Lucas Dixon, University of Edinburgh
                lucasd@dai.ed.ac.uk
    Modified:   25 Oct 2004: Lucas - generalised best first search to 
                  arbitrary orders on 'a types. 
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

    Generic search tools (doesn't assume anything about state type)

    Note the convention of names: 

    "gf" is a function from state to bool
    that is true when the state is a goal state.

    "xf" is an expansion function that given a state, returns a list
    of states that are teh children of it.

    "st" is a state.

    In the code below a state is 'a type so that we can apply the
    search to any structures we want!

    Note: Useful property of search strategies is that they don't
    re-evaluate a state. If they do and the state evaluation has side
    effects, like editing contextual information, then re-evaluation
    can cause problems with the contextual information. For example,
    consider keeping track of the number of nodes in a branchh of the
    search space, the evaluation function can effect this, but if a
    state is re-evaluated then the number will no longer reflect the
    states so-far unexplored. Alternative to forcing
    non-re-evaluation, is to force a behaviour of the contextual
    information - but I prefer to limit the search, and let people do
    what they want with contextual information, and have it behave as
    they expect.
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

(* signature GEN_SEARCH = 
sig

val DEPTH_FIRST

end;
*)


structure GSearch = 
struct

(* simple depth first search. The Agenda is a list of lists,
representing the different depths. We pass a goal function gf, and a
node expansion function xf *)
  fun depth_fs gf xf st = 
      let 
        fun dfs_aux [] = NONE
          | dfs_aux (hq :: tq) = 
            case Seq.pull hq of 
              NONE => dfs_aux tq
            | SOME (h, hs) => 
              if (gf h) then 
                SOME (h, Seq.make (fn () => dfs_aux (hs :: tq)))
              else
                dfs_aux ((xf h)::hs::tq)
      in
        Seq.make ( fn () => dfs_aux [(Seq.single st)] )
      end;

(* Note that for efficiency, we scan bfs from left to to right and
then right to left etc. To change this so that we always scan form
l->r we could add in a "rev" in the case ([],m), to get (rev m, [])
this would keep the order of traditional breadth first search. The
speed decrease is a "rev" operation at each level of depth, that takes
time equal to the number of nodes at the level above. For a constant
time operation that preserves order at each level of increased depth,
we could use a two way list which we can add elements to both ends.
*)
  fun breadth_fs gf xf st = 
      let 
        fun bfs_aux ([],[]) = NONE (* fst list is cur level, second is next *)
          | bfs_aux ([],m) = bfs_aux (m, [])
          | bfs_aux (hq :: tq, l) = 
            case Seq.pull hq of 
              NONE => bfs_aux (tq, l)
            | SOME (h, hs) => 
              if (gf h) then 
                SOME (h, Seq.make (fn () => bfs_aux ((hs :: tq), l) ))
              else
                bfs_aux (hs::tq, (xf h) :: l)
      in
        Seq.make ( fn () => bfs_aux ([(Seq.single st)],[]) )
      end;


  (* best first search, first uses quicksort on new nodes, then a
  merge into existing list => most efficient method, in practice, I
  think. *)
  fun best_fs gf hof xf st = 
      let 
        fun merge (l1 as (h1 :: t1)) 
                  (l2 as (h2 :: t2)) = 
            (case hof (h1, h2) of 
               LESS => h1 :: (merge t1 l2)
             | EQUAL => h1 :: (merge t1 l2)
             | GREATER => h2 :: (merge l1 t2))
          | merge [] b = b
          | merge a [] = a

        fun bfs_aux [] = NONE
          | bfs_aux (h :: t) = 
            if (gf h) then 
              SOME (h, Seq.make (fn () => bfs_aux t))
            else 
              bfs_aux (merge (Library.sort hof (Seq.list_of (xf h))) t)
      in
        Seq.make ( fn () => bfs_aux [st] )
      end;

  (* if you want an A* type algorithm, the augment the state with any
  needed info (the so-far score), and use best first over that and a
  heuristic is then calculated on node expansion, (or a function for
  it is) which the heuristic function then accesses. *)
      
end;
