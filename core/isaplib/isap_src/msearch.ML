(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      generic/msearch.ML
    Author:     Lucas Dixon, University of Edinburgh
                lucas.dixon@ed.ac.uk
    Modified:   28 Oct 2004
    Created:    28 Oct 2004
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

    Multiple strategy search for lazily evaluated trees. 

    i.e. when given an unfolding function: 
         ('a -> 'a seq )

    we can have multiple different search mechanisms nested in each
    other, and allow the nodes (the "'a"'s) to specify when to start a
    new search strategy or end an old one. We do this using search
    operators.
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
structure MSearch (* : MSEARCH *) = 
struct

exception msearch_exp of string;

(* General static data function for stacked search: note you can only
mix stacked search strategies if they have the same static data
type. Static data is used to allow functions that depend on the
agenda, such as pretty printing. Our polymorphic trick allow us to
arbitrarily extend this to provide an abstract interface to the
operations we want on the agenda. This is very useful for
debugging. *)
type staticdT = 
     { name : string, 
       pretty_agenda : unit -> Pretty.T };

(* We do a clever thing: we encode into the notion of strategy the
operations we wish to perform on it. This allows us to "hide" the type
of agenda, providing simpy an interafce for its usage - very very
cool, I think. *)
datatype 'a T = strategy of 
         { add_nodes     : 'a Seq.seq -> 'a T,
					 pop_node      : unit -> ('a T * 'a) option,
           staticd       : staticdT };

(* fun staticd_of (strategy rep) = #staticd rep; *)

fun strat_add_states staticdf add_nodes pop_node d nds =
    let val newd = add_nodes nds d in
      strategy { add_nodes = strat_add_states staticdf add_nodes pop_node newd,
                 pop_node = strat_pop_state staticdf add_nodes pop_node newd,
                 staticd = staticdf newd }
    end
and strat_pop_state staticdf add_nodes pop_node d () = 
    case pop_node d of 
      NONE => NONE
    | SOME (newd, el) => 
      SOME (strategy { add_nodes = strat_add_states staticdf add_nodes pop_node newd,
                       pop_node = strat_pop_state staticdf add_nodes pop_node newd,
                       staticd = staticdf newd }, 
            el);
fun mk_strategy { agenda, add_nodes, pop_node, staticdf } =
    strategy {add_nodes = strat_add_states staticdf add_nodes pop_node agenda, 
              pop_node = strat_pop_state staticdf add_nodes pop_node agenda,
              staticd = staticdf agenda };

fun add_states (strategy rep) = (#add_nodes rep);
fun pop_state (strategy rep) = (#pop_node rep) ();
fun staticd_of (strategy rep) = (#staticd rep);

(* operators for nodes to effect the search startegy. *)
datatype 'a searchopT = addstrategy of 'a T
                      | endstrategy;

(* popfopf is the function to get the pop-operations from the state type *)
(* for reasoning states this is: *)
fun search popsopf xf sstrat st =
    let 
      fun subsearch NONE () = NONE
        | subsearch (SOME (sstrat, st)) () = 
          case (popsopf st) of 
            NONE => (xf st) |> add_states sstrat
                            |> pop_state
                            |> (fn ss => subsearch ss ())
          | SOME(st', addstrategy sstrat') => 
            (SOME (sstrat', st'))
              |> Seq.make o subsearch
              |> add_states sstrat
              |> pop_state
              |> (fn ss => subsearch ss ())
          | SOME(st', endstrategy) => 
            SOME (st', Seq.make (fn () => subsearch (pop_state sstrat) ()))
    in Seq.make (subsearch (SOME (sstrat, st))) end;


fun traced_search tracef popsopf xf sstrat st =
    let 
      fun tracesubsearch x () = subsearch' (tracef x) ()
      and subsearch' NONE () = NONE
        | subsearch' (SOME (sstrat, st)) () = 
          case (popsopf st) of 
            NONE => (xf st) |> add_states sstrat
                            |> pop_state
                            |> (fn ss => tracesubsearch ss ())
          | SOME(st', addstrategy sstrat') => 
            (SOME (sstrat', st'))
              |> (Seq.make o tracesubsearch)
              |> add_states sstrat
              |> pop_state
              |> (fn ss => tracesubsearch ss ())
          | SOME(st', endstrategy) => 
            SOME (st', 
                  Seq.make (fn () => tracesubsearch (pop_state sstrat) ()))
    in Seq.make (tracesubsearch (SOME (sstrat, st))) end;

datatype ('a, 'b) delayopT = delay of 'b 
                  | resume of ('a list -> ('a  list* 'a list ));

fun delayed_search delayopf popsopf xf sstrat st =
let 
	fun subsearch _ NONE  () = NONE
        | subsearch delayed (SOME (sstrat, st)) () = 
          case (popsopf st) of 
            NONE => (xf st) |> add_states sstrat
                            |> pop_state
                            |> (fn ss => delayed_subsearch delayed ss ())
          | SOME(st', addstrategy sstrat') => 
            (SOME (sstrat', st'))
              |> Seq.make o delayed_subsearch delayed
              |> add_states sstrat
              |> pop_state
              |> (fn ss => delayed_subsearch delayed ss ())
          | SOME(st', endstrategy) => 
            SOME (st', Seq.make (fn () => delayed_subsearch delayed (pop_state sstrat) ()))
	and
		delayed_subsearch _ NONE  () = NONE
          | delayed_subsearch delayed (SOME(sstrat,st)) () = 
						case (delayopf st) of 
							NONE => subsearch delayed (SOME(sstrat,st)) ()							
						|	SOME (st', delay d) =>  
							 subsearch (st'::delayed)  (pop_state sstrat) ()
						| SOME (st', resume defrost_fun) =>
							(*h' should be the state h with the resume-attribute reset if need be*)
							let 
								val (defrosted, frozen) = defrost_fun delayed
							in
								subsearch frozen (SOME (add_states sstrat (Seq.of_list defrosted),st)) ()
							end
		in Seq.make (delayed_subsearch [] (SOME (sstrat, st))) end;


(* depth first search *)
local 
val add_nodes = Seq.append;
fun pop_node agenda = 
    case (Seq.pull agenda) of NONE => NONE 
                            | SOME (h,t)  => SOME (t,h);
fun pretty agenda () = 
    Pretty.chunks [ Pretty.block [Pretty.str "No info available"] ];
in
fun depthfs () = 
    mk_strategy { agenda = Seq.empty,
                  add_nodes = add_nodes,
                  pop_node = pop_node, 
                  staticdf = (fn agneda => {name = "Depth First Search",
                                            pretty_agenda = pretty agneda}) };
end;

(* breadth first search *)
(* THINK: is this OK for lazy case - for the non-lasy version this is
horribly ineffcient - as the append operation takes time O(agenda) and
the agenda gets bigquickly - maybe for the lazy case this is OK as we
don't actually compute the append until we need to... ? *)
local 
fun add_nodes sts agenda = Seq.append agenda sts;
fun pop_node agenda = 
    case (Seq.pull agenda) of NONE => NONE 
                            | SOME (h,t)  => SOME (t,h);
fun pretty agenda () = 
    Pretty.chunks [ Pretty.block [Pretty.str "No info available"] ];
in
fun breadthfs () = 
    mk_strategy { agenda = Seq.empty,
                  add_nodes = add_nodes,
                  pop_node = pop_node, 
                  staticdf = (fn agneda => {name = "Breadth First Search",
                                            pretty_agenda = pretty agneda}) };
end;


(* best first search: quick sorts first list, and merges this into old
list: this is the quickest version possible (I think) Note: we
evaluate eagerly all possible next states as we need to be able to
compare them. *)
local 
fun add_nodes hof sts agenda = 
    let 
      fun merge (l1 as (h1 :: t1)) 
                (l2 as (h2 :: t2)) = 
          (case hof (h1, h2) of 
             LESS => h1 :: (merge t1 l2)
           | EQUAL => h1 :: (merge t1 l2)
           | GREATER => h2 :: (merge l1 t2))
        | merge [] b = b
        | merge a [] = a
    in
      merge (Library.sort hof (Seq.list_of sts)) agenda
    end;

fun pop_node [] = NONE
  | pop_node (n::t) = SOME (t,n);

fun pretty agenda () = 
    Pretty.chunks [
    Pretty.block [Pretty.str "Agenda Size: ",
                  Pretty.str (string_of_int (length agenda))]
    ];

in
fun bestfs hof = 
    mk_strategy { agenda = [],
                  add_nodes = add_nodes hof,
                  pop_node = pop_node, 
                  staticdf = (fn agneda => {name = "Best First Search",
                                            pretty_agenda = pretty agneda}) };
end;



(* Iterative Deepening search, increment depth by Sequence value.
For example, typical depth increment function is:

fun mksimpledepthf i = 
    let fun f j () = SOME (j, Seq.make f (j + i))
    in Seq.make (f i) end;

Note that the nieve implementation is always
non-terminating. Furthermore, when used with another strategy, it will
result in repeated state. Also, it breaks our requirement on not
re-evaluating states (for subspace).

The claimed advantge of such a style is that it reduces the needed
storage, while mantaining completeness and also depth first style
behaviour.

Beware technique developer!

Doesn't make sense in a setting of mixed strategies as no result is
defined - or multiple results will be returned. Possibilities:

give back multiple results, OR
give back only first result

*)
(*
local 
fun add_nodes sts (maxd,next,d,agenda) = 
    let val d' = d + 1 in 
      if d = maxd then agenda 
      else (map (fn n => (d', n)) (Seq.list_of sts)) @ agenda;
fun pop_node [] = NONE
  | pop_node (maxd,d,(d',h)::t) = SOME ((maxd,d',t),h);
in
fun idfs sf = 
    SSearch.sstrategy { empty = [],
                add_nodes = add_nodes,
                pop_node = pop_node,
                goalf = rstate_gaolf
              };
end;
*)




end;

(* 
datatype ('d, 'a) strategyT = sstrategy of 
		     { agenda    : 'd, 
			     add_nodes : 'a Seq.seq -> 'd -> 'd,
			     pop_node  : 'd  -> ('d * 'a) option};

fun add_states (sstrategy {agenda, add_nodes, pop_node}) sts = 
    sstrategy {agenda = add_nodes sts agenda, 
               add_nodes=add_nodes, pop_node=pop_node};

fun pop_state (sstrategy {agenda, add_nodes, pop_node}) = 
    case pop_node agenda of 
      NONE => NONE
    | SOME (agenda', a) => 
      SOME (sstrategy {agenda=agenda',add_nodes=add_nodes,
                       pop_node=pop_node}, a);

*)
