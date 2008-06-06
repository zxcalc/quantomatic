(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      generic/lsearch.ML
    Author:     Moa Johansson, University of Edinburgh
                
    Created:    10 Aug 2005
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

 A lazy version of stacked search that gives back one state at the time.
 Useful for best-first rippling interface.
 
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature LSEARCH =
  sig
    val lazy_search :
       ('a -> 'b Seq.seq) ->
       ('a -> 'b -> bool) ->
       ('a -> ('a * 'b MSearch.searchopT) option) ->
       'b MSearch.T list -> 'a -> 'b list * 'b MSearch.T list
    val pop_state :
       'a -> 'b MSearch.T -> ('a -> 'b -> bool) -> 'b list -> 'b MSearch.T

end;

structure LSearch : LSEARCH =
struct

(* Removes the state 'st' from the agenda of the given
  strategy. *)
fun pop_state st sstrat eqf sts =
		case (MSearch.pop_state sstrat) of
			NONE => MSearch.add_states sstrat (Seq.of_list sts)
		| SOME (sstrat',st') => 
				if (eqf st st') then
				MSearch.add_states sstrat' (Seq.of_list sts)
			else 
				pop_state st sstrat' eqf (st'::sts)

(* Lazy search for stacked search interface.  *)
fun lazy_search xf eqf popf [] st = ([], [])
	| lazy_search xf eqf popf (sstrat::sstrat_stack) st =
		let val states = Seq.list_of (xf st)
		in
				case (popf st) of
            NONE =>
						let val sstrat' = pop_state st sstrat eqf [] in
						(states, ((MSearch.add_states sstrat' (Seq.of_list states)):: sstrat_stack))
						end

          | SOME(st', MSearch.addstrategy new_sstrat) =>
						let val sstrat' =  pop_state st sstrat eqf [] in
            lazy_search xf eqf popf (new_sstrat::sstrat'::sstrat_stack) st'
						end
             
          | SOME(st', MSearch.endstrategy) => 
							lazy_search xf eqf popf sstrat_stack st'
		end


end;