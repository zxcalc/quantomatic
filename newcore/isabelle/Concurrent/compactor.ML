(*  Title:      Pure/Concurrent/compactor.ML
    Author:     Michael James Bradley
    
A compactor was designed to help with the parallelisation of fast-to-compute
functions, which are typically not parallelisable due to the required overheads.
It will automatically combine lots of small functions into larger ones which
can be run efficiently in parallel.

The compactor has two main modes:

Static: you specify how many functions should be grouped into one, and the
        compactor will always create groups of this size.
        
Dynamic: you specify how long any group of functions should take, and the
         compactor decides how many of the fast-to-compute (hereafter known
         as "little" functions) should be combined to acheive this. If the
         cumulative running times of the little functions changes, then
         a dynamic compactor will adapt to this.
         
There are a number of additional parameters and settings to enable you to try and optimise
its performance.
*)
signature COMPACTOR =
sig
  (*This exception is raised whenever you use a compactor incorrectly.
  (It will not be raised due to a fault in the compactor itself)*)
  exception Compactor_Use_Error of string
  (*The type of a compactor!*)
  type compactor
  (*The type of a direction. This is used for accumulating the results
  of functions, and is similar to the idea of folding left or right.
  
  If f1 is the first little function consumed by the Compactor, f2 is the next,
  and so on, then the different directions give this effect:
  
  Right:  f1 (f2 (f3 (f4...
  Left:   ...f4 (f3 (f2 (f1
  
  So left applies the function the compactor first consumes first,
  but right does the opposite. This functionality is
  for efficiency.
  *)
  datatype direction = Left | Right
  
  (*Create a new compactor which will always combine
  the specified number of functions*)
  val new_static :
       int        (* The number of little functions you want combined *)
    -> compactor
  
  (*Create a new compactor which will change
  how many functions are compacted according to how long
  it takes to run each one (times the functions).
  You specify how long you want each returned function
  to take roughly*)
  val new_dynamic :
       Time.time (* The amount of time any group of little functions should take *)
    -> compactor
    
  (*For a dynamic compactor, set the time to aim for*)
  val set_time : compactor
    -> Time.time (*The time it should now aim for groups to take*)
    -> compactor
    
  (*Return the time a dynamic compactor is aiming for*)
  val get_time : compactor -> Time.time
  
  (*For a static compactor, set the size of the groups created*)
  val set_size : compactor
    -> int (*The new group size*)
    -> compactor
  
  (*For a static compactor, return the size of the groups it is going
  to produce*)
  val get_size : compactor -> int
  
  (*Returns true if a compactor is dynamic*)
  val is_dynamic : compactor -> bool
  
  (*Returns true if a compactor is static*)
  val is_static : compactor -> bool
  
  (*For dynamic compactors only, set the estimated number of functions
  that you think need to be compacted to use up the time given to this compactor
  If NONE is supplied, it will use a default.
  
  Default: NONE (when first created)
  *)
  val set_estimated_number : compactor
    -> int option (* The number of little functions you think will need to be combined to make up the right time *)
    -> compactor
  
  (*Return the estimated number of little functions (set by you) that should be
  compacted to make up the required amount of time
  
  Default: NONE
  *)
  val get_estimated_number : compactor
    -> int option (* The estimate being used right now, if one was set *)
  
  (*Sets the direction in which functions are compacted.
  This is useful for efficiency.
  
  Right: f1 (f2 (f3 (f4 f_id)))
  Left: f4 (f3 (f2 (f1 f_id)))
  
  Default: Left
  *)
  val set_fold_direction : compactor
    -> direction (* The direction that functions will be accumulated in *)
    -> compactor
  
  (*Get the direction in which functions are compacted
  
  Default: Left
  *)
  val get_fold_direction : compactor
    -> direction (* The direction that will be used by the compactor *)
  
  
  (*Tell the compactor to prime itself. Priming a compactor means
  that it will precompute groups of functions initially sequentially
  to try to obtain a good estimate of how many functions to use
  before it starts returning functions to you.
  
  Note: its precomputed groups will be returned to you as
  dummy functions, so the first few will be nearly instant to process.
  Not many groups should be precomputed.
  *)
  val prime : compactor
    -> int option (* This can act as a limit to the number of groups
                     which can be precomputed before the compactor starts
                     returning groups for you to compute. It is recommended
                     you set this if the supplied functions are likely
                     to vary significantly in their running times, as otherwise
                     a compactor may never consider the situation stable
                     enough to stop precomputing! *)
    -> bool       (* If true, this will turn a dynamic compactor into a static
                     one once the precomputation is complete. Static compactors
                     have a smaller overhead than dynamic compactors, so
                     if you are confident the priming has given a good
                     result, then it might be worth setting this to true. *)
    -> compactor
  
  (*Tell the compactor not to prime itself first*)
  val do_not_prime : compactor -> compactor
  
  (*Return whether or not the compactor is priming, and if it is, the details.
  The returned value has the format:
  
  NONE if it is not going to prime first
  SOME (an optional limit on how many groups of functions it can be precompute,
  whether or not it should become a static compactor after priming)*)
  val check_prime : compactor -> (int option * bool) option
  
  (*Set the scaling effect within the compactor. This determines how quickly
  it can scale up or down as functions change (if these values are too high,
  anomalies can cause dramatic shifts in estimates)
  
  high - how much the compactor is allowed to scale up its estimate for the number
  of functions required in a group, if it discovered it was wrong (in its previous estimate)
  low - how much the compactor is allowed to scale down its estimate for the number
  of functions required in a group, if it discovered it was wrong
  
  Note: the compactor will only accept values >=1. For most purposes, 1 is clearly not helpful...
  
  This is pointless if the compactor is static! So it will throw an error
  
  Default: (2,2)
  *)
  val set_scaling : compactor
    -> (int * int) (* (low,high) - described above *)
    -> compactor
    
  (*Return the scaling in use by the compactor. If you have not set any,
  then this will return the defaults used
  
  Default: (2,2)
  *)
  val get_scaling : compactor
    -> (int * int)
  
  (*
  This is the most generic way to set up a compactor. Look at the argument
  descriptions to know how to use this.
  *)
  val compact : compactor
    -> ('a -> ((('b -> 'b) * 'a) option)) (* This whole type is called the producer. The producer must
                                             be able to, when given the second half of its previous result,
                                             return a new little function for the compactor.
                                             The little function is the ('b -> 'b) part of the type *)
    -> 'a                                 (* An identity element for the producer. This is what will be
                                             passed to it first *)
    -> 'b                                 (* An identity for the little functions. The results of the little
                                             functions are accumulated by passing the result of the previous
                                             little function to the next one *)
    -> ((unit -> 'b) * int) Seq.seq       (* The result is a sequence of functions which, when run, will
                                             compute a group of the little functions supplied and return
                                             the result. The integer says how many functions the compactor wanted
                                             (not necessarily succeeded) to put into this group *)
  
  (****************************************)
  (********Simplification functions********)
  (****************************************)
  
  (*Compact by taking the little functions from a sequence*)
  val compact_with_seq : compactor
    -> (('b -> 'b) Seq.seq) (* The sequence of little functions to be fed to the compactor *)
    -> 'b                   (* The identity for the little function *)
    -> ((unit -> 'b) * int) Seq.seq (* The resulting sequence of computable groups of the little functions *)
  
  (*Compact by taking the little functions from a list*)
  val compact_with_list : compactor
    -> (('b -> 'b) List.list)         (* The list of little functions to be fed to the compactor *)
    -> 'b                             (* The identity for the little function *)
    -> ((unit -> 'b) * int) Seq.seq   (* The resulting sequence of computable groups of the little functions *)
  
  (*Compact over a sequence with a function designed to take an element from
  the existing sequence and return an entirely new sequence. The resulting
  sequences will be appended together!
  
  Synonymous to Seq.maps
  *)
  val compact_maps_seq : compactor
    -> ('b -> 'c Seq.seq)           (* The function which takes an element from the sequence,
                                       and returns a new sequence *)
    -> 'b Seq.seq                   (* The sequence of elements *)
    -> (unit -> 'c Seq.seq) Seq.seq (* The new sequence generated *)
    
  (*Compact a function which will take an element from
  the given sequence, and convert it to an element for a new sequence
  
  Synonymous to Seq.map*)
  val compact_map_seq : compactor
    -> ('b -> 'c)                   (* Function being applied to the elements in the sequence *)
    -> ('b Seq.seq)                 (* The sequence itself *)
    -> (unit -> 'c Seq.seq) Seq.seq (* A new sequence! It is a sequence of sequences,
                                       where the inner sequence contains all of the individual
                                       results obtained when compacting the functions *)
  
  (*Similar to compact_maps_seq but for lists*)
  val compact_maps_list : compactor
    -> ('b -> 'c List.list)           (* The function which takes an element from the list, and returns a new list *)
    -> 'b List.list                   (* The list of elements *)
    -> (unit -> 'c List.list) Seq.seq (* The new sequence generated *)
  
  (*Similar to compact_map_seq but for lists*)
  val compact_map_list : compactor
    -> ('b -> 'c)                     (* The function to be applied *)
    -> ('b List.list)                 (* The list of elements *)
    -> (unit -> 'c List.list) Seq.seq (* A sequence of lists of elements produced. Each list of elements
                                         is formed from compacting several applications of the function together*)
end;

(*
An implementation for a compactor.

IMPLEMENTATION WARNING:

The dynamic compactor uses shared memory to change how it compacts its functions.
This memory is not synchronised on for speed, so it is possible that two functions could write
to this memory at once. It will attempt to handle any exceptions caused by "giving up" on writing.
This is MUCH more efficient than attempting to obtain a lock through, for example, Future.guarded_access.
*)
structure Compactor :> COMPACTOR =
struct
  
  (*This exception is raised whenever you use a compactor incorrectly.
  (It will not be raised due to a fault in the compactor itself)*)
  exception Compactor_Use_Error of string
  
  (*The type of a direction. This is used for accumulating the results
  of functions, and is similar to the idea of folding left or right.
  
  If f1 is the first little function consumed by the Compactor, f2 is the next,
  and so on, then the different directions give this effect:
  
  Right:  f1 (f2 (f3 (f4...
  Left:   ...f4 (f3 (f2 (f1
  
  So left applies the function the compactor first consumes first,
  but right does the opposite. This functionality is
  for efficiency.
  *)
  datatype direction = Left | Right
  
  (*The static or dynamic part of the compactor
  For the dynamic compactor, the format is:
  
  (
  The time to aim for,
  An estimate of the number of functions that will be required <optional>,
    (
    The number of big functions it is allowed to precompute when priming,
    Whether or not it should become static after priming
    ) <optional>
  )
  *)
  datatype comp = Static of int | Dynamic of (Time.time * int option * (int option * bool) option)
  (*Includes the direction of the fold
  The last two integers are the scaling factors (scaling: (up,down))*)
  type compactor = (direction * comp * (int * int))
  
  (*Default initialisation values*)
  val defaultPrimingLimit = SOME 15;
  val defaultGoStatic = false;
  val defaultEstimate = NONE;
  val defaultPriming = SOME (defaultPrimingLimit,defaultGoStatic);
  val defaultScaling = (2,2)
  val defaultDirection = Left;
  
  (*Create a new compactor which will always combine
  the specified number of functions*)
  fun new_static n = if (n<=0) then
    raise Compactor_Use_Error ("Cannot create a static compactor expected to compact a non-positive" ^
    " number of elements")
    else (defaultDirection,Static n,defaultScaling)
  
  (*Create a new compactor which will change
  how many functions are compacted according to how long
  it takes to run each one (times the functions).
  You specify how long you want each returned function
  to take roughly*)
  fun new_dynamic time = if (time<Time.zeroTime) then
    raise Compactor_Use_Error ("Cannot create a compactor expected to produce functions which" ^
    " require negative time to run")
    else (defaultDirection,Dynamic (time,defaultEstimate,defaultPriming),defaultScaling)
  
  (*For a dynamic compactor, set the time to aim for*)
  fun set_time (dir,Dynamic (_,est,pr),scaling) t =
    if (t<Time.zeroTime) then raise Compactor_Use_Error ("Cannot create a compactor expected to produce functions which" ^
    " require negative time to run")
    else
    (dir,Dynamic (t,est,pr),scaling)
    | set_time _ _ = raise Compactor_Use_Error ("Cannot set the time " ^
    "of a static compactor")
  
  (*Return the time a dynamic compactor is aiming for*)
  fun get_time (_,Dynamic (t,_,_),_) = t
    | get_time _ = raise Compactor_Use_Error ("Cannot get the time " ^
    "of a static compactor")
  
  (*For a static compactor, set the size of the groups created*)
  fun set_size (dir,Static (_),scaling) n = if (n<=0) then
    raise Compactor_Use_Error ("Cannot create a static compactor expected to compact a non-positive" ^
    " number of elements")
    else (dir,Static n,scaling)
    | set_size _ _ = raise Compactor_Use_Error ("Cannot set the size of a dynamic " ^
    "compactor")
  
  (*For a static compactor, return the size of the groups it is going
  to produce*)
  fun get_size (_,Static n,_) = n
    | get_size _ = raise Compactor_Use_Error ("Cannot get the size of a dynamic " ^
    "compactor")
    
  (*Returns true if a compactor is dynamic*)
  fun is_dynamic (_,Static _,_) = false
    | is_dynamic _ = true
  
  (*Returns true if a compactor is static*)
  fun is_static (_,Static _,_) = true
    | is_static _ = false
  
  (*For a dynamic compactors only, set the estimated number of functions
  that you think need to be compacted to use up the time given to this compactor
  If NONE is supplied, it will use a default.
  
  Default: NONE
  *)
  fun set_estimated_number (dir,(Dynamic (t,_,p)),scaling) i = (dir,Dynamic (t,i,p),scaling)
    | set_estimated_number _ _ = raise Compactor_Use_Error
    "Cannot set the estimated number of functions in a dynamic compactor."
  
  (*Return the estimated number of functions (set by you) that should be
  compacted to make up the required amount of time
  
  Default: NONE
  *)
  fun get_estimated_number (_,(Dynamic (_,i,_)),_) = i
    | get_estimated_number _ = raise Compactor_Use_Error
    "Cannot retrieve the estimated number of functions in a dynamic compactor."
    
  (*Sets the direction in which functions are compacted.
  This is useful for efficiency.
  
  Right: f1 (f2 (f3 (f4 f_id)))
  Left: f4 (f3 (f2 (f1 f_id)))
  
  Default: Left
  *)
  fun set_fold_direction (_,cp,scaling) dir = (dir,cp,scaling)
  
  (*Get the direction in which functions are compacted
  
  Default: Left
  *)
  fun get_fold_direction (dir,_,_) = dir
  
  (*Tell the compactor to prime itself. Priming a compactor means
  that it will precompute groups of functions initially sequentially
  to try to obtain a good estimate of how many functions to use
  before it starts returning functions to you.
  
  Note: its precomputed groups will be returned to you as
  dummy functions, so the first few will be nearly instant to process.
  Not many groups should be precomputed.
  *)
  fun prime (dir,Dynamic (t,est,_),scaling) limit go_static =
    if (is_none limit) then (dir,Dynamic (t,est,SOME (limit,go_static)),scaling)
    else (if ((the limit)<0) then
      raise Compactor_Use_Error
      "Cannot prime a dynamic compactor with a negative limit."
      else (dir,Dynamic (t,est,SOME (limit,go_static)),scaling))
    | prime _ _ _ = raise Compactor_Use_Error
    "Cannot prime a static compactor."
  
  (*Tell the compactor not to prime itself first*)
  fun do_not_prime (dir,Dynamic(t,est,_),scaling) = (dir,Dynamic (t,est,NONE),scaling)
    | do_not_prime cp = cp
    
  (*Return whether or not the compactor is priming, and if it is, the details.
  The returned value has the format:
  
  NONE if it is not going to prime first
  SOME (an optional limit on how many groups of functions it can be precompute,
  whether or not it should become a static compactor after priming)*)
  fun check_prime (_,Dynamic(_,_,pr),_) = pr
    | check_prime _ = NONE
    
  (*Set the scaling for the compactor*)
  fun set_scaling (_,Static _,_) _ = raise Compactor_Use_Error "Cannot set the scaling factors for a static compactor."
    | set_scaling (dir,cp,_) (high,low) = if (high<1) then
        raise (Compactor_Use_Error "Cannot set the scaling up factor to be less than one")
      else
      if (low<1) then
        raise (Compactor_Use_Error "Cannot set the scaling down factor to be less than one")
      else
        (dir,cp,(high,low))
      
    
  (*Return the scaling in use by the compactor. If you have not set any,
  then this will return the defaults used*)
  fun get_scaling (_,_,scaling) = scaling
    
  (*Combine a list of functions in the appropriate direction*)
  fun combine _ [] res = res
    | combine Right (f::fs) res = f (combine Right fs res)
    | combine Left (f::fs) res = combine Left fs (f res)
    
  (*Grab the specified number of functions from the producer and return
  the next element to be given to the prodcuer.
  It will return NONE instead of the next element if the producer finishes*)
  fun grab 0 _ next_input current = (current,SOME next_input)
    | grab n producer next_input current =
    let val ans = producer next_input in
      case ans of
          NONE => (current,NONE)
        | SOME (g,next_input) => grab (n-1) producer next_input (g::current)
    end
  
  (*****************************************)
  (**********Functions for static***********)
  (*****************************************)
  
  (*Generates the sequence of groups of functions for a static compactor...
  
  Args: direction
        size of group
        producer
        next input for producer
        identity element for little functions
  *)
  fun choose_static dir n producer next_input f_id =
    Seq.make (fn() =>(*First, grab however many functions I want*)
    let val (fs,new_prod) = grab n producer next_input [] in
    case fs of [] => NONE (*Empty to begin with*)
             |  _ => SOME (
      (*The next element in the sequence*)
      (fn () => combine dir fs f_id,n)
    ,
    (*The remaining sequence*)
    case new_prod of
        NONE => Seq.empty (*No more little functions left*)
      | SOME next_input => choose_static dir n producer next_input f_id
    )
    end)
  
  (*****************************************)
  (*********Functions for dynamic***********)
  (*****************************************)
  
  (*
  How the estimated time changes:
  
  The value passed around is held as a five tuple (1,2,3,4,5):
  
  (1) = The total amount of time taken (for the number of functions)
  (2) = The number of functions that were used to create this total
  (3) = 75% of the average = 3/4 * ((1)/(2)) (calulated here for efficiency)
  (4) = 125% of the average = 5/4 * ((1)/(2)) (calulated here for efficiency)
  (5) = The time stamp
  
  **********************
  Creating a suggestion:
  **********************
  
  When a suggestion is computed, it will look at the current
  total time and see if it is above or below the target time.
  
  If it is below the target time, it will double the number of functions compacted.
  If the estimated time due to doubling goes above the target, it will
  not double but scale up to meet the estimated amount of time.
  
  If it is above the target time, it will halve the number of functions compacted.
  If the estimated time due to halving goes below the target, it will
  not double but scale down to meet the estimated amount of time.
  
  ***********************
  Adjusting the estimate:
  ***********************
  
  I have run a lot of the functions and have the time it took them to run. I now
  take the following steps:
  
  Firstly, I compare my time stamp with the one in the estimate.
  If the time stamp in the estimate is higher than mine, then I am late
  and my data should probably be ignored, so I do nothing.
  
  Assuming I am newer, I now look if my total time was closer to the target than the stored time.
  If so, I store my result and compute the average appropriately, provided that the new time
  is at least 10% different from the stored time.
  
  If my time is actually worse, I compute my average time and see if it varies
  from the previous average time by more than a specific percentage. This is done by seeing
  if my average falls between 25% either side of the existing average
  (So 75% (3/4) of the existing average and 125% (5/4) of it). If I do
  fall between this result, then I decide to record my result. Otherwise, I do not
  worry.
  *)
  
	(*
	Adjust the current estimated times.
	
	Args: the recorded time for this group
	      the number of functions in this group
	      a reference to the shared estimate
	      the time stamp for this group
	      the target time for this compactor
	
	Note: this is optimised to avoid computing anything unnecessarily,
	which makes the code look very ugly...*)
	fun adjust rec_t n my_ref time_stamp target_t =
	  let
	     val (total,_,low_avg,high_avg,stamp) = !my_ref; (*The current estimate*)
	     val time = (Time.toMicroseconds rec_t) : LargeInt.int
	     (*My average*)
	     val my_avg = (time div n) : LargeInt.int;
	     (*75% of my average*)
	     val my_low_avg = ((my_avg * (3 : LargeInt.int)) div (4 : LargeInt.int)) : LargeInt.int;
	     (*125% of my average*)
	     val my_high_avg = (low_avg + ((my_avg div (2 : LargeInt.int)) : LargeInt.int)) : LargeInt.int
	     (*What I would update the estimate to*)
	     val new_est = (time,n,my_low_avg,my_high_avg,time_stamp) : (LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int);
	  in
	    if (stamp>time_stamp) then () else
	      (*See if the new time is significantly better (10%)*)
	      if (abs(target_t-time)<abs(target_t-total) andalso (
	      abs(time-total)>(total div 10))) then (*My time is better, so attempt a recording:*)
	        (my_ref:=new_est handle exn => if Exn.is_interrupt exn then reraise exn else ())
	      else
	        (*My time is worse. See if this failure was due to
	        a problem with the average*)
          if (low_avg<=my_avg andalso my_avg<=high_avg) then
          () (*Probably not a significant enough issue to report*)
          else
          (*The average previously computed has been recorded badly, so report
          my result!*)
          (my_ref:=new_est handle exn => if Exn.is_interrupt exn then reraise exn else ())
	  end
	
	(*Calculate how many functions should be compacted
	based on the previous results...
	*)
	fun suggested t (total,no_fs,_,_,_) (scaleup,scaledown) =
	  let
	    val ans = if (total>t) then
	      (*The current total is above the target!*)
	      if ((total div (scaledown : LargeInt.int))>=t) then
	        (*Halving (scaling down) is still above, so do this...*)
	        (no_fs div (scaledown : LargeInt.int)) : LargeInt.int
	      else
	        (*Scale to the right number of functions*)
	        ((t * no_fs) div total) : LargeInt.int
	    else
	      (*The current total is lower than the target*)
	      if ((total * (scaleup : LargeInt.int))<=t) then
	        (*Doubling (scaling up) is still below, so do this...*)
	        (no_fs * (scaleup : LargeInt.int)) : LargeInt.int
	      else
	        (*Scale to the right number of functions*)
	        ((t * no_fs) div total) : LargeInt.int
	  in
	    if (ans<=(0 : LargeInt.int)) then (1 : LargeInt.int) (*Minimum number*) else ans
	  end

  (*Create the sequence of groups for the dynamic compactor
  
  Args: the time stamp for the next group
        the direction when combining little functions
        the target time for groups
        the producer of the little functions
        the next element to be fed to the producer
        the first element to be fed to the little functions
        the reference to the shared estimate
        the scaling factors to use when updating the estimate
  *)
  fun choose_dynamic time_stamp dir t producer next_input f_id my_ref scaling =
    Seq.make (fn() =>(*First, grab however many functions I want*)
    let val sg = (suggested t (!my_ref) scaling) (*Get the suggested number*)
        val (fs,new_prod) = grab sg producer next_input [] in
    case fs of [] => NONE (*Empty to begin with*)
             |  _ => SOME (
      (*The next element in the sequence*)
      ((fn () =>
        (*This is the "hard" computational part of the algorithm*)
        let
          (*Time how long the computation took*)
          val sw = Stopwatch.start (Stopwatch.new);
          val ans = combine dir fs f_id; (*Do the computation...*)
          val time = fst (Stopwatch.stop sw)
        in (
          (*Now that I've done the computation, it is time to adjust
          the suggested number of elements to be compacted*)
          adjust time sg my_ref time_stamp t;
          (*Return the answer*)
          ans)
        end),sg)
    ,
    (*The remaining sequence*)
    case new_prod of
        NONE => Seq.empty (*No more little functions left*)
      | SOME next_input => choose_dynamic (time_stamp+1) dir t producer next_input f_id my_ref scaling
    )
    end)
    
  
  (*Prime the dynamic compactor first. This uses almost the same algorithm for computing
  the estimates, but does this work sequentially initially.
  The algorithm is modified a little to handle EXTREMELY (less than a microsecond) quick
  functions which the ordinary dynamic algorithm would be confused by due to random variation.
  Hence, this has a higher overhead and updates more often.
  
  Args: whether or not the compactor is nearly stable (requires stability over two precomputed groups)
        the time stamp for the next group of little functions
        the direction to combine little functions in
        the target time for groups
        the producer of little functions
        the next input for the producer
        the first element to be fed to a little function
        the current estimate
        the current number of precomputed functions that are permitted left (setting this to -1 effectively
        means priming will continue until stability is reached
        whether or not the compactor should become static after priming is complete
        the scaling parameters to use when updating the estimate
  *)
  fun prime_dynamic _ time_stamp dir t producer next_input f_id (est as (_,no_fs,_,_,_)) 0 go_static scaling =
      (*Need to stop priming*)
      if (go_static) then (*Become a static compactor*)
        choose_static dir no_fs producer next_input f_id
      else (*Stay as a dynamic compactor*)
       choose_dynamic (time_stamp+1) dir t producer next_input f_id (Unsynchronized.ref est) scaling
    (*Normal case...*)
    | prime_dynamic stable time_stamp dir t producer next_input f_id (est as (total,no_fs,low_avg,high_avg,_)) limit go_static scaling =
      Seq.make (fn() =>
      let
        val sg = suggested t est scaling; (*Compute the suggested number of functions...*)
        val (fs,new_prod) = grab sg producer next_input []
      in
      case fs of [] => NONE (*Empty to begin with*)
               |  _ =>
        (*The next element in the sequence*)
        let
          (*Time how long the computation took*)
          val sw = Stopwatch.start (Stopwatch.new);
          val ans = combine dir fs f_id;
          val time = Time.toMicroseconds (fst (Stopwatch.stop sw));
          (*Work out what the adjustment should be...*)
          (*My average time*)
          val my_avg = (time div sg) : LargeInt.int;
          (*75% of my average time*)
          val my_low_avg = ((my_avg * (3 : LargeInt.int)) div (4 : LargeInt.int)) : LargeInt.int;
          (*125% of my average time*)
          val my_high_avg = (low_avg + ((my_avg div (2 : LargeInt.int)) : LargeInt.int)) : LargeInt.int
          (*The estimate I would use*)
          val new_est = (time,sg,my_low_avg,my_high_avg,time_stamp) : (LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int);
          (*The new estimate that will be used*)
          val res_est = (*I know my time stamp is better than the existing one, so I don't need to check this.*)
            if ((abs(t-time)<abs(t-total) orelse (total<5000)) andalso (
            (*Heuristic for very small times above*)
            (*At extremely small times, it is hard to see an improvement*)
            abs(time-total)>(total div 10) orelse (
            (*If there was no change, did the number of functions change in the right direction?*)
            (total<t andalso no_fs<sg) orelse (total>t andalso no_fs>sg)
            ))
            ) then
              (*My time is better, so change the estimate*)
              new_est
            else
              (*My time is worse. See if this failure was due to
              a problem with the average*)
              if (low_avg<=my_avg andalso my_avg<=high_avg) then
                est (*Probably not a significant enough issue to report*)
              else
                (*The average previously computed has been recorded badly, so report
                my result!*)
                new_est
           fun get_no_fs (_,fs,_,_,_) = fs (*Return the number of functions in an estimate*)
        in
          (*Return the next part of the sequence...*)
          SOME ((fn () => ans,sg),
            (*The remaining sequence*)
            case new_prod of
                NONE => Seq.empty (*No more little functions*)
              | SOME next_input => 
              (*See if the situation is now considered stable...*)
              if ((get_no_fs res_est)=no_fs) then
                if stable then (*The result has stabilised! Use this...*)
                  if (go_static) then (*Turn into a static compactor!*)
                    choose_static dir no_fs producer next_input f_id
                  else (*Stay dynamic*)
                    choose_dynamic (time_stamp+1) dir t producer next_input f_id (Unsynchronized.ref est) scaling
                else (*Mark it as stable for the next attempt*)
                  prime_dynamic true (time_stamp+1) dir t producer next_input f_id res_est (limit-1) go_static scaling
              else (*Definitely not ready to stop priming yet...*)
                prime_dynamic false (time_stamp+1) dir t producer next_input f_id res_est (limit-1) go_static scaling
          )
        end
      end
      )
    
  (*
  This is the most generic way to set up a compactor. Look at the argument
  descriptions to know how to use this.
  *)
  fun compact (dir,compactor,scaling) producer p_id f_id = case compactor of
      Dynamic (t,est,do_prime) =>  (*Wants a dynamic compactor*)
        let
          val time = (Time.toMicroseconds t) (*Convert the time to microseconds*)
          val start_est = if (is_none est) then (*The first estimate*)
                (1,0,0,0,0) (*Guess the functions take no time at all*)
                : (LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int)
              else (*Use the estimate provided*)
                let val est = (the est) : LargeInt.int;
                    (*Calculate the estimate amounts of time*)
                    val avg = (time div est) : LargeInt.int;
                    val low_avg = (((3 : LargeInt.int) * avg) div (4 : LargeInt.int)) : LargeInt.int;
                    val high_avg = (low_avg + ((avg div (2 : LargeInt.int)) : LargeInt.int)) : LargeInt.int
                in
                  (time,est,low_avg,high_avg,0) (*Form the supplied estimate*)
                  : (LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int)
                end
        in
          case (do_prime) of
              NONE => (*No need to prime the compactor first*)
              choose_dynamic 1 dir time producer p_id f_id (Unsynchronized.ref start_est) scaling
            | SOME (limit,go_static) => (*I want to prime first!
              Sets the limit to -1 if no limit is in place, since this effectively allows priming to
              go on forever if necessary*)
              prime_dynamic false 1 dir time producer p_id f_id start_est (if (is_none limit) then (~1) else (the limit)) go_static scaling
        end
        (*Create the static compactor (much easier)*)
    | Static n => choose_static dir n producer p_id f_id
  
  (****************************************)
  (********Simplification functions********)
  (****************************************)
  
  (*Compact by taking the little functions from a sequence*)
  fun compact_with_seq comp seq f_id =
    compact (set_fold_direction comp Left) (fn sq => Seq.pull sq) seq f_id
  
  (*Compact by taking the little functions from a list*)
  fun compact_with_list comp ls f_id =
    compact (set_fold_direction comp Left) (fn [] => NONE
                   | (x::xs) => SOME (x,xs)) ls f_id
  
  (*Compact over a sequence with a function designed to take an element from
  the existing sequence and return an entirely new sequence. The resulting
  sequences will be appended together!
  
  Synonymous to Seq.maps
  *)
  fun compact_maps_seq comp f seq = Seq.map (fn (x,_) => x) (
    compact_with_seq (set_fold_direction comp Left) (
    Seq.map (fn x => fn sq => Seq.append (f x) sq) seq) Seq.empty)
  
  (*Compact a function which will take an element from
  the given sequence, and convert it to an element for a new sequence
  
  Synonymous to Seq.map*)
  fun compact_map_seq comp f seq = Seq.map (fn (x,_) => x) (
    compact_with_seq comp (
    Seq.map (fn x => fn sq => Seq.cons (f x) sq) seq) Seq.empty)
  
  (*Similar to compact_maps_seq but for lists*)
  fun compact_maps_list comp f ls = Seq.map (fn (x,_) => x) (
    compact_with_list comp (
    List.map (fn x => fn xs => ((f x)@xs)) ls) [])
  
  (*Similar to compact_map_seq but for lists*)
  fun compact_map_list comp f ls = Seq.map (fn (x,_) => x) (
    compact_with_list comp (
    List.map (fn x => fn xs => (f x)::xs) ls) [])
end;
