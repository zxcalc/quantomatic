(*  Title:      Pure/Concurrent/simple_future.ML
    Author:     Michael James Bradley
    
This signature and structure acts as a wrapper around the Futures library, and so provides
simplified access to some of its most important method calls.

Additional methods refer to the implicit cancelling of futures and the passing of future data.
Future data enables futures to automatically cancel themselves and provide a very basic technique for
debugging. Inside this data, each thread keeps track of a unique ID, which can be used to determine
its position within the hierarchy of future groups when it was constructed (and possibly failed).
If a function creating a future terminates, all futures it created using future data are automatically
terminated as well.
*)

(*The signature for the library*)
signature FUTURE_WRAPPER =
sig
	(*The type of a synchronised variable*)
	type 'a var
	
	(*Wrap the raw ML reference in a mutex and condition variable
	for the pthread protocol*)
	val var : 'a -> 'a var
	(*Read the internal reference variable. This will read whatever the value
	is at the given time. (No need to synchronize)*)
	val value: 'a var -> 'a
	(*For guarded_access v f:
	x = value v
	If f x produces NONE then the condition within f has failed, and so the
	the function will be executed at a later suitable time.
	If f x produces SOME (y,x2) then the new value of v is set to x2 and
	y is returned
	*)
	val guarded_access: 'a var    (*The varable you wish to use or modify (atomically)*)
	  -> ('a -> ('b * 'a) option) (*The operation to perform on the variable*)
	  -> 'b                       (*Return the first element of the result of the operation*)
	
	(*Change the value held in a variable with synchronized access.*)
	val change : 'a var (*The variable to change*)
	  -> ('a -> 'a)     (*A function to update the value, dependent on the previous value*)
	  -> unit           (*Returned only strictly after the change has been made*)
	
	(*The type of a future*)
	type 'a future
	(*The type of a result (before an exception can be thrown)*)
	type 'a result
	(*The type of a future group*)
	type group
	
	(*Turn a function into a task which will be evaluated at some point
	in the future*)
	val future: (unit -> 'a) (*The task to be evaluated*)
	  -> 'a future           (*The resulting future. The task may not have been evaluated yet*)
	(*Return the result of a future*)
	val join: 'a future (*The future itself*)
	  -> 'a             (*The result of the computation of its task (hence this is blocking)*)
	(*Cancel the computation of a future*)
	val cancel: 'a future -> unit
	(*Test to see if the value in a future has been evaluated yet*)
	val is_finished: 'a future -> bool
	(*Turn a value directly into a future, in case a function is expecting that
	type (does not perform any evaluation - this is for performance)*)
	val future_value: 'a -> 'a future
	(*Faster way to manipulate values in futures by trying to reuse
	the same thread.*)
	val future_map: ('a -> 'b) (*The function to apply to the value once it has been computed*)
	  -> 'a future             (*The original future*)
	  -> 'b future             (*The new future. Note that the original future may not have been computed yet either*)
	(*Calculate the result of a function, capturing any exceptions*)
	val capture: ('a -> 'b) -> 'a -> 'b result
	(*Release any exception of the given result
	If there was no exception, the value that was returned is given to you*)
	val release: 'a result -> 'a
	(*Release all values in a list in the given order. The first exception
	encountered is thrown*)
	val release_first: 'a result List.list -> 'a List.list
	(*Return the values of a list of futures as results (not throwing their
	exceptions yet*)
	val join_results: 'a future List.list -> 'a result List.list
	(*Create a new empty future group with the given group as a parent.
	If no parent is given, this is a group at the highest level (exceptions
	propogate to no other groups*)
	val new_group: group option -> group
	(*Create a future as normal with the given function, but also
	add that function to the given future group*)
	val future_group: group (*The future group to be added to. If the group
	                          is cancelled, this future is cancelled as well*)
	  -> (unit -> 'a)       (*The task to be computed at some point*)
	  -> 'a future          (*The resulting future*)
	(*Cancel all future tasks within a group*)
	val cancel_group: group -> unit
	
	(*Additional functions for futures:*)
	
	(* Since futures do not automatically cancel nested futures
	upon termination, it is very easy to leave many dangling
	calls. Hence, the following functions wrap around the future's library
	to cope with this, but require you to pass "future_data" around
	in your own functions.*)
  
  (*The type of data passed between future calls*)
  type future_data;
  
  (*This call is similar to future_group, except that
  you must pass your data into it and the function to be run
  should expect future data.
  
  If the caller is the parent, and the future produced is the child,
  then:
  
  1. When the parent finishes the child will be cancelled.
  2. If the child thread terminates, any children of its own created
  with this function will automatically be cancelled too.
  
  In particular, point 2 means that you only have to "clear up" manually
  in the outermost function (of nested futures).
  *)
  val future_group_data : future_data (*The parent future data*)
    -> (future_data -> 'a)            (*The task to compute at some point, which
                                        should expect to be given future data*)
    -> ('a future)
  
  (*
  This can be called manually. Any futures created
  with the given future data (passed into "future_group_data") will be
  cancelled.
  *)
  val clear_up : future_data -> unit
  
  (*
  Converts the future into a string. This will have some form
  of an ID to recognise the "future" running (for debugging purposes)
  as well as a custom message. (See implementation - best to create your own)
  *)
  val future_data_to_string : future_data -> string
  
  (*
  Return new future data which is effectively empty. The string you
  pass is the message it should have (if you want to include one) attached to it.
  This is similar to creating a future group with no parent.
  *)
  val new_future_data : string option -> future_data
  
  (*
  This function copies your current future data to another which
  uses a different future group. That means that you can split up future
  calls and cancel separately. Consider the following example:
  
  f () =
    future y;future z;...
    g ();
    future a;future b;...
    ...return x...
  
  g () =
    future c;future d;...return y...
    
  g() was not called with a future. If you used the function
  future_group_data for all your future calls (like you were
  supposed to), then g()'s futures can only be cleared up when you're
  ready to clear up f()'s futures, since you're using the same group.
  (Automatically if f() is not an outermost function, g()'s futures
  would only be cancelled once f() has terminated).
  Hence, it can be worth copying the future data.
  *)
  val copy_future_data : future_data -> future_data
  
  (*Set a message to be used whenever the future data is printed.
  Returns the new future data to use.*)
  val set_message : future_data -> string option -> future_data
  
  (*Return the message in the future data*)
  val get_message : future_data -> string option
  
  (*Get the future id. This is a list of integers. Each integer
  represents the number of the call of future_group_data
  within that future. Hence, it looks like this:
  
  main () = []
    future (... [1]
      future (...) [1,1]
      future (... [1,2]
        future (...) [1,2,1]
        future (...) [1,2,2]
      ...)
      future (...) [1,3]
    ...)
    future (...) [2]
    
  Note that the root caller's list is [].
  *)
  val get_future_id : future_data -> int list
end;

structure Future_Wrapper :> FUTURE_WRAPPER =
struct
	(*For synchronized access to variables:*)
	type 'a var = 'a Synchronized.var
	(*Wrap the raw ML reference in a mutex aand condition variable
	for the pthread protocol*)
	fun var x = Synchronized.var "" x
	(*Read the internal reference variable. This will read whatever the value
	is at the given time. (No need to synchronize)*)
	val value = Synchronized.value
	(*For guarded_access v f:
	x = value v
	If f x produces NONE then the condition within f has failed, and so the
	the function will be executed at a later suitable time.
	If f x produces SOME (y,x2) then the new value of v is set to x2 and
	y is returned
	*)
	val guarded_access = Synchronized.guarded_access
	
	(*Change the value held in a variable with synchronized access *)
	val change = Synchronized.change
	
	(*Using futures:*)
	type 'a future = 'a Future.future
	type 'a result = 'a Exn.result
	type group = Future.group
	
	(*Turn a function into a task which will be evaluated at some point
	in the future*)
	val future = Future.fork
	(*Return the result of a future*)
	val join = Future.join
	(*Cancel the computation of a future*)
	fun cancel f = ((Future.cancel f);())
	(*Test to see if the value in a future has been evaluated:*)
	val is_finished = Future.is_finished
	(*Turn a value directly into a future, in case a function is expecting that
	type (doesn't require evaluation as such)*)
	val future_value = Future.value
	(*Faster way to manipulate values in futures by trying to reuse
	the same thread.*)
	val future_map = Future.map
	(*Calculate the result of a function, capturing any exceptions*)
	fun capture f x = Exn.Res (f x)
	  handle exn => if Exn.is_interrupt exn then reraise exn else Exn.Exn exn;
	(*Release any exception of the given result*)
	fun release (Exn.Res y) = y
	  | release (Exn.Exn e) = raise e
	(*Release all values in a list in the given order. The first exception
	encountered is thrown*)
	fun release_first [] = []
	  | release_first (x::xs) = let fun match (Exn.Exn e) = raise e
	  				  | match (Exn.Res y) = y::(release_first xs) in
	  				  match x end
	(*Return the values of a list of futures as results (not throwing their
	exceptions yet*)
	val join_results = Future.join_results
	(*Create a new empty future group with the given group as a parent.
	If no parent is given, this is a group at the highest level (exceptions
	propogate to no other groups*)
	val new_group = Future.new_group
	(*Create a future as normal with the given function, but also
	add that function to the given future group
	
	TODO: find out how this is supposed to be done!*)
	fun future_group g f = List.hd (Future.forks ({name = "fork", group = SOME g, deps = [], pri = 0, interrupts = true}) [f])
	(*Cancel all future tasks within a group*)
	fun cancel_group g = (Future.cancel_group g;())
	
	(*Additional functions for futures:*)
  
	(*
	
	WARNING:
	
	For efficiency reasons again, the functions below are not thread
	safe in the sense that if the same future_data is used across
	multiple threads, it might not work. This would be considered
	improper use since future data is naturally kept unique between threads.
	*)
	
  (*The type of data passed between future calls.
  
  Note: the middle int is the next id to use for a future call
  The string is any additional message to be added to the future
  id when printing
  
  Important: in proper use the referenced int is
  thread safe since the future_data should be unique to every thread.*)
  type future_data = (int list) * (int Unsynchronized.ref) * (Future.group) * string option
  
  (*This function is an augmentation to the standard future group call.
  It takes the existing data from a future call.
  Returns the new data to be used by the calling function and
  the future that was created.
  The function passed in should have the type:
  
  f : future_data -> 'a
  *)
  fun future_group_data (id_list,new_id,gr,m) f =
    let
      val new_group = Future.new_group (SOME gr); (*gr becomes the parent group*)
      val new_id_list = (!new_id)::id_list; (*attach the id*)
      val _ = new_id:=(!new_id)+1 (*Increase the unique identifier*)
    in
      (* (Data for caller,future) *)
      future_group gr (fn() => (
        let val ans = f (new_id_list,Unsynchronized.ref 1,new_group,m) in
          (cancel_group new_group; (*Cancel any dangling future calls I made*)
          ans (*Return the result*)) end
          ))
    end
  
  (*This should be called when a future is about to terminate. It will automatically
  cancel all the futures created within this function call
  
  Does nothing if the data contains no group*)
  fun clear_up (_,_,gr,_) = cancel_group gr
  
  (*For converting the future data into a string...
  Terminates with a new line, and prints the message
  after the future id. The future id is printed in the right order!*)
  fun future_data_to_string (id_list,_,_,m) =
    let
      val message = if (is_none m) then "" else the m;
      (*Convert the id list to a string...*)
      fun int_list_to_string [] = "]"
        | int_list_to_string [x] = Int.toString(x) ^ "]"
        | int_list_to_string (x::xs) = Int.toString(x) ^ "," ^ (int_list_to_string xs)
    in
      (*The id followed by the message*)
      "ID: [" ^ (int_list_to_string (List.rev id_list)) ^ " " ^ message ^ "\n"
    end
  
  (*Return new effectively empty future data, but with the given message
  This contains a default group*)
  fun new_future_data m = ([],Unsynchronized.ref 1,new_group NONE,m)
  
  (*This copies your future data except for the group.
  Instead, a fresh new group is supplied.
  Hence, when a function inside clears up, it won't cancel your futures
  This technique is quite inefficient (unnecessary group
  creation and cancellation ~probably)! Better care in code
  produces better results...
  
  Futures created with the new data essentially hold the same id
  and message so for printing purposes, it looks the same and futures
  created from it appear to have branched off of the original
  future data.*)
  fun copy_future_data (id_list,new_id,_,m) = (id_list,new_id,new_group NONE,m)
  
  (*Set a message to be used whenever the future data is printed.
  Returns the new future data to use.*)
  fun set_message (id_list,new_id,gr,_) m = (id_list,new_id,gr,m)
  
  (*Return the message in the future call*)
  fun get_message (_,_,_,m) = m
  
  (*Return the id of the future in reverse order*)
  fun get_future_id (id_list,_,_,_) = id_list
end;
