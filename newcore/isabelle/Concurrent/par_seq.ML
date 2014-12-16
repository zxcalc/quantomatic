(*  Title:      Pure/Concurrent/par_seq.ML
    Author:     Michael James Bradley
  
This is a signature for a parallel (lazy) sequence. It is exactly the same as for an ordinary
sequence, except for four (+1) functions which can convert to and from
the sequential sequence to the parallel sequence. The one other exception
is the format of "make" explained in the signature below

If an element in the sequence raises an exception, this should only be raised
when the element is pulled from the sequence.

****************************************************************************************************
The use of the immediate signature and structure below is discouraged. It became quite complicated
to manage parameters within the parallel lazy sequence at the same time as its own parallel
implementation. Hence, a wrapper structure named Safe_Parallel_Seq was created, which manages
issues surrounding the passing in of future data, and the setting of parameters within the sequence.
****************************************************************************************************
*)
signature PARALLEL_SEQ =
sig
  (*An empty sequence exception. This is raised when you demand the head or
  tail of an empty sequence*)
  exception Empty_Parallel_Sequence of string
  (*The type of a parallel sequence*)
  type 'a pseq
  (*This is the type of parameters to pass into the parallel lazy sequence,
  which governs how elements are pulled
  
  1st element - how many elements to pull at the same time. If NONE, all elements
  are pulled and this is no longer lazy (if elements are cheap, this is risky)
  2nd element - if fewer than this number of elements in the list have been preprocessed, then
  pull another chunk of elements in parallel again
  3rd element - the minimum time boundary in microseconds that tasks should take before
  tasks are processed in parallel. If 1st is NONE, this will be ignored as soon as one function
  takes longer than the time boundary. This is to protect efficiency.
  *)
  type param = (int option * int * int)
  
  (*Add a function for future evaluation to the sequence.
  The format is different to the form of make in the original sequential sequence
  because the functions you pass in are modified. Hence, passing in a function
  of the form (unit -> 'a * 'a seq) is not helpful because the returned 'a cannot
  be modified without evaluating the entire function.*)
  val make : (Future_Wrapper.future_data -> 'a) (*An element to add to the sequence*)
    -> 'a pseq (*An existing sequence*)
    -> 'a pseq (*A new sequence, with the element attached to the front of the passed in sequence*)
  (*Create the empty sequence.*)
  val empty : 'a pseq
  (*Get the next element from the sequence. Returns NONE
  if a next element does not exist. This requires parameters to specify
  the behaviour of the pull operation (how many elements are computed in parallel for example)
  Pulling from the same sequence twice (to return exactly the same element)
  can be especially painful since further values in the sequence may be
  re-evaluated too!*)
  val pull : Future_Wrapper.future_data -> 'a pseq -> param -> ('a * 'a pseq) option
  (*Attach an element to the front of the sequence*)
  val cons : 'a -> 'a pseq -> 'a pseq
  (*Create a sequence with a single element*)
  val single : 'a -> 'a pseq
  (*Attempt to apply a function to an element. If it succeeds, it will return
  a singleton sequence containing the answer. If it fails an empty sequence
  will be returned*)
  val try : ('a -> 'b) -> 'a -> 'b pseq
  (*Return the first element of the sequence.
  Raises empty sequence exception if there is no head
  Calling this on a sequence more than once may force evaluation of the head
  multiple times (dependent on whether or not it has begun evaluation).*)
  val hd : Future_Wrapper.future_data -> 'a pseq  -> param -> 'a
  (*Return the tail of the sequence
  Raises empty sequence exception if there is no tail
  As with hd, calling this multiple times may incur a penalty.
  This is much less likely with tl, and if no form of removal (typically filtering)
  of the elements was applied, then it is basically free since
  the element dropped will not be evaluated unnecessarily.*)
  val tl : 'a pseq -> 'a pseq
  (*Return a list of the first n elements (evaluated) paired with the rest*)
  val chop : Future_Wrapper.future_data -> int -> 'a pseq -> param -> (('a list) * ('a pseq))
  (*Take the first n elements of the sequence. (Forget the rest)*)
  val take : int -> 'a pseq -> 'a pseq
  (*Convert the sequence to a list of values.
  WARNING: This forces evaluation of the entire sequence!!*)
  val list_of : Future_Wrapper.future_data -> 'a pseq -> param -> 'a list
  (*Convert a list back into a sequence.*)
  val of_list : 'a list -> 'a pseq
  (*Join two sequences together (first appears in front)*)
  val append : 'a pseq -> 'a pseq -> 'a pseq
  (*Map a function over a sequence. This is done lazily like all of the functions*)
  val map : ('a -> 'b) -> 'a pseq -> 'b pseq
  (*Apply a function to the first sequence, and append the result
  to the next sequence*)
  val mapp : ('a -> 'b) -> 'a pseq -> 'b pseq -> 'b pseq
  (*Interleave two sequences together, starting with the first
  If one runs out, the other is appended to the end.*)
  val interleave: 'a pseq * 'a pseq -> 'a pseq
  (*Filter the values in a sequence. This is still lazy to a point.
  (See implementation details)*)
  val filter : Future_Wrapper.future_data -> ('a -> bool) -> 'a pseq -> param -> 'a pseq
  (*Flatten (concatenate) a sequence of sequences. This too maintains
  laziness across the sequence of sequences and in each individual sequence*)
  val flat : Future_Wrapper.future_data -> 'a pseq pseq -> param -> 'a pseq
  (*Maps a function which produces a sequence
  for every element in the original sequence. It automatically
  appends all of the results*)
  val maps : Future_Wrapper.future_data -> ('a -> 'b pseq) -> 'a pseq -> param -> 'b pseq
  (*Maps a filter function over the sequence. If the filter returns
  none, the element is dropped. If the filter returns SOME b then
  b is attached to the sequence.*)
  val map_filter : Future_Wrapper.future_data -> ('a -> 'b option) -> 'a pseq -> param -> 'b pseq
  (*Apply a function over the sequence. The second element
  is paired with this function for every single value*)
  val lift : ('a -> 'b -> 'c) -> 'a pseq -> 'b -> 'c pseq
  (*Similar to lift but appends the results of the function*)
  val lifts : Future_Wrapper.future_data -> ('a -> 'b -> 'c pseq) -> 'a pseq -> 'b -> param -> 'c pseq
  (*Creates a singleton sequence element with the given value
  and future data*)
  val succeed : 'a -> 'a pseq
  (*Returns an empty sequence regardless of the element*)
  val fail : 'a -> 'b pseq
  (*Turn a parallel lazy sequence into a sequential lazy sequence.
  This is really for typing. The elements will still be processed
  in parallel if they were originally added to a parallel sequence.
  Elements added later are processed sequentially.*)
  val sequential_seq_of : Future_Wrapper.future_data -> 'a pseq -> param -> 'a Seq.seq
  (*Turn a parallel lazy sequence into a sequential lazy sequence
  with immediate effect. That means that, other than futures already started,
  further pulls to the sequence will not cause parallel evaluation generally.
  
  Warning: this is not guaranteed if there are inner parallelised parts
  of the sequence, or if you filtered across it.*)
  val truly_sequential_seq_of : Future_Wrapper.future_data -> 'a pseq -> 'a Seq.seq
  (*Turn a sequential lazy sequence into a parallel lazy sequence.
  Elements that were originally in the lazy sequence will continue
  to be processed sequentially, unless you apply any function across the sequence.
  The sequential elements must be processed sequentially,
  but the function applied will be applied in parallel*)
  val of_sequential_seq : 'a Seq.seq -> 'a pseq
  (*Apply a function to a singleton list to produce a sequence
  of singleton lists, which will be turned back into an ordinary
  sequence containing the element in each resulting singleton list*)
  val singleton: ('a list -> 'b list pseq) -> 'a -> 'b pseq
  (*Print a sequence, up to the n elements as supplied*)
  val print: Future_Wrapper.future_data ->
    (int -> 'a -> unit) -> int -> 'a pseq -> param -> unit
  (*Accumulating a function over a sequence; this is lazy.
  Apply the function to an element of the first
  sequence and the sequence produced by "this" to produce a new
  sequence.*)
  val it_right: Future_Wrapper.future_data ->
    ('a * 'b pseq -> 'b pseq) -> 'a pseq * 'b pseq -> param -> 'b pseq
  (*Apply a function f to an element x to produce a sequence.
  Then apply g to every element in this sequence to produce
  a new sequence for each individual result, and then
  append the results*)
  val THEN: ('a -> 'b pseq) * ('b -> 'c pseq) ->
    Future_Wrapper.future_data -> 'a -> param -> 'c pseq
  (*Apply a function f to an element x to produce a sequence
  If the sequence is initially empty, then return g x instead*)
  val ORELSE: ('a -> 'b pseq) * ('a -> 'b pseq) -> 
    'a -> 'b pseq
  (*Apply a function f to an element x to generate a sequence.
  Then apply a function g to the same element to generate a second
  sequence and append the first to the second*)
  val APPEND: ('a -> 'b pseq) * ('a -> 'b pseq) ->
    'a -> 'b pseq
  (*For every function in the list, apply
  that function to each element in the sequence generated so far,
  and append the resulting sequences*)
  val EVERY: Future_Wrapper.future_data ->
    ('a -> 'a pseq) list -> 'a -> param -> 'a pseq
  (*Wait for the first function to return a non-empty sequence
  and return that sequence*)
  val FIRST: ('a -> 'b pseq) list -> 'a -> 'b pseq
  (*Return the sequence generated by f applied to the given element,
  or if that returns an empty sequence, return a singleton sequence
  containing the given element instead*)
  val TRY: ('a -> 'a pseq) -> 'a -> 'a pseq
  (*f is applied to an element x to produce a sequence. If f produces the empty
  sequence, a singleton sequence with x will be returned. Otherwise,
  f is then applied to every element in the produced sequence.
  This recurs.*)
  val REPEAT: Future_Wrapper.future_data ->
    ('a -> 'a pseq) -> 'a -> param -> 'a pseq
  (*Apply the given function to the sequence first.
  Then repeat it across the resulting sequence.*)
  val REPEAT1: Future_Wrapper.future_data ->
    ('a -> 'a pseq) -> 'a -> param -> 'a pseq
  (*If the first integer is less than the second, this returns
  a singleton sequence with the element supplied. Otherwise,
  the function f is applied to the second integer and the element.
  This sequence is appended to a sequence formed recursively
  by reapplying Interval, reducing the second integer by 1, to
  every element produced in the first sequence.*)
  val INTERVAL: Future_Wrapper.future_data ->
    (int -> 'a -> 'a pseq) -> int -> int -> 'a -> param -> 'a pseq
  (*If the function returns the empty sequence, this will
  return the empty sequence. Otherwise it will return the frst
  element only of the resulting sequence*)
  val DETERM: Future_Wrapper.future_data ->
    ('a -> 'b pseq) -> 'a -> param -> 'b pseq
end;

(*
A structure implementing the parallel sequence. This essentially works
by using a normal sequence of elements which are either classified as either:

1. "Waiting" - indicates evaluation of this element has not started.
2. "Started" - indicates a future call has been performed on this element,
so it has either been evaluated or is being evaluated.
3. "Done" - for minor efficiency, remember that a value has already been processed.

When you pull, if the element you are pulling is "Waiting", then a number
of elements are really pulled and futures are created for each of them. They
are then pushed back into the sequence and the first one is actually returned.

The number of elements pre-evaluated is proportional to the number of cores (see
in the code for the exact number) by default. This can be set as a parameter.

This code contains a version of pulling that includes a time check, which
means that the code checks to see if pulling is expensive and will
parallelise accordingly if it is expensive. Otherwise, it will
behave more sequentially. The overhead of system calls for the time
is still significant however, so this parallel sequence should still
be used carefully.
*)
structure Parallel_Seq :> PARALLEL_SEQ =
struct
  structure Future = Future_Wrapper;
  (*The type of an element in the sequence*)
  datatype 'a elem = Waiting of (Future.future_data -> 'a)
                    | Started of ('a Future.future)
                    | Done of 'a (*For speed*)
  (*An empty sequence exception. This is raised when you demand the head or
  tail of an empty sequence*)
  exception Empty_Parallel_Sequence of string
  (*The data type for lazy parallel sequences.
  Elements are either waiting to be started by a future,
  or have already been started and will have their result captured.
  (Exceptions are captured so that exceptions are released
  at the time element when the element is pulled)
  Futures capture their elements automatically!*)
  datatype 'a pseq = Seq of unit -> (('a elem * 'a pseq) option);
  
  (*Recommended defaults for the parameter settings
  Lookahead is optional. If NONE, all elements will be pulled at the same time!
  The second int is the buffer, and generally it should be less than lookahead*)
  type param = (int option * int * int);
  
  (*Add a function for future evaluation to the sequence.
  The format is different to the form of make in the original sequential sequence
  because the functions you pass in are modified. Hence, passing in a function
  of the form (unit -> 'a * 'a seq) is not helpful because the returned 'a cannot
  be modified without evaluating the entire function.*)
  fun make f sq = Seq (fn () => SOME (Waiting f,sq));
  (*Create the empty sequence.*)
  val empty = Seq (fn () => NONE);
  
  (*Pull just the element, without evaluating, and return the rest of
  the sequence. Thisis considered "cheap" since the value in the element
  is not computed by this.*)
  fun pull_elem (Seq sq) = sq ()
  
  (*This is needed to prevent lazy
  evaluation when attempting to call futures further
  down the list.*)
  fun form elem sq = Seq (fn () => SOME (elem,sq))
  
  (*This will look ahead a number of elements in the sequence
  and start them off as futures.
  Note: last parameter "diff" is to decide whether or not the whole sequence should be pulled
  at the same time.*)
  fun pull_ahead _ sq 0 _ _ = sq (*Looked at enough already*)
    | pull_ahead fd sq n buff diff = case (pull_elem sq) of
        NONE => empty (*The sequence is empty anyway, so return it*)
      | SOME (Waiting f,sq2) => (*This needs to begin evaluation!*)
          form (Started (
          Future.future_group_data fd f)) (
          pull_ahead fd sq2 (n-diff) buff diff)
      | SOME (Started f,sq2) => (
      if (buff=0) then sq else (Seq (fn() => (SOME (Started f,pull_ahead fd sq2 n (buff-1) diff)))))
      (*Already started. Only look ahead again when
      the first chunk is evaluated. This creates bursts of processing, which for a program
      (which is constantly running) is the preferable way to make the most of the available
      cores. Also, this prevents long failed look aheads.*)
      | SOME (Done f,sq2) => (
      if (buff=0) then sq else (Seq (fn() => (SOME (Done f,pull_ahead fd sq2 n (buff-1) diff)))));
  
  (*Pull a single element from the sequence.
  This will evaluate the element pulled.*)
  fun pull_one fd sq = case (pull_elem sq) of
      NONE => NONE (*The sequence is empty so this fails*)
    | (SOME (Waiting f,sq2)) => SOME (f fd,sq2) (*Force evaluation now.
    Generally the Waiting case will not come up, since pull_ahead will have been called
    before it.*)
    | (SOME (Started v,sq2)) => SOME (Future.join v,sq2) (*Join on the existing
    future, which has hopefully already finished.*)
    | (SOME (Done v,sq2)) => SOME (v,sq2);
  
  (*Get the next element from the sequence. Returns NONE
  if a next element does not exist. This requires parameters to determine
  the behaviour of the pull operation.
  Pulling from the same sequence twice (to return exactly the same element)
  can be especially painful since further values in the sequence may be
  re-evaluated too, so avoid this!!*)
  fun pull fd sq (look_ahead,buffer,time_boundary) =
    let
      (*Time the first pull*)
      val sw = Stopwatch.start (Stopwatch.new);
      val res = pull_one fd sq;
      val t = Time.toMicroseconds (fst (Stopwatch.stop sw));
    in
      case res of
        NONE => NONE
      | SOME (v,sq2) => if (t>time_boundary) then (*Decide whether or not to parallelise*)
        ((*The tasks seem to be taking long enough, so parallelise!*)
        case look_ahead of
            NONE => SOME (v,pull_ahead fd sq2 1 buffer 0) (*0 means it will pull forever*)
          | (SOME la) => SOME (v,pull_ahead fd sq2 la buffer 1)) (*1 ensures it will only pull la times at most*)
        else
          res (*We can ignore if the request to pull all elements was made, since it would not be beneficial to try anyway*)
    end
  
  (*Attach an element to the front of the sequence*)
  fun cons v sq = Seq (fn () => SOME (Done v,sq))
  
  (*Create a sequence with a single element*)
  fun single v = cons v empty;
  
  (*Return the first element of the sequence.
  Raises empty sequence exception if there is no head
  Calling this on a sequence more than once may force evaluation of the head
  multiple times (dependent on whether or not it has begun evaluation).*)
  fun hd fd sq param = case (pull fd sq param) of
      NONE => raise Empty_Parallel_Sequence "Cannot return the head of the empty sequence."
    | SOME (v,_) => v
  
  (*Return the tail of the sequence
  Raises empty sequence exception if there is no tail
  As with hd, calling this multiple times may incur a penalty.*)
  fun tl sq = case (pull_elem sq) of
      NONE => raise Empty_Parallel_Sequence "Cannot return the tail of the empty sequence."
    | SOME (_,sq2) => sq2
  
  (*Attempt to apply a function to an element. If it succeeds, it will return
  a singleton sequence containing the answer. If it fails an empty sequence
  will be returned*)
  fun try f x =
    (case Basics.try f x of
      SOME y => single y
    | NONE => empty);
  
  (*Return a list of the first n elements (evaluated) paired with the rest*)
  fun chop fd n xq param =
    if n <= (0 : int) then ([], xq)
    else
      (case pull fd xq param of
        NONE => ([], xq)
      | SOME (x, xq') => apfst (Basics.cons x) (chop fd (n - 1) xq' param));
  
  (*Take the first n elements of the sequence. (Forget the rest)*)
  fun take n (Seq sq) =
    if n <= (0 : int) then empty
    else 
    Seq (fn () => case (sq()) of
        NONE => NONE
      | SOME (v,sq2) => SOME (v, take (n-1) sq2))
  
  (*Convert the sequence to a list of values.*)
  fun list_of fd xq param =
    case pull fd xq param of
      NONE => []
    | SOME (x, xq') => x :: list_of fd xq' param;
  
  (*Convert a list back into a sequence.*)
  fun of_list xs = fold_rev cons xs empty;
  
  (*Join two sequences together (first appears in front)*)
  fun append xq yq =
    let
      fun copy sq = Seq (fn () =>
        case (pull_elem sq) of
          NONE => pull_elem yq
        | SOME (v,sq2) => (SOME (v,copy sq2)))
    in copy xq end;
  
  (*Map a function over a sequence. This is done lazily like all of the functions*)
  fun map f xq = Seq (fn () =>
    case (pull_elem xq) of
      NONE => NONE (*Nothing to do with the empty sequence*)
      (*Waiting: create a new waiting which incorporates the work of the map function*)
    | SOME (Waiting g,sq2) => SOME (Waiting (fn fd => (f (g fd))),map f sq2)
      (*Started: don't waste the current future that is processing. Instead, create a new
      Waiting element which will join on this future when pulled.*)
    | SOME (Started g,sq2) => SOME (Waiting (fn _ => (f (Future.join g))),map f sq2)
      (*Done: just use the value - even cheaper than started!*)
    | SOME (Done v,sq2) => SOME (Waiting (fn _ => (f v)),map f sq2));
    
  (*Apply a function to the first sequence, and append the result
  to the next sequence*)
  fun mapp f xq yq = append (map f xq) yq;
  
  (*Interleave two sequences together, starting with the first
  If one runs out, the other is appended to the end.*)
  fun interleave (xq, yq) =
    Seq (fn () => case (pull_elem xq) of
        NONE => pull_elem yq (*Return the remaining sequence yq*)
      | SOME (v,sq2) => SOME (v,interleave (yq,sq2)))
        (*Swap sequences and return the element that was found*)
  
  (*An auxiliary function for filter. Note that this is forced to evaluate
  the filter condition!*)
  fun filter' fd pred xq param =
    let
      fun copy sq = Seq (fn () =>
        case (pull fd sq param) of
            NONE => NONE
          | SOME (v,sq2) => if (pred v) then
              SOME (Done v,copy sq2)
            else
              pull_elem (copy sq2))
    in copy xq end;
  
  (*Filter the values in a sequence.
  
  Implementation detail:
  
  The filter function is simplified by turning the filter function into
  a function which returns SOME x if it is true for x, and NONE otherwise.
  A new filter removes all of the NONE values and then a final map converts
  SOME x to x. This allows the possibly heavy duty predicate function to be processed
  in parallel, and drastically simplifes the filter function so you won't notice
  that it forces evaluation on pulling up to the next "true" element
  *)
  fun filter fd pred xq param =
    map (fn x => the x) (*Convert SOME x => x*)
      (filter' fd (fn x => not (is_none x)) (*Filter out the nones*)
      (map (fn x => if (pred x) then SOME x else NONE) xq) param); (*The map function
      based on the filter function.*)
  
  (*Flatten (concatenate) a sequence of sequences. This too maintains
  laziness across the sequence of sequences and in each individually sequence
  
  Note: when the sequence of sequences experiences a "pull_elem"
  it does not force parallel pulling on the sequence
  it pulls out. When a pull then occurs on this sequence,
  it will be parallelised. This is for a number of reasons:
  
  1. pull_elem is supposed to be as cheap as possible for the parallel
  processing to be as useful as possible.
  2. If the sequence is very short, then pulling a single element
  like this will be cheaper. If it is very long, the next
  pull to the flattened sequence will cause parallelised pulling anyway.
  3. Avoiding parallel pulling here increases (~probably) the coarseness of the concurrency,
  which for now is considered a good thing.
  *)
  fun flat fd sqq param = Seq (fn () =>
    case (pull fd sqq param) of (*I'll need the next sequence in the sequence of sequences.
    This is likely to be cheap, so it just pulls one element*)
        NONE => NONE (*Must have finished flattening*)
        (*I will need to pull the element out of this sequence, and append it to whatever
        the result of flatten is.*)
      | SOME (xq,xqq) => pull_elem (append xq (flat fd xqq param)))
  
  (*Maps a function which produces a sequence
  for every element in the original sequence. It automatically
  appends all of the results*)
  fun maps fd f sq = flat fd (map f sq)
  
  (*Maps a filter function over the sequence. If the filter returns
  none, the element is dropped. If the filter returns SOME b then
  b is attached to the sequence.
  *)
  fun map_filter fd f = maps fd (fn x => (case f x of NONE => empty | SOME y => single y));
  
  (*Apply a function over the sequence. The second element
  is paired with this function for every single value*)
  fun lift f xq y = map (fn x => f x y) xq;
  
  (*Similar to lift but appends the results of the function*)
  fun lifts fd f xq y = maps fd (fn x => f x y) xq;
  
  (*Creates a singleton sequence element with the given value
  and future data*)
  fun succeed x = single x;
  
  (*Returns an empty sequence regardless of the element*)
  fun fail _ = empty;
  
  (*Turn a parallel lazy sequence into a sequential lazy sequence
  with immediate affect. That means that, other than futures already started,
  further pulls to the sequence will not cause parallel evaluation.*)
  fun truly_sequential_seq_of fd sq = let
    fun seq_of sq = Seq.make (fn () =>
      case (pull_elem sq) of
          NONE => NONE
        | SOME (e,sq2) => SOME (e,seq_of sq2)) in
    Seq.map (fn
      (Waiting g) => (g fd)
    | (Started g) => (Future.join g)
    | (Done v) => v
    ) (seq_of sq) end
  
  (*Turn a parallel lazy sequence into a sequential lazy
  sequence. This is still lazy*)
  fun sequential_seq_of fd sq param = Seq.make (fn() =>
      case (pull fd sq param) of
          NONE => NONE
        | SOME (e,sq2) => SOME (e,sequential_seq_of fd sq2 param))
  
  (*Turn a sequential lazy sequence into a parallel lazy sequence.
  Elements that were originally in the lazy sequence will continue
  to be processed sequentially, unless you apply any function across the sequence.
  The sequential elements must be processed sequentially,
  but the function applied will be applied in parallel*)
  fun of_sequential_seq sq = Seq (fn () =>
    case (Seq.pull sq) of
        NONE => NONE
      | SOME (v,sq2) =>
        SOME (Done v,of_sequential_seq sq2));
  
  (*Apply a function to a singleton list to produce a sequence
  of singleton lists, which will be turned back into an ordinary
  sequence containing the element in each resulting singleton list*)
  fun singleton f x = f [x] |> map (fn [y] => y | _ => raise Empty);
  
  (*Print a sequence, up to the n elements as supplied*)
  fun print fd print_elem count xq param =
    let
      fun prnt (k: int) xq =
        if k > count then ()
        else
          (case pull fd xq param of
            NONE => ()
          | SOME (x, xq') => (print_elem k x; writeln ""; prnt (k + 1) xq'));
    in prnt 1 xq end;
  
  (*Accumulating a function over a sequence; this is lazy.
  Apply the function to an element of the first
  sequence and the sequence produced by "this" to produce a new
  sequence.*)
  fun it_right fd f (xq, yq) param =
    let
      fun its s = Seq (fn () => case (pull fd s param) of
          NONE => pull_elem yq
        | SOME (v,sq2) => pull_elem (f (v,its sq2)))
    in its xq end;
  
  (*Apply a function f to an element x to produce a sequence.
  Then apply g to every element in this sequence to produce
  a new sequence for each individual result, and then
  append the results*)
  fun op THEN (f, g) fd x param = maps fd g (f x) param;
  
  (*Apply a function f to an element x to produce a sequence
  If the sequence is initially empty, then return g x instead*)
  fun op ORELSE (f, g) x = Seq (fn () => case (pull_elem (f x)) of
      NONE => pull_elem (g x)
    | some => some);
  (*Apply a function f to an element x to generate a sequence.
  Then apply a function g to the same element to generate a second
  sequence and append the first to the second*)
  fun op APPEND (f, g) x = append
    (Seq (fn () => pull_elem (f x)))
    (Seq (fn () => pull_elem (g x)));
  
  (*For every function in the list, apply
  that function to each element in the sequence generated so far,
  and append the resulting sequences*)
  fun EVERY fd fs x param = fold_rev (fn a => fn b => fn x => THEN (a,b) fd x param) fs succeed x;
  
  (*Wait for the first function to return a non-empty sequence
  and return that sequence*)
  fun FIRST fs = fold_rev (curry op ORELSE) fs fail;
  
  (*Return the sequence generated by f applied to the given element,
  or if that returns an empty sequence, return a singleton sequence
  containing the given element instead*)
  fun TRY f = ORELSE (f, succeed);
    
  (*f is applied to an element x to produce a sequence. If f produces the empty
  sequence, a singleton sequence with x will be returned. Otherwise,
  f is then applied to every element in the produced sequence.
  This recurs.*)
  fun REPEAT fd f x param =
    let
      fun rep qs x = Seq (fn() =>
        (case pull fd (f x) param of
          NONE => SOME (Done x, repq qs)
        | SOME (x', q) => pull_elem (rep (q :: qs) x')))
      and repq [] = Seq (fn () => NONE)
        | repq (q :: qs) = Seq (fn() =>
            (case pull fd q param of
              NONE => (pull_elem (repq qs))
            | SOME (x, q) => pull_elem (rep (q :: qs) x)));
    in rep [] x end;
  
  (*Apply the given function to the sequence first.
  Then repeat it across the resulting sequence.*)
  fun REPEAT1 fd f x param = THEN (f, fn y => REPEAT fd f y param) fd x param;
  
  (*If the first integer is less than the second, this returns
  a singleton sequence with the element supplied. Otherwise,
  the function f is applied to the second integer and the element.
  This sequence is appended to a sequence formed recursively
  by reapplying Interval, reducing the second integer by 1, to
  every element produced in the first sequence.*)
  fun INTERVAL fd f (i: int) j x param =
    if i > j then single x
    else op THEN (f j, fn y => INTERVAL fd f i (j - 1) y param) fd x param;
  
  (*If the fnction returns the empty sequence, this will
  return the empty sequence. Otherwise it will return the frst
  element only of the resulting sequence*)
  fun DETERM fd f x param = Seq (fn () => pull_elem (
    (case pull fd (f x) param of
      NONE => empty
    | SOME (x', _) => cons x' empty)));
end;

(*This signature is extremely similar to that of PARALLEL_SEQ so please
refer to that for details on what the functions do. The difference
with this is that the signature requires passing of future data
to all functions worked on within the sequence. This is considered better
management, since if you ever cancelled the sequence, you'd want any futures
it triggered to be cancelled too. This signature makes you "think" about this
(It has no additional power compared to the original signature for parallel
lazy sequences).
It also cuts out seemingly unnecessary future data and other parameter requirements.

IMPORTANT: when a function involves two sequences, the parameters of the first are given to the
resulting sequence. In flattening, the outer parameters are kept*)
signature SAFE_PARALLEL_SEQ =
sig
  exception Empty_Parallel_Sequence of string
  type 'a PSeq
  (*The type of parameters passed to the lazy sequence*)
  type param = (int option * int * int)
  (*These functions allow the setting of parameters in the parallel lazy sequence,
  which govern how the elements are pulled from the sequence! Note that
  when sequences with different parameters are combined, you should consider the behaviour
  unpredictable so please avoid this! (Or you can try to keep in mind the description
  above this).*)
  
  (*Set how many elements should be pulled by the sequence in parallel.
  If NONE is supplied, then the sequence will no longer behave in a lazy way.
  However, do not consider it guaranteed that all elements will be pulled. If elements
  appear too small to be worth pulling, a single element only may be pulled sequentialy.*)
  val set_look_ahead : 'a PSeq -> int option -> 'a PSeq
  (*Set how many elements the sequence checks ahead of its current pull before it
  decides to pull further elements. This can help to keep the future's library loaded
  for longer, while keeping the sequence lazy*)
  val set_buffer : 'a PSeq -> int -> 'a PSeq
  (*Set how long a function must take in order to trigger a parallel
  computation. The integer given is in microseconds*)
  val set_time_boundary : 'a PSeq -> int -> 'a PSeq
  (*Return the parameter values*)
  val get_look_ahead : 'a PSeq -> int option
  val get_buffer : 'a PSeq -> int
  val get_time_boundary : 'a PSeq -> int
  
  (*An exception raised when parameters are set poorly*)
  exception Parallel_Lazy_Sequence_Params of string
  
  (*Return the future data in use by this sequence*)
  val get_future_data : 'a PSeq -> Future_Wrapper.future_data
  (*Set the future data to be used by this sequence (generally avoid doing this
  - especially mid computation!)*)
  val set_future_data : Future_Wrapper.future_data -> 'a PSeq -> 'a PSeq
  val make : (Future_Wrapper.future_data -> 'a) -> 'a PSeq -> 'a PSeq
  (*Create a new empty sequence, which will begin by using the passed in future data*)
  val empty : Future_Wrapper.future_data -> 'a PSeq
  val pull : 'a PSeq -> ('a * 'a PSeq) option
  val cons : 'a -> 'a PSeq -> 'a PSeq
  val single : Future_Wrapper.future_data -> 'a -> 'a PSeq
  val try : Future_Wrapper.future_data -> (Future_Wrapper.future_data -> 'a -> 'b) -> 'a -> 'b PSeq
  val hd : 'a PSeq -> 'a
  val tl : 'a PSeq -> 'a PSeq
  val chop : int -> 'a PSeq -> (('a list) * ('a PSeq))
  val take : int -> 'a PSeq -> 'a PSeq
  val list_of : 'a PSeq -> 'a list
  val of_list : Future_Wrapper.future_data -> 'a list -> 'a PSeq
  val append : 'a PSeq -> 'a PSeq -> 'a PSeq
  val map : (Future_Wrapper.future_data -> 'a -> 'b) -> 'a PSeq -> 'b PSeq
  val mapp : (Future_Wrapper.future_data -> 'a -> 'b) -> 'a PSeq -> 'b PSeq -> 'b PSeq
  val interleave: 'a PSeq * 'a PSeq -> 'a PSeq
  val filter : (Future_Wrapper.future_data -> 'a -> bool) -> 'a PSeq -> 'a PSeq
  val flat : 'a PSeq PSeq -> 'a PSeq
  val maps : (Future_Wrapper.future_data -> 'a -> 'b PSeq) -> 'a PSeq -> 'b PSeq
  val map_filter : (Future_Wrapper.future_data -> 'a -> 'b option) -> 'a PSeq -> 'b PSeq
  val lift : (Future_Wrapper.future_data -> 'a -> 'b -> 'c) -> 'a PSeq -> 'b -> 'c PSeq
  val lifts : (Future_Wrapper.future_data -> 'a -> 'b -> 'c PSeq) -> 'a PSeq -> 'b -> 'c PSeq
  val succeed : Future_Wrapper.future_data -> 'a -> 'a PSeq
  val fail : Future_Wrapper.future_data -> 'a -> 'b PSeq
  val sequential_seq_of : 'a PSeq -> 'a Seq.seq
  val truly_sequential_seq_of : 'a PSeq -> 'a Seq.seq
  val of_sequential_seq : Future_Wrapper.future_data -> 'a Seq.seq -> 'a PSeq
  val singleton: Future_Wrapper.future_data -> (Future_Wrapper.future_data -> 'a list -> 'b list PSeq) -> 'a -> 'b PSeq
  val print: (Future_Wrapper.future_data -> int -> 'a -> unit) -> int -> 'a PSeq -> unit
  val it_right: (Future_Wrapper.future_data -> 'a * 'b PSeq -> 'b PSeq) -> 'a PSeq * 'b PSeq -> 'b PSeq
  val THEN: (Future_Wrapper.future_data -> 'a -> 'b PSeq) * (Future_Wrapper.future_data -> 'b -> 'c PSeq) -> Future_Wrapper.future_data -> 'a -> param -> 'c PSeq
  val ORELSE: (Future_Wrapper.future_data -> 'a -> 'b PSeq) * (Future_Wrapper.future_data -> 'a -> 'b PSeq) -> Future_Wrapper.future_data -> 'a -> param -> 'b PSeq
  val APPEND: (Future_Wrapper.future_data -> 'a -> 'b PSeq) * (Future_Wrapper.future_data -> 'a -> 'b PSeq) -> Future_Wrapper.future_data -> 'a -> param -> 'b PSeq
  val EVERY: Future_Wrapper.future_data -> (Future_Wrapper.future_data -> 'a -> 'a PSeq) list -> 'a -> param -> 'a PSeq
  val FIRST: Future_Wrapper.future_data -> (Future_Wrapper.future_data -> 'a -> 'b PSeq) list -> 'a -> param -> 'b PSeq
  val TRY: Future_Wrapper.future_data -> (Future_Wrapper.future_data -> 'a -> 'a PSeq) -> 'a -> param -> 'a PSeq
  val REPEAT: Future_Wrapper.future_data -> (Future_Wrapper.future_data -> 'a -> 'a PSeq) -> 'a -> param -> 'a PSeq
  val REPEAT1: Future_Wrapper.future_data -> (Future_Wrapper.future_data -> 'a -> 'a PSeq) -> 'a -> param -> 'a PSeq
  val INTERVAL: Future_Wrapper.future_data -> (Future_Wrapper.future_data -> int -> 'a -> 'a PSeq) -> int -> int -> 'a -> param -> 'a PSeq
  val DETERM: Future_Wrapper.future_data -> (Future_Wrapper.future_data -> 'a -> 'b PSeq) -> 'a -> param -> 'b PSeq
end;

(*The safer way to use the parallel sequence, which manages the future data considerably for you.
It does force you to supply functions expecting future data, but this can of course be
escaped with fn fd => f if you know these functions will not need to create their own futures.

The implementations in here are pretty obvious so do not contain the comments
like Parallel_Seq. Essentially, it will pass the parameters around when needed

Warning: an adjustment to the functions passed in to EVERY and FIRST means
a simple map must be computed over the list, so it is not entirely lazy.
A direct implementation of what is below might be able to avoid this, but it is unlikely
the mapping will take any significant amount of time.*)
structure Safe_Parallel_Seq :> SAFE_PARALLEL_SEQ =
struct
  (*The number of cores available with this machine*)
  fun num_cores() = Multithreading.max_threads_value();
  (*This is the number of elements further down the sequence that will attempt
  to be evaluated after a pull*)
  fun recommended_look_ahead() = SOME ((num_cores()) * 10);
  (*How far I am willing to look ahead to find the first unevaluated element*)
  fun recommended_buffer() = num_cores();
  (*If pulling takes longer than this to compute, the next
  series of values will be pulled in parallel.
  This reduces the severity of the worst case.*)
  (*Time in microseconds*)
  val recommended_time_boundary = 400;
  
  exception Empty_Parallel_Sequence of string
  (*An exception raised when parameters are set poorly*)
  exception Parallel_Lazy_Sequence_Params of string
  structure Future = Future_Wrapper;
  structure Par_Seq = Parallel_Seq;
  (*Parameters are tagged onto the end*)
  type param = (int option * int * int)
  type 'a PSeq = ('a Par_Seq.pseq) * Future.future_data * param
  (*Return the future data in use by this sequence*)
  fun get_future_data (_,fd,_) = fd
  (*Set the future data to be used by this sequence (generally avoid doing this)*)
  fun set_future_data fd (sq,_,param) = (sq,fd,param)
  
  (*Set how many elements should be pulled by the sequence in parallel.
  If NONE is supplied, then the sequence will no longer behave in a lazy way.
  However, do not consider it guaranteed that all elements will be pulled. If elements
  appear too small to be worth pulling, a single element only may be pulled sequentialy.*)
  fun set_look_ahead (sq,fd,(_,buffer,time_boundary)) la = case la of
      NONE => (sq,fd,(NONE,buffer,time_boundary))
    | (SOME v) => if (v<=0) then raise Parallel_Lazy_Sequence_Params "Look ahead must strictly be positive (>=1)"
      else (sq,fd,(la,buffer,time_boundary))
  (*Set how many elements the sequence checks ahead of its current pull before it
  decides to pull further elements. This can help to keep the future's library loaded
  for longer, while keeping the sequence more lazy*)
  fun set_buffer (sq,fd,(la,_,time_boundary)) v = if (v<0) then
    raise Parallel_Lazy_Sequence_Params "Buffer must be non-negative (>=0)"
    else (sq,fd,(la,v,time_boundary))
  (*Set how long a function must take in order to trigger a parallel
  computation. The integer given is in microseconds*)
  fun set_time_boundary (sq,fd,(la,buffer,_)) time_boundary = if (time_boundary<=0) then
    raise Parallel_Lazy_Sequence_Params "Time boundary must be positive (>0)"
    else (sq,fd,(la,buffer,time_boundary))
    
  (*Return the parameter values*)
  fun get_look_ahead (_,_,(la,_,_)) = la
  fun get_buffer (_,_,(_,buffer,_)) = buffer
  fun get_time_boundary (_,_,(_,_,time_boundary)) = time_boundary
  fun fst' (a,_,_) = a
  
  (*All the functions below make use of the sequence passed in. A few complications
  do arise. In particular, if a function requires a function which returns a sequence,
  then this returned sequence is not of type Seq.seq. Hence, the function has to
  be modified. The extreme case of this is in FIRST and EVERY, where a list of functions
  have to be adjusted.*)
  fun make f (sq,fd,param) = (Par_Seq.make f sq,fd,param)
  fun empty fd = (Par_Seq.empty,fd,(recommended_look_ahead(),recommended_buffer(),recommended_time_boundary))
  fun pull (sq,fd,param) = case (Par_Seq.pull fd sq param) of
      NONE => NONE
    | SOME (v,sq2) => SOME (v,(sq2,fd,param))
  fun cons v (sq,fd,param) = (Par_Seq.cons v sq,fd,param)
  fun single fd v = (Par_Seq.single v,fd,(recommended_look_ahead(),recommended_buffer(),recommended_time_boundary))
  fun try fd f v = (Par_Seq.try (f fd) v,fd,(recommended_look_ahead(),recommended_buffer(),recommended_time_boundary))
  fun hd (sq,fd,param) = Par_Seq.hd fd sq param
  fun tl (sq,fd,param) = (Par_Seq.tl sq,fd,param)
  fun chop n (sq,fd,param) = let val (ls,sq2) = Par_Seq.chop fd n sq param in (ls,(sq2,fd,param)) end
  fun take n (sq,fd,param) = (Par_Seq.take n sq,fd,param)
  fun list_of (sq,fd,param) = Par_Seq.list_of fd sq param
  fun of_list fd ls = (Par_Seq.of_list ls,fd,(recommended_look_ahead(),recommended_buffer(),recommended_time_boundary))
  fun append (sq,fd,param) (sq2,_,_) = (Par_Seq.append sq sq2,fd,param)
  fun map f (sq,fd,param) = (Par_Seq.map (f fd) sq,fd,param)
  fun mapp f (sq,fd,param) (sq2,_,_) = (Par_Seq.mapp (f fd) sq sq2,fd,param)
  fun interleave  ((sq,fd,param),(sq2,_,_)) = (Par_Seq.interleave (sq,sq2),fd,param)
  fun filter f (sq,fd,param) = (Par_Seq.filter fd (f fd) sq param,fd,param)
  fun flat (sqq,fd,param) = (Par_Seq.flat fd (Par_Seq.map fst' sqq) param,fd,param)
  fun maps f (sq,fd,param) = (Par_Seq.maps fd (fn a => fst' (f fd a)) sq param,fd,param)
  fun map_filter f (sq,fd,param) = (Par_Seq.map_filter fd (f fd) sq param,fd,param)
  fun lift f (sq,fd,param) b = (Par_Seq.lift (f fd) sq b,fd,param)
  fun lifts f (sq,fd,param) b = (Par_Seq.lifts fd (fn a => fn b' => fst' (f fd a b')) sq b param,fd,param)
  fun succeed fd v = (Par_Seq.succeed v,fd,(recommended_look_ahead(),recommended_buffer(),recommended_time_boundary))
  fun fail fd v = (Par_Seq.fail v,fd,(recommended_look_ahead(),recommended_buffer(),recommended_time_boundary))
  fun sequential_seq_of (sq,fd,param) = Par_Seq.sequential_seq_of fd sq param
  fun truly_sequential_seq_of (sq,fd,_) =  Par_Seq.truly_sequential_seq_of fd sq
  fun of_sequential_seq fd sq = (Par_Seq.of_sequential_seq sq,fd,(recommended_look_ahead(),recommended_buffer(),recommended_time_boundary))
  fun singleton fd f v = (Par_Seq.singleton (fn a => fst' (f fd a)) v,fd,(recommended_look_ahead(),recommended_buffer(),recommended_time_boundary))
  fun print f n (sq,fd,param) = Par_Seq.print fd (f fd) n sq param
  fun it_right f ((sq,fd,param),(sq2,_,_)) = (Par_Seq.it_right fd (fn (a,b) => fst' (f fd (a,(b,fd,param)))) (sq,sq2) param,fd,param)
  fun THEN (f,g) fd v param = (Par_Seq.THEN (fn a => fst' (f fd a),fn b => fst' (g fd b)) fd v param,fd,param)
  fun ORELSE (f,g) fd v param= (Par_Seq.ORELSE (fn a => fst' (f fd a),fn b => fst' (g fd b)) v,fd,param)
  fun APPEND (f,g) fd v param = (Par_Seq.APPEND (fn a => fst' (f fd a),fn b => fst' (g fd b)) v,fd,param)
  fun EVERY fd fs v param = (Par_Seq.EVERY fd (List.map (fn f => (fn a => fst' (f fd a))) fs) v param,fd,param)
  fun FIRST fd fs v param = (Par_Seq.FIRST (List.map (fn f => (fn a => fst' (f fd a))) fs) v,fd,param)
  fun TRY fd f v param = (Par_Seq.TRY (fn a => fst' (f fd a)) v,fd,param)
  fun REPEAT fd f v param = (Par_Seq.REPEAT fd (fn a => fst' (f fd a)) v param,fd,param)
  fun REPEAT1 fd f v param = (Par_Seq.REPEAT1 fd (fn a => fst' (f fd a)) v param,fd,param)
  fun INTERVAL fd f i j v param = (Par_Seq.INTERVAL fd (fn n => fn a => fst' (f fd n a)) i j v param,fd,param)
  fun DETERM fd f v param = (Par_Seq.DETERM fd (fn a => fst' (f fd a)) v param,fd,param)
end;

