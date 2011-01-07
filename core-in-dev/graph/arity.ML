(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation arity of edges into or out of a vertex. *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

(* The arity of a vertex consists of 3 incidences: 
   edges coming in, 
   edges going out, 
   and undirected edges that are connected to this vertex. 
   
   Note: self loops are assumed to create 2 incidences to a vertex. Only
   isolated vertices are incidence 'zero'. 
*)

signature ARITY = 
sig
  type T
  val zero : T
  val add : T -> T -> T

  val add_1_in : T -> T
  val add_1_out : T -> T
  val add_1_undir : T -> T

  val subtract : T -> T -> T
  val divide : T -> T -> T
  val mult : T -> T -> T
  val div_by_int : int -> T -> T
  val mult_by_int : int -> T -> T

  val max_incidence : T -> int
  val min_incidence : T -> int
  val min_nonzero_incidence : T -> int option

  val has_negative_incidence : T -> bool

  val get_in : T -> int
  val get_out : T -> int
  val get_undir : T -> int

  val update_in : (int -> int) -> T -> T
  val update_out : (int -> int) -> T -> T
  val update_undir : (int -> int) -> T -> T

  val rep_of : T -> {no_in : int, (* number of incoming edges *)
             no_out : int, (* number of outgoig edges *)
             no_undir : int (* number of undirected, connected edges *) }

end;

(* *)
structure Arity = 
struct
  
  (* not every *)
  datatype T = Arity of 
            {no_in : int, (* number of incoming edges *)
             no_out : int, (* number of outgoig edges *)
             no_undir : int (* number of undirected, connected edges *) }
  
  (* zero is the arity of an isolated (unconnected) vertex *)
  val zero = Arity {no_in = 0, no_out = 0, no_undir = 0 };

  fun is_zero (Arity rep) = 
      (#no_in rep = 0 andalso #no_out rep = 0 andalso #no_undir rep = 0);

  (* *)
  fun get_in (Arity rep) = #no_in rep;
  fun get_out (Arity rep) = #no_out rep;
  fun get_undir (Arity rep) = #no_undir rep;

  fun update_in f (Arity rep) = 
      Arity { no_in = f(#no_in rep), no_out = #no_out rep, 
              no_undir = #no_undir rep };
  fun update_out f (Arity rep) = 
      Arity { no_in = #no_in rep, no_out = f(#no_out rep), 
              no_undir = #no_undir rep };
  fun update_undir f (Arity rep) = 
      Arity { no_in = f(#no_in rep), no_out = #no_out rep, 
              no_undir = f(#no_undir rep) };

  (* *)  
  fun add (Arity rep1) (Arity rep2) = 
      Arity { no_in = (#no_in rep1) + (#no_in rep2), 
        no_out = (#no_out rep1) + (#no_out rep2),
        no_undir = (#no_undir rep1) + (#no_undir rep2) };
  
  (* *)  
  val add_1_in = update_in (fn i => i + 1);
  val add_1_out = update_out (fn i => i + 1);
  val add_1_undir = update_undir (fn i => i + 1);
  
  (* *)
  fun subtract (Arity rep1) (Arity rep2) = 
      Arity { no_in = (#no_in rep1) - (#no_in rep2), 
        no_out = (#no_out rep1) - (#no_out rep2),
        no_undir = (#no_undir rep1) - (#no_undir rep2) };
  
  fun has_negative_incidence (Arity rep) = 
      (#no_in rep) < 0 orelse (#no_out rep) < 0 orelse (#no_undir rep) < 0;
  
  fun div_by_int i (Arity rep) = 
      Arity { no_in = (#no_in rep) div i, 
        no_out = (#no_out rep) div i,
        no_undir = (#no_undir rep) div i };
  
  fun mult_by_int i (Arity rep)= 
      Arity { no_in = (#no_in rep) * i, 
        no_out = (#no_out rep) * i,
        no_undir = (#no_undir rep) * i };
  
  fun non_zero_div x y = if y <> 0 then x div y else 0; 
  
  fun divide (Arity rep1) (Arity rep2) = (* rep1 / rep2 *)
      Arity { no_in = non_zero_div (#no_in rep1) (#no_in rep2),
        no_out = non_zero_div (#no_out rep1) (#no_out rep2),
        no_undir = non_zero_div (#no_undir rep1) (#no_undir rep2) };
  
  fun max_incidence (Arity rep) = 
      Int.max(Int.max (#no_in rep, #no_out rep), #no_undir rep);

  fun min_incidence (Arity rep) = 
      Int.min(#no_undir rep,Int.min(#no_in rep,#no_out rep));
  
  fun min_nonzero_incidence (Arity rep) = 
      fold (fn x => (fn NONE => SOME x 
                      | SOME y => (if y <> 0 then SOME (Int.min(x,y)) 
                                   else SOME y)))
         [#no_in rep, #no_out rep, #no_undir rep]
         NONE;


end; (* *)
