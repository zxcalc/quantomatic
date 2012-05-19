(* Util struct used to handle metric types:
  * Thay are int tuples of variable length. 
  * (we go up to 6 *)

signature METRIC_TYPE =
  sig

    structure Graph : BANG_GRAPH;

    type base_type = int; (* In case we want to switch to real numbers... *)
    type foreign_type = base_type list; (* Vectors as lists :( *)

    datatype metric_type = Tup1 of base_type |
                       Tup2 of (base_type * base_type) |
                       Tup3 of (base_type * base_type * base_type) |
                       Tup4 of (base_type * base_type * base_type * base_type) |
                       Tup5 of (base_type * base_type * base_type * base_type *
                                  base_type) |
                       Tup6 of (base_type * base_type * base_type * base_type *
                                  base_type * base_type) |
                       Malformed;

    datatype metric_rec = MR of { desc      : string, 
                                  compute   : (Graph.T -> foreign_type option), 
                                  ord_graph : ((Graph.T * Graph.T) -> order option),
                                  ord       : (((foreign_type) * (foreign_type))
                                  -> order option),
                                  bottom    : foreign_type};

    val to_foreign_type   : metric_type option -> foreign_type option;
    val to_foreign_type_safe : metric_type -> foreign_type;
    val from_foreign_type : foreign_type -> metric_type; 

    (* Getters for metrics records *)
    val get_mr_desc : metric_rec -> string;
    val get_mr_compute : metric_rec -> (Graph.T -> foreign_type option);
    val get_mr_ord : metric_rec -> 
      (((foreign_type) * (foreign_type)) -> order option);
    val get_mr_ord_graph : metric_rec -> ((Graph.T * Graph.T) -> order option);
    val get_mr_bottom : metric_rec -> foreign_type;
  end;

functor MetricType(
  structure Graph : BANG_GRAPH
) : METRIC_TYPE =
  struct
    structure Graph = Graph;
    
    type base_type = int;
    type foreign_type = base_type list;

    datatype metric_type = 
                       Tup1 of base_type |
                       Tup2 of (base_type * base_type) |
                       Tup3 of (base_type * base_type * base_type) |
                       Tup4 of (base_type * base_type * base_type * base_type) |
                       Tup5 of (base_type * base_type * base_type * base_type *
                                  base_type) |
                       Tup6 of (base_type * base_type * base_type * base_type *
                                  base_type * base_type) |
                       Malformed;

    datatype metric_rec = MR of { desc      : string, 
                                  compute   : (Graph.T -> foreign_type option), 
                                  ord_graph : ((Graph.T * Graph.T) -> order option),
                                  ord        : (((foreign_type) *
                                  (foreign_type)) -> order option),
                                  bottom    : foreign_type};

    fun get_mr_desc (MR {desc=d, ...}) = d;
    fun get_mr_compute (MR {compute=c,...}) = c;
    fun get_mr_ord_graph (MR {ord_graph=ord,...}) = ord;
    fun get_mr_ord (MR {ord=ord,...}) = ord;
    fun get_mr_bottom (MR {bottom=b,...}) = b;
 
    fun to_foreign_type (SOME (Tup1 a)) = SOME [a]
      | to_foreign_type (SOME (Tup2 (a,b))) = SOME [a, b]
      | to_foreign_type (SOME (Tup3 (a,b,c))) = SOME [a,b,c]
      | to_foreign_type (SOME (Tup4 (a,b,c,d))) = SOME [a,b,c,d]
      | to_foreign_type (SOME (Tup5 (a,b,c,d,e))) = SOME [a,b,c,d,e]
      | to_foreign_type (SOME (Tup6 (a,b,c,d,e,f))) = SOME [a,b,c,d,e,f]   
      | to_foreign_type _ = NONE

    
    fun to_foreign_type_safe ((Tup1 a)) = [a]
      | to_foreign_type_safe ((Tup2 (a,b))) = [a, b]
      | to_foreign_type_safe ((Tup3 (a,b,c))) = [a,b,c]
      | to_foreign_type_safe ((Tup4 (a,b,c,d))) = [a,b,c,d]
      | to_foreign_type_safe ((Tup5 (a,b,c,d,e))) = [a,b,c,d,e]
      | to_foreign_type_safe ((Tup6 (a,b,c,d,e,f))) = [a,b,c,d,e,f]   
      | to_foreign_type_safe Malformed = []

    fun from_foreign_type [a] = Tup1 a
      | from_foreign_type [a,b] = Tup2 (a,b)      
      | from_foreign_type [a,b,c] = Tup3 (a,b,c)      
      | from_foreign_type [a,b,c,d] = Tup4 (a,b,c,d)      
      | from_foreign_type [a,b,c,d,e] = Tup5 (a,b,c,d,e)      
      | from_foreign_type [a,b,c,d,e,f] = Tup6 (a,b,c,d,e,f)      
      | from_foreign_type _ = Malformed
 
 
  end;


