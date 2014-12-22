signature METRIC =
sig

  type T (* Actually redundant since we know that it is going to be of 
          * type metric_type, i.e. an int tuple of length <= 6 *)
 
  structure MT : METRIC_TYPE

  val name : string
  val desc : string

  val compute : MT.Graph.T -> T option (* In case a metric is for a specific
  graphical theory : we allow it to fail *)
  val ord : (T * T) -> order option
  val ord_graph : (MT.Graph.T * MT.Graph.T) -> order option
  val bottom : T;

  val metric_record : MT.metric_rec;
end;


