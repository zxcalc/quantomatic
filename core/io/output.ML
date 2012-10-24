signature OUTPUT =
sig
  type data
  type T
  val output : data -> T
end;

functor OutputListify(Out1 : OUTPUT)
= struct
  type T = Out1.T list
  type data = Out1.data
  fun output x = [Out1.output x]
end;

(* composition *)
functor ComposeOutputs (
  structure Out1 : OUTPUT
  structure Out2 : OUTPUT
  sharing type Out2.data = Out1.T)
: OUTPUT
= struct
  type T = Out2.T
  type data = Out1.data
  val output = Out2.output o Out1.output;
end;


functor OutputMap (OutputItem : OUTPUT) : OUTPUT
= 
struct
  type data = OutputItem.data list 
  type T = OutputItem.T list
  val output = map OutputItem.output;
end;

