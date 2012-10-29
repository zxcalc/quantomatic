(* BASIC INPUT/OUTPUT *)
signature INPUT =
sig
  type T
  type data
  val input : T -> data

  exception bad_input_exp of string*string; (* message, location *)
end;


(* composition *)
functor ComposeInputs (
  structure In1 : INPUT
  structure In2 : INPUT
  sharing type In1.data = In2.T)
: INPUT
= struct
  type T = In1.T
  type data = In2.data
  exception bad_input_exp of string*string;
  fun input i = In2.input (In1.input i)
      handle In1.bad_input_exp e => raise bad_input_exp e
           | In2.bad_input_exp e => raise bad_input_exp e;
end;
