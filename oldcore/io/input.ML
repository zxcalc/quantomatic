signature INPUT =
sig
  type T
  type data
  val input : T -> data

  exception bad_input_exp of string*string; (* message, location *)
end;
