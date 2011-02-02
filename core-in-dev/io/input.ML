signature INPUT =
sig
  type T
  type data
  val input : T -> data
  exception Bad_input_exp of string;
end;

signature XML_INPUT =
sig include INPUT where type T = XML.tree end;
