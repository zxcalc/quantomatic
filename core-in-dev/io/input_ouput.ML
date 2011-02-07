signature INPUT =
sig
  type T
  type data
  val input : T -> data
  exception bad_input_exp of string;
end;

signature XML_INPUT =
sig include INPUT where type T = XML.tree end;

signature OUTPUT =
sig
  type data
  type T
  val output : data -> T
end;

signature XML_INPUT =
sig include OUTPUT where type T = XML.tree end;

