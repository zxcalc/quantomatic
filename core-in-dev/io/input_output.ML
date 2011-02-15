signature INPUT =
sig
  type T
  type data
  val input : T -> data
  exception bad_input_exp of string;
end;

signature XML_INPUT = INPUT where type T = XML.tree;

signature OUTPUT =
sig
  type data
  type T
  val output : data -> T
end;

signature XML_OUTPUT = OUTPUT where type T = XML.tree;

signature PRETTY_XML_OUTPUT = OUTPUT where type T = Pretty.T;

