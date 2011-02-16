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

functor OutputListXML (
  val list_name : string
	structure Output1 : PRETTY_XML_OUTPUT
) : PRETTY_XML_OUTPUT
= 
struct
  type data = Output1.data list 
  type T = Pretty.T

  fun output rewrites = 
      Pretty.chunks [ Pretty.str ("<"^list_name^">"), 
		      Pretty.indent 2 (Pretty.block (map Output1.output rewrites)),
		      Pretty.str ("</"^list_name^">"),
		      Pretty.str ""
		    ];
end;

