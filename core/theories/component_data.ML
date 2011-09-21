signature COMPONENT_DATA = 
sig
  datatype T = Unit
             | String of string
             | LinratExpr of LinratExpr.T;
  exception bad_format_exp of string;

  val xml_of : T -> XML.tree list
  val of_xml_unit : XML.tree list -> T
  val of_xml_string : XML.tree list -> T
  val of_xml_linrat : XML.tree list -> T

  val string_of : T -> string
  val of_string_unit : string -> T
  val of_string_string : string -> T
  val of_string_linrat : string -> T

  val pretty : T -> Pretty.T
end;

structure ComponentData : COMPONENT_DATA = 
struct
  datatype T = Unit
             | String of string
             | LinratExpr of LinratExpr.T;
  exception bad_format_exp of string;

  fun xml_of Unit = []
    | xml_of (String s) = [XML.Text s]
    | xml_of (LinratExpr e) = [OutputLinratXML.output e];

  fun of_xml_unit _ = Unit
  fun of_xml_string [XML.Text s] = String s
    | of_xml_string _ = raise bad_format_exp "Raw text expected"
  fun of_xml_linrat [n] = (LinratExpr (InputLinratXML.input n) handle XML_Parse_Utils.bad_input_exp(s,_) => raise bad_format_exp s)
    | of_xml_linrat _ = raise bad_format_exp "Single XML node expected"

  fun string_of Unit = ""
    | string_of (String s) = s
    | string_of (LinratExpr e) = Pretty.string_of (LinratExpr.pretty e)

  fun of_string_unit _ = Unit
  fun of_string_string s = (String s)
  fun of_string_linrat e = (LinratExpr (LinratExpr.parse e))
      handle LinratExpr.parse_exp => raise bad_format_exp ("\"" ^ e ^ "\" is not a valid linear rational expression")

  fun pretty Unit = Pretty.str ""
    | pretty (String s) = Pretty.quote (Pretty.str s)
    | pretty (LinratExpr e) = LinratExpr.pretty e
end;

signature ELEMENT_COMPONENT_DATA_PARAM =
sig
  (* type of data, e.g. vertex data *)
  type data
  exception unknown_type_exp of string;

  (* get the type-string of a bit of data *)
  (* this avoids having to format the data *)
  val type_of : data -> string;
  val cdata_of : data -> ComponentData.T;

  (* the default data *)
  val default_data : string -> data;

  val update : (ComponentData.T -> ComponentData.T) -> (data -> data);
end;

(* Basic implementation for things with no data *)
structure NoData_Param 
: ELEMENT_COMPONENT_DATA_PARAM where type data = unit 
= struct
  type data = unit
  exception unknown_type_exp of string;

  fun type_of () = "unit";

  fun cdata_of () = ComponentData.Unit;

  fun default_data "unit" = ()
    | default_data n = raise unknown_type_exp n;

  fun update _ = I;
end;

(* Adds OGraph semantics to a ELEMENT_COMPONENT_DATA_PARAM *)
functor OVertexComponentData_Param(
  structure VertexDataParam : ELEMENT_COMPONENT_DATA_PARAM
  structure OVertex : OVERTEX
  sharing type OVertex.IData.data = VertexDataParam.data
)
: ELEMENT_COMPONENT_DATA_PARAM
= struct 
  type data = OVertex.data;
  exception unknown_type_exp = VertexDataParam.unknown_type_exp

  fun type_of (OVertex.EVert) = "edge-point"
    | type_of (OVertex.IVert d) = VertexDataParam.type_of d;

  fun cdata_of (OVertex.EVert) = ComponentData.Unit
    | cdata_of (OVertex.IVert d) = VertexDataParam.cdata_of d;

  fun default_data "edge-point" = OVertex.EVert
    | default_data n = OVertex.IVert (VertexDataParam.default_data n);

  fun update _ OVertex.EVert = OVertex.EVert
    | update upd (OVertex.IVert d) = (OVertex.IVert (VertexDataParam.update upd d));
end;

signature ELEMENT_COMPONENT_DATA =
sig
  include ELEMENT_COMPONENT_DATA_PARAM;

  val type_and_cdata_of : data -> (string * ComponentData.T);

  val type_and_xml_of : data -> string * (XML.tree list);
  val of_type_and_xml : (string * (XML.tree list)) -> data;
  val update_from_xml : (XML.tree list) -> (data -> data);

  val type_and_string_of : data -> string * string;
  val of_type_and_string : (string * string) -> data;
  val update_from_string : string -> (data -> data);
end;

functor ElementComponentData(Param : ELEMENT_COMPONENT_DATA_PARAM) : ELEMENT_COMPONENT_DATA =
struct
  open Param;
  local open ComponentData in

  fun type_and_cdata_of d = (type_of d,cdata_of d);

  fun type_and_xml_of d =
    let
      val (t,cd) = type_and_cdata_of d
      fun to_xml Unit = []
        | to_xml (String s) = [XML.Text s]
        | to_xml (LinratExpr e) = [OutputLinratXML.output e]
    in
      (t,to_xml cd)
    end;

  fun update_from_xml xml =
    let
      fun xml_updater _ Unit = Unit
        | xml_updater [XML.Text s] (String _) = (String s)
        | xml_updater _            (String _) = raise bad_format_exp "String value expected"
        | xml_updater [xml] (LinratExpr _) = ((LinratExpr (InputLinratXML.input xml))
              handle XML_Parse_Utils.bad_input_exp (msg,_) => raise bad_format_exp msg)
        | xml_updater _     (LinratExpr _) = raise bad_format_exp "Single XML element expected"
    in
      update (xml_updater xml)
    end;

  fun of_type_and_xml (t,xml) =
      update_from_xml xml (default_data t)

  fun type_and_string_of d =
    let
      val (t,cd) = type_and_cdata_of d
      fun to_string Unit = ""
        | to_string (String s) = s
        | to_string (LinratExpr e) = Pretty.string_of (LinratExpr.pretty e)
    in
      (t,to_string cd)
    end;

  fun update_from_string str =
    let
      fun string_updater _   Unit = Unit
        | string_updater str (String _) = (String str)
        | string_updater str (LinratExpr _) = (LinratExpr (LinratExpr.parse str))
              handle LinratExpr.parse_exp => raise bad_format_exp ("\"" ^ str ^ "\" is not a valid linear rational expression")
    in
      update (string_updater str)
    end;

  fun of_type_and_string (t,str) =
      update_from_string str (default_data t)

  end; (* local open *)
end;

structure EmptyElementComponentData = ElementComponentData(NoData_Param);


(* vi:sts=2:ts=2:sw=2:et
*)
