(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  
    This file defines the various kinds of global names structures and 
    frequently maps, bij, etc between them 
 *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 



(* this signature is used just to enforce type constraint that distinguish 
   different kinds of names *)
signature QUANTO_NAME 
= sig
  
  structure V : SSTR_NAME  (* vertices *)
  structure E : SSTR_NAME  (* Edges *)
 
  structure B : SSTR_NAME  (* Bang-boxes *)
  structure X : SSTR_NAME  (* eXpression variables in data *)
  structure R : SSTR_NAME  (* Rules *)
  structure T : SSTR_NAME  (* Theories *)
  structure TagName : SSTR_NAME
  structure GraphName : SSTR_NAME

  structure VTab : NAME_TABLE
      where type name = V.name
        and type NSet.T = V.NSet.T

  structure ETab : NAME_TABLE
      where type name = E.name
        and type NSet.T = E.NSet.T
  
  structure BTab : NAME_TABLE
      where type name = B.name
        and type NSet.T = B.NSet.T
  
  structure XTab : NAME_TABLE
      where type name = X.name
        and type NSet.T = X.NSet.T
  
  structure RTab : NAME_TABLE
      where type name = R.name
        and type NSet.T = R.NSet.T
  
  structure TTab : NAME_TABLE
      where type name = T.name
        and type NSet.T = T.NSet.T

  structure EVFn : NAME_FUNCTION
      where type dom = E.name
        and type DomSet.T = E.NSet.T
        and type cod = V.name
        and type CodSet.T = V.NSet.T

  structure BVRel : NAME_RELATION
    where type dom = B.name
      and type DomSet.T = B.NSet.T
      and type cod = V.name
      and type CodSet.T = V.NSet.T

  structure BBRel : NAME_RELATION
    where type dom = B.name
      and type DomSet.T = B.NSet.T
      and type cod = B.name
      and type CodSet.T = B.NSet.T

  structure RTagRel : NAME_RELATION
    where type dom = R.name
      and type DomSet.T = R.NSet.T
      and type cod = TagName.name
      and type CodSet.T = TagName.NSet.T

  structure VSub : NAME_SUBSTITUTION
    where type dom = V.name
      and type DomSet.T = V.NSet.T
      and type cod = V.name
      and type CodSet.T = V.NSet.T

  structure ESub : NAME_SUBSTITUTION
    where type dom = E.name
      and type DomSet.T = E.NSet.T
      and type cod = E.name
      and type CodSet.T = E.NSet.T

  structure BSub : NAME_SUBSTITUTION
    where type dom = B.name
      and type DomSet.T = B.NSet.T
      and type cod = B.name
      and type CodSet.T = B.NSet.T

  structure XSub : NAME_SUBSTITUTION
    where type dom = X.name
      and type DomSet.T = X.NSet.T
      and type cod = X.name
      and type CodSet.T = X.NSet.T

  structure RSub : NAME_SUBSTITUTION
    where type dom = R.name
      and type DomSet.T = R.NSet.T
      and type cod = R.name
      and type CodSet.T = R.NSet.T

  structure VVInj : NAME_INJECTION
    where type dom = V.name
      and type DomSet.T = V.NSet.T
      and type cod = V.name
      and type CodSet.T = V.NSet.T

  structure EEInj : NAME_INJECTION
    where type dom = E.name
      and type DomSet.T = E.NSet.T
      and type cod = E.name
      and type CodSet.T = E.NSet.T
end;

local 
structure QuantoNames :> QUANTO_NAME
= struct
  structure V = struct open SStrName; val default_name = mk "Va"; end (* vertices *)
  structure E = struct open SStrName; val default_name = mk "Ea"; end (* edges *)
  structure B = struct open SStrName; val default_name = mk "Ba"; end (* bang boxes *)
  structure X = struct open SStrName; val default_name = mk "x"; end (* expressions *)
  structure R = struct open SStrName; val default_name = mk "new-rule-1"; end (* Rules *)
  structure T = struct open SStrName; val default_name = mk "new-theory-1"; end  (* Theories *)
  
  structure TagName : SSTR_NAME = 
      struct open SStrName; val default_name = mk "new-ruleset-1"; end
  structure GraphName : SSTR_NAME = 
    struct open SStrName; val default_name = SStrName.mk "new-graph-1" end

  structure SStrTab = NameTable(structure Dom = SStrName)
  structure SStrFn  = NameFunction(structure Dom = SStrName; structure Cod = SStrName)
  structure SStrRel = NameRelation(structure Dom = SStrName; structure Cod = SStrName)
  structure SStrSub = NameSubstitution(structure Name = SStrName)
  structure SStrInj = NameInjection(structure Dom = SStrName; structure Cod = SStrName)

  structure VTab = SStrTab
  structure ETab = SStrTab
  structure BTab = SStrTab
  structure XTab = SStrTab
  structure RTab = SStrTab
  structure TTab = SStrTab

  structure EVFn = SStrFn

  structure BVRel  = SStrRel
  structure BBRel  = SStrRel
  structure RTagRel = SStrRel

  structure VSub = SStrSub
  structure ESub = SStrSub
  structure BSub = SStrSub
  structure XSub = SStrSub
  structure RSub = SStrSub

  structure VVInj = SStrInj
  structure EEInj = SStrInj
end
in
(* Put opaque Quanto names and name maps into the top-level *)
open QuantoNames

(* install pretty printers *)
local
    fun prettyVertex _ _ x =
      PolyML.PrettyString("\"" ^ V.string_of_name x ^ "\"");
    fun prettyEdge _ _ x =
      PolyML.PrettyString("\"" ^ E.string_of_name x ^ "\"");
    fun prettyBBox _ _ x =
      PolyML.PrettyString("\"" ^ B.string_of_name x ^ "\"");
in
    val () = PolyML.addPrettyPrinter prettyVertex;
    val () = PolyML.addPrettyPrinter prettyEdge;
    val () = PolyML.addPrettyPrinter prettyBBox;
end

end
