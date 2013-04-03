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
  
  structure V : SSTR_NAME;  (* vertices *)
  structure E : SSTR_NAME;  (* Edges *)
  
  structure B : SSTR_NAME;  (* Bang-boxes *)
  structure X : SSTR_NAME;  (* eXpression variables in data *)
  structure R : SSTR_NAME;  (* Rules *)
  structure T : SSTR_NAME;  (* Theories *)

  structure VMap : NAME_MAP
    sharing VMap.Dom.Sharing = V.Sharing
    sharing VMap.Cod.Sharing = V.Sharing
  structure VInjEndo : NAME_INJENDO
    sharing VInjEndo.Dom.Sharing = V.Sharing

  structure EMap : NAME_MAP
    sharing EMap.Dom.Sharing = E.Sharing
    sharing EMap.Cod.Sharing = E.Sharing
  structure EInjEndo : NAME_INJENDO
    sharing EInjEndo.Dom.Sharing = E.Sharing

  structure EVMap : NAME_MAP
    sharing EVMap.Dom.Sharing = E.Sharing
    sharing EVMap.Cod.Sharing = V.Sharing

  structure BMap : NAME_MAP
    sharing BMap.Dom.Sharing = B.Sharing
    sharing BMap.Cod.Sharing = B.Sharing
  structure BInjEndo : NAME_INJENDO
    sharing BInjEndo.Dom.Sharing = B.Sharing

  structure BVBRel : NAME_BINREL
    sharing BVBRel.Dom.Sharing = B.Sharing;
    sharing BVBRel.Cod.Sharing = V.Sharing;

  (* *)
  structure VXBRel : NAME_BINREL
  sharing VXBRel.Dom.Sharing = V.Sharing;
  sharing VXBRel.Cod.Sharing = X.Sharing;
  structure EXBRel : NAME_BINREL
  sharing EXBRel.Dom.Sharing = E.Sharing;
  sharing EXBRel.Cod.Sharing = X.Sharing;

  (* Tags to identify sets of rules *)
  structure TagName : SSTR_NAME;  

  (* *)
  structure TagRel : NAME_BINREL
    sharing TagRel.Dom.Sharing = R.Sharing;
    sharing TagRel.Cod.Sharing = TagName.Sharing;
  
    
  structure GraphName : SSTR_NAME;
end;

local 
structure QuantoNames :> QUANTO_NAME
= struct
  structure V = struct open SStrName;
    val default_name = mk "Va"; end;  (* vertices *)
  structure E = struct open SStrName;
    val default_name = mk "Ea"; end;  (* edges *)
  structure B = struct open SStrName; 
    val default_name = mk "Ba"; end; (* bang boxes *)
  structure X = struct open SStrName; 
    val default_name = mk "x"; end; (* expressions *)
  structure R = struct open SStrName; 
    val default_name = mk "new-rule-1"; end;   (* Rules *)
  structure T = struct open SStrName; 
    val default_name = mk "new-theory-1"; end;  (* Theories *)
  
  structure VMap = SStrMap;
  structure VInjEndo = SStrInjEndo;
  
  structure EMap = SStrMap;
  structure EInjEndo = SStrInjEndo;
  
  structure EVMap = SStrMap;
  
  structure BMap = SStrMap;
  structure BInjEndo = SStrInjEndo;

  (* Bijections between vertex names and Expression Names *)
  structure VXBRel = SStrBRel;
  structure EXBRel = SStrBRel;
  structure BVBRel = SStrBRel;
  
  (* Ruleset *)
  (* global name structure for tags *)
  structure TagName : SSTR_NAME = 
      struct open SStrName; val default_name = mk "new-ruleset-1"; end;
  (* binary relation between tags and rule names *)
  structure TagRel : NAME_BINREL = SStrBRel;
  
  structure GraphName : SSTR_NAME = 
    struct open SStrName; val default_name = SStrName.mk "new-graph-1" end;

end;
in
(* Put Quanto names into the top-level along with the appropriate 
   constraints. *)
open QuantoNames;

(* new unified-mapping structures *)
structure VTab = NameTable(structure Dom = V)
structure ETab = NameTable(structure Dom = E)
structure BTab = NameTable(structure Dom = B)
structure XTab = NameTable(structure Dom = X)
structure RTab = NameTable(structure Dom = R)
structure TTab = NameTable(structure Dom = T)

structure EVFn = NameFunction(structure Dom = E; structure Cod = V)
structure BVRl = NameRelation(structure Dom = B; structure Cod = V)
structure BBFn = NameFunction(structure Dom = B; structure Cod = B)

structure VInj = NameInjection(structure Dom = V; structure Cod = V)
structure EInj = NameInjection(structure Dom = E; structure Cod = E)
structure BInj = NameInjection(structure Dom = B; structure Cod = B)
structure XInj = NameInjection(structure Dom = X; structure Cod = X)

structure VXRl = NameRelation(structure Dom = V; structure Cod = X)
structure EXRl = NameRelation(structure Dom = E; structure Cod = X)
 

end;
