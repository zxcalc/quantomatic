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
  structure H : EHN_NAME;   (* Edge-halfs *)
  sharing type H.edgename = E.name;
  
  structure B : SSTR_NAME;  (* Bang-boxes *)
  structure X : SSTR_NAME;  (* eXpression variables in data *)
  structure R : SSTR_NAME;  (* Rules *)
  structure T : SSTR_NAME;  (* Theories *)
  structure UV : SSTR_NAME;  (* Vertex User Data *)

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

  structure HMap : NAME_MAP
    sharing HMap.Dom.Sharing = H.Sharing
    sharing HMap.Cod.Sharing = H.Sharing
  structure HInjEndo : NAME_INJENDO
    sharing HInjEndo.Dom.Sharing = H.Sharing

  structure BMap : NAME_MAP
    sharing BMap.Dom.Sharing = B.Sharing
    sharing BMap.Cod.Sharing = B.Sharing
  structure BInjEndo : NAME_INJENDO
    sharing BInjEndo.Dom.Sharing = B.Sharing

  structure VBMap : NAME_MAP
    sharing VBMap.Dom.Sharing = V.Sharing;
    sharing VBMap.Cod.Sharing = B.Sharing;

  structure UVMap : NAME_MAP
    sharing UVMap.Dom.Sharing = UV.Sharing
    sharing UVMap.Cod.Sharing = UV.Sharing
  structure UVInjEndo : NAME_INJENDO
    sharing UVInjEndo.Dom.Sharing = UV.Sharing

  (* *)
  structure VXBRel : NAME_BINREL
  sharing VXBRel.Dom.Sharing = V.Sharing;
  sharing VXBRel.Cod.Sharing = X.Sharing;

  structure VVUDBRel : NAME_BINREL
  sharing VVUDBRel.Dom.Sharing = V.Sharing;
  sharing VVUDBRel.Cod.Sharing = UV.Sharing;

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
  structure H = EhnFun(E); (* half-edges *)
  structure B = struct open SStrName; 
    val default_name = mk "Ba"; end; (* bang boxes *)
  structure X = struct open SStrName; 
    val default_name = mk "x"; end; (* expressions *)
  structure R = struct open SStrName; 
    val default_name = mk "new-rule-1"; end;   (* Rules *)
  structure T = struct open SStrName; 
    val default_name = mk "new-theory-1"; end;  (* Theories *)
  structure UV = struct open SStrName;
    val default_name = mk "Va"; end;  (* vertices *)
 
  structure VMap = SStrMap;
  structure VInjEndo = SStrInjEndo;
  
  structure EMap = SStrMap;
  structure EInjEndo = SStrInjEndo;

  structure HMap = NameMapFun(structure Dom = H and Cod = H);
  structure HInjEndo = NameInjEndoFun(H);
  
  structure BMap = SStrMap;
  structure BInjEndo = SStrInjEndo;

  structure UVMap = SStrMap;
  structure UVInjEndo = SStrInjEndo;

  (* Bijections between vertex names and Expression Names *)
  structure VXBRel = SStrBRel;
  structure VVUDBRel = SStrBRel;
  structure VBMap = SStrMap;
  
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
end;
