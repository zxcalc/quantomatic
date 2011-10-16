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
  structure B : SSTR_NAME;  (* Bang-boxes *)
  structure X : SSTR_NAME;  (* eXpression variables in data *)
  structure R : SSTR_NAME;  (* Rules *)
  structure T : SSTR_NAME;  (* Theories *)

  structure VMap : NAME_MAP
  sharing VMap.Dom.Sharing = V.Sharing
  sharing VMap.Cod.Sharing = V.Sharing
  
  structure VInjEndo : NAME_INJENDO
  sharing VInjEndo.Dom.Sharing = V.Sharing
  
  (* *)
  structure VXBRel : NAME_BINREL
  sharing VXBRel.Dom.Sharing = V.Sharing;
  sharing VXBRel.Cod.Sharing = X.Sharing;
end;

structure QuantoNames :> QUANTO_NAME
= struct
  structure V = SStrName;  (* vertices *)
  structure E = SStrName;  (* edges *)
  structure H = EhnFun(E); (* half-edges *)
  structure B = SStrName;  (* bang boxes *)
  structure X = SStrName;  (* expressions *)
  structure R = SStrName;  (* Rules *)
  structure T = SStrName;  (* Theories *)

  structure VMap = SStrMap;
  structure VInjEndo = SStrInjEndo;
  (* Bijections between vertex names and Expression Names *)
  structure VXBRel = SStrBRel;
end;

(* Put Quanto names into the top-level along with the appropriate 
   constraints. *)
open QuantoNames;

