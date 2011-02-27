(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Computing rewrites of graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

signature REWRITE_SHARING =
sig
   
end;
 
signature REWRITE
= sig

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor RewriteFun(structure GraphSubst : GRAPH_SUBST)
= struct
  
end;


structure DB_Rewrite = RewriteFun(MatchSearch);
structure Rewrite : RULE = DB_Rewrite;
