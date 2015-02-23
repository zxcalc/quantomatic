(* IMRPOVE: stop using StrNames and use SStrNames - can be more
   efficient and is safer *)
signature STR_NAME = NAME where type name = string;
structure StrName : STR_NAME =
  NameFun(structure Namer = StrNamer val default_name = "a");

structure StrMap = NameMapFun(structure Dom = StrName and Cod = StrName);
structure StrInjEndo = NameInjEndoFun(StrName);

(* Safe string names do not let you know the internal type of names; you 
   have to go through strings. *)
signature SSTR_NAME = SNAME where type src = string;

(* safe strings as names *)
structure SStrName :> SSTR_NAME
= struct
  structure S = NameFun(structure Namer = StrSNamer 
                         val default_name = StrSNamer.mk "s0");
  open S;
  type src = string;
  val mk = StrSNamer.mk;
  val dest = StrSNamer.dest;
end;

structure SStrMap = NameMapFun(structure Dom = SStrName and Cod = SStrName);
structure SStrInjEndo = NameInjEndoFun(SStrName);
structure SStrBRel = NameBRelFun(structure Dom = SStrName and Cod = SStrName);

signature STR_INT_NAME = NAME where type name = string * int;
structure StrIntName : STR_INT_NAME = 
  NameFun(structure Namer = StrIntNamer val default_name = ("a",0));

(* safe (string * int) names *)
signature SSTR_INT_NAME = SNAME where type src = string * int;
structure SStrIntName :> SSTR_INT_NAME
= struct
  open StrIntName;
  type src = string * int;
  val mk = I;
  val dest = I;
end;

signature INT_NAME = NAME where type name = int;
structure IntName : INT_NAME = 
  NameFun(structure Namer = IntNamer 
           val default_name = 0);

(* safe int names *)
signature SINT_NAME = SNAME where type src = int;
structure SIntName :> SINT_NAME
= struct
  open IntName;
  type src = int;
  val mk = I;
  val dest = I;
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(* Lifting into FreshNamers *)
structure IntNSet = IntName.NSet;
structure StrNSet = StrName.NSet;
structure StrIntNSet = StrIntName.NSet;

structure IntNTab = IntName.NTab;
structure StrNTab = StrName.NTab;
structure StrIntNTab = StrIntName.NTab;

structure IntTab = IntNTab.Tab;
structure StrTab = StrNTab.Tab;
structure StrIntTab = StrIntNTab.Tab;
