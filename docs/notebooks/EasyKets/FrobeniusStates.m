(* ::Package:: *)

BeginPackage["EasyKets`FrobeniusStates`",{"EasyKets`"}];

EasyKets`FrobeniusStates::usage=StringJoin[{
"Functions for manipulating and testing Frobenius states.\n\n",
fourstate::usage="fourstate[dim,state,cup] constructs a 4-partite state for checking
    strong symmetry.", "\n",
cap::usage="cap[dim,state,counit] builds a cap given a tripartite state an a counit.","\n",
isfrobstate::usage="isfrobstate[dim,state,cup,counit] checks if the given triple forms
    a Frobenius state.","\n",
comult::usage="comult[dim,state,cup] builds a comultiplication from the given data.","\n",
mult::usage="mult[dim,state,cup] builds a multiplication from the given data.","\n",
unit::usage="unit[dim,state,counit] builds a unit from the given data.","\n",
frobalg::usage="frobalg[dim,state,cup,counit] builds the usual 4-tuple of a Frobenius
    algebra, given the 3-tuple of a Frobenius state.","\n",
isantispecial::usage="isantispecial[dim,state,cup,counit] checks whether a given
    Frobenius state is anti-special."
}];

Begin["`Private`"];
fourstate[dim_,st_,cup_]:=eval[dim,T[id[dim^2],cup,id[dim^2]].T[st,st]];
cap[dim_,st_,counit_]:=eval[dim,T[dot,id[dim^2]].st];

isfrobstate[dim_,st_,cup_,counit_]:=
(eval[dim,T[cup,id[dim]].T[id[dim],cap[dim,st,counit]]]==eval[dim,id[dim]])&&
(issymm[fourstate[dim,st,cup]]);

comult[dim_,st_,cup_]:=eval[dim,T[id[dim^2],cup].T[st,id[dim]]];
mult[dim_,st_,cup_]:=eval[dim,T[id[dim],cup].T[comult[dim,st,cup],id[dim]]];
unit[dim_,st_,counit_]:=eval[dim,T[id[dim],counit,counit].st];

frobalg[dim_,st_,cup_,counit_]:={
  mult[dim,st,cup],
  unit[dim,st,counit],
  comult[dim,st,cup],
  counit
};

isantispecial[dim_,st_,cup_,counit_]:=Module[{mu,del,eta,eps},
  {mu,eta,del,eps}=frobalg[dim,st,cup,counit];
  dim*eval[dim,mu.del]==eval[dim,mu.del.eta.eps.mu.del]];
End[];
EndPackage[];
