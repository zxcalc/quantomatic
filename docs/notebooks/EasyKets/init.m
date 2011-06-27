(* ::Package:: *)

Notation`AutoLoadNotationPalette=False;
BeginPackage["EasyKets`", {"Notation`"}];

EasyKets::usage=StringJoin[{
"Bras and kets are always expressed in the computational basis, with basis vectors
numbered from 0 to n-1. To input a ket, type \[EscapeKey]kk\[EscapeKey]. To input a bra, type \[EscapeKey]bb\[EscapeKey].
Product vectors are written as lists of numbers, e.g. \[VerticalSeparator]0,1,1\[RightAngleBracket]. Note that most functions
take the dimension of the space as an argument. To compute a reduced bra-ket
expression, use eval[dim, ...] or (... // eval[dim]).", "\n\n",
kt::usage = "kt[list] forms a new ket from the given list.", "\n",
br::usage = "br[list] forms a new bra from the given list.", "\n",
T::usage = "T[v1, v2, ...] computes an n-fold tensor product.", "\n",
id::usage = "id[n] is an n-dimensional identity matrix.", "\n",
swapmap::usage = "swapmap[dim] is a dim-dimensional swap gate.", "\n",
toket::usage = "toket[dim, m] converts a matrix m into a bra-ket expression.", "\n",
fromket::usage = "fromket[dim, e] converts bra-ket expression e into a matrix.", "\n",
symm::usage = "symm[e] symmetrises bra-ket expression e.", "\n",
issymm::usage = "issymm[e] returns true if e is symmetric.", "\n",
asymm::usage = "asymm[e] anti-symmetrises bra-ket expression e.", "\n",
isasymm::usage = "isasymm[e] returns true if e is anti-symmetric.", "\n",
scalareq::usage = "scalareq[dim, e1, e2] returns true if e1 = e2, up to a scalar.", "\n",
eval::usage = "eval[dim,e] evaluates and simplifies a bra-ket expression e.
eval[dim][e] is the curried form of eval[dim,e]."
}];



Begin["`Private`"];
SumList[lst_]:=Fold[Plus,0,lst];
SumTable[tab_]:=SumList[Flatten[tab]];
id[n_]:=SumList[Table[kt[i].br[i],{i,0,n-1}]];

allperms[lst_]:=Module[{lst2,len},
  lst2=lst;
  lst2[[2]]=lst[[1]]; lst2[[1]]=lst[[2]];
  len = Length[lst];
  If[len==2,{{lst},{lst2}},
  {
    Permute[lst,AlternatingGroup[len]],
    Permute[lst2,AlternatingGroup[len]]
  }]
];

SumPerms[efun_,ofun_,lst_]:=
  If[Length[lst]==1,efun@@lst[[1]],
    Module[{prms,len},
      prms=allperms[lst];
      len = Length[ prms[[1]] ];
      1/(Length[lst]!)*
        SumList[Table[
          efun[  prms[[1]][[j]]  ]+
          ofun[  prms[[2]][[j]]  ],
          {j,1,len} 
        ]]
    ]
  ];

T[]:={{1}};
T[X_]:=X;
T[X_,Y__]:=Simplify[Fold[KroneckerProduct,X,{Y}]];

toket[dim_,matr_]:=Module[
  {rows,cols,inparties,outparties,kets,bras,ketbra},
    rows = Length[matr];
    cols = Length[matr[[1]]];
    inparties=Log[dim,cols];
    outparties=Log[dim,rows];
    kets=Function[tup,kt@@tup]/@Tuples[Table[i,{i,0,dim-1}],outparties];
    bras=Function[tup,br@@tup]/@Tuples[Table[i,{i,0,dim-1}],inparties];
    ketbra[i_,j_]:=Switch[{inparties,outparties},
      {0,0},1,
      {0,_},kets[[i]],
      {_,0},bras[[j]],
      {_,_},kets[[i]].bras[[j]]
    ];

    SumTable[
      Table[Table[
        matr[[i]][[j]]*(ketbra[i,j]),
      {j,1,cols}],{i,1,rows}]]
];

fromket[dim_,term_]:=Module[{basis,evalkt,evalbr},
  basis=Table[Table[{If[i==j,1,0]},{j,1,dim}],{i,1,dim}];
  evalkt[lst__]:=T@@Function[i,basis[[i+1]]]/@{lst};
  evalbr[lst__]:=T@@Function[i,Transpose[basis[[i+1]]]]/@{lst};
  term/.{kt->evalkt,br->evalbr}];

symm[term_]:=Module[{symmket,symmbra},
  symmket[l__]:=SumPerms[(kt@@#)&,kt@@#&,{l}];
  symmbra[l__]:=SumPerms[(br@@#)&,br@@#&,{l}];
  Simplify[term/.{kt->symmket,br->symmbra}]];

asymm[term_]:=Module[{asymmket,asymmbra},
  asymmket[l__]:=SumPerms[(kt@@#)&,-(kt@@#)&,{l}];
  asymmbra[l__]:=SumPerms[(br@@#)&,-(br@@#)&,{l}];
  Simplify[term/.{kt->asymmket,br->asymmbra}]];

swapmap[dim_]:=SumTable[Table[Table[kt[i,j].br[j,i],{j,0,dim-1}],{i,0,dim-1}]];

issymm[st_]:=symm[st]===st;
isasymm[st_]:=asymm[st]===st;
eval[dim_,expr_]:=Simplify[toket[dim,fromket[dim,expr]]]/.{0 . _->0}/.{_ . 0->0};
eval[dim_]:=Function[expr,eval[dim,expr]];

firstnonzero[x_]:=Module[{lst},
  lst=DeleteCases[Flatten[x],0];
  If[Length[lst]==0,0,lst[[1]]]
];

scalareq[dim_,e1_,e2_]:=Module[{m1,m2,n1,n2},
  m1=fromket[dim,e1];
  m2=fromket[dim,e2];
  n1=firstnonzero[m1];
  n2=firstnonzero[m2];
  (1/n1)*m1==(1/n2)*m2
];

End[];



Notation[ParsedBoxWrapper[RowBox[{TagBox["\[LeftBracketingBar]", Identity, Selectable -> False, SyntaxForm -> "\[LeftAngleBracket]"], "lst__", "\[RightAngleBracket]"}]] \[DoubleLongLeftRightArrow] ParsedBoxWrapper[RowBox[{"kt", "[", "lst__", "]"}]]];
Notation[ParsedBoxWrapper[RowBox[{"\[LeftAngleBracket]", "lst__", TagBox["\[RightBracketingBar]", Identity, Selectable -> False, SyntaxForm -> "\[RightAngleBracket]"]}]] \[DoubleLongLeftRightArrow] ParsedBoxWrapper[RowBox[{"br", "[", "lst__", "]"}]]];
AddInputAlias[ParsedBoxWrapper[RowBox[{TagBox["\[LeftBracketingBar]", Identity, Selectable -> False, SyntaxForm -> "\[LeftAngleBracket]"], "\[SelectionPlaceholder]", "\[RightAngleBracket]"}]],"kk"];
AddInputAlias[ParsedBoxWrapper[RowBox[{"\[LeftAngleBracket]", "\[SelectionPlaceholder]", TagBox["\[RightBracketingBar]", Identity, Selectable -> False, SyntaxForm -> "\[RightAngleBracket]"]}]],"bb"];


EndPackage[];
