<< Combinatorica`;
id = IdentityMatrix[2];
T[X_, Y__] := Simplify[Fold[KroneckerProduct, X, {Y}]];
Dag[X_] := ConjugateTranspose[X];
rootd = Sqrt[2];
h = 1/rootd ({{1, 1}, {1, -1}});
b0 = {{1},{0}}; b1 = {{0},{1}};
compbasis = {b0, b1};
PermutedTensor[perm_] :=
  (perm1 = Function[x, x + 1] /@ perm; 
   Function[p, Permute[p, perm1]] /@ Tuples[{1, 2}, Length[perm1]]);
sig[perm__] := 
  Function[t, Flatten[T @@ t]] /@ 
  Function[b, compbasis[[b]]] /@ PermutedTensor[{perm}];
bplus = h.b0; bminus = h.b1;
ezdag = rootd*bplus; exdag = h.ezdag;
ez = Dag[ezdag]; ex = ez.h;
dz = T[Dag[b0], T[b0, b0]] + T[Dag[b1], T[b1, b1]];
dzdag = Dag[dz];
dx = T[h, h].dz.h;
dxdag = h.dzdag.T[h, h];
unbiasedz[a_] := b0 + \[ExponentialE]^(\[ImaginaryI] a)*b1;
az[a_] := dzdag.T[id, unbiasedz[a]];
ax[a_] := h.az[a].h
bell = dz.ezdag; belldag = Dag[bell];

(* some unit tests *)
{ez.b0 == IdentityMatrix[1], ez.b1 == IdentityMatrix[1], 
 ex.bplus == IdentityMatrix[1], ex.bplus == IdentityMatrix[1], 
 T[exdag, exdag] == rootd*dz.exdag, T[ezdag, ezdag] == rootd*dx.ezdag,
  dzdag.T[id, ezdag] == id, dxdag.T[id, exdag] == id, 
 sig[0, 2, 1].T[{{x1}, {x2}}, {{y1}, {y2}}, {{z1}, {z2}}] == 
  T[{{x1}, {x2}}, {{z1}, {z2}}, {{y1}, {y2}}], unbiasedz[0] == ezdag, 
 unbiasedz[\[Pi]] == rootd*bminus}
