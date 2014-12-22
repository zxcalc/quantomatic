(*  Title:      Pure/net.ML
    Author:     Lawrence C Paulson, Cambridge University Computer Laboratory
    Copyright   1993  University of Cambridge

Discrimination nets: a data structure for indexing items

From the book
    E. Charniak, C. K. Riesbeck, D. V. McDermott.
    Artificial Intelligence Programming.
    (Lawrence Erlbaum Associates, 1980).  [Chapter 14]

match_term no longer treats abstractions as wildcards; instead they match
only wildcards in patterns.  Requires operands to be beta-eta-normal.
*)

signature NET =
sig
  type key
  val key_of_term: term -> key list
  val encode_type: typ -> term
  type 'a net
  val empty: 'a net
  val is_empty: 'a net -> bool
  exception INSERT
  val insert: ('a * 'a -> bool) -> key list * 'a -> 'a net -> 'a net
  val insert_term: ('a * 'a -> bool) -> term * 'a -> 'a net -> 'a net
  val insert_safe: ('a * 'a -> bool) -> key list * 'a -> 'a net -> 'a net
  val insert_term_safe: ('a * 'a -> bool) -> term * 'a -> 'a net -> 'a net
  exception DELETE
  val delete: ('b * 'a -> bool) -> key list * 'b -> 'a net -> 'a net
  val delete_term: ('b * 'a -> bool) -> term * 'b -> 'a net -> 'a net
  val delete_safe: ('b * 'a -> bool) -> key list * 'b -> 'a net -> 'a net
  val delete_term_safe: ('b * 'a -> bool) -> term * 'b -> 'a net -> 'a net
  val lookup: 'a net -> key list -> 'a list
  val match_term: 'a net -> term -> 'a list
  val unify_term: 'a net -> term -> 'a list
  val entries: 'a net -> 'a list
  val subtract: ('b * 'a -> bool) -> 'a net -> 'b net -> 'b list
  val merge: ('a * 'a -> bool) -> 'a net * 'a net -> 'a net
  val content: 'a net -> 'a list
end;

structure Net: NET =
struct

datatype key = CombK | VarK | AtomK of string;

(*Keys are preorder lists of symbols -- Combinations, Vars, Atoms.
  Any term whose head is a Var is regarded entirely as a Var.
  Abstractions are also regarded as Vars;  this covers eta-conversion
    and "near" eta-conversions such as %x.?P(?f(x)).
*)
fun add_key_of_terms (t, cs) =
  let fun rands (f$t, cs) = CombK :: rands (f, add_key_of_terms(t, cs))
        | rands (Const(c,_), cs) = AtomK c :: cs
        | rands (Free(c,_),  cs) = AtomK c :: cs
        | rands (Bound i,  cs)   = AtomK (Name.bound i) :: cs
  in case head_of t of
      Var _ => VarK :: cs
    | Abs _ => VarK :: cs
    | _     => rands(t,cs)
  end;

(*convert a term to a list of keys*)
fun key_of_term t = add_key_of_terms (t, []);

(*encode_type -- for indexing purposes*)
fun encode_type (Type (c, Ts)) = Term.list_comb (Const (c, dummyT), map encode_type Ts)
  | encode_type (TFree (a, _)) = Free (a, dummyT)
  | encode_type (TVar (a, _)) = Var (a, dummyT);


(*Trees indexed by key lists: each arc is labelled by a key.
  Each node contains a list of items, and arcs to children.
  The empty key addresses the entire net.
  Lookup functions preserve order in items stored at same level.
*)
datatype 'a net = Leaf of 'a list
                | Net of {comb: 'a net,
                          var: 'a net,
                          atoms: 'a net Symtab.table};

val empty = Leaf[];
fun is_empty (Leaf []) = true | is_empty _ = false;
val emptynet = Net{comb=empty, var=empty, atoms=Symtab.empty};


(*** Insertion into a discrimination net ***)

exception INSERT;       (*duplicate item in the net*)


(*Adds item x to the list at the node addressed by the keys.
  Creates node if not already present.
  eq is the equality test for items.
  The empty list of keys generates a Leaf node, others a Net node.
*)
fun insert eq (keys,x) net =
  let fun ins1 ([], Leaf xs) =
            if member eq xs x then  raise INSERT  else Leaf(x::xs)
        | ins1 (keys, Leaf[]) = ins1 (keys, emptynet)   (*expand empty...*)
        | ins1 (CombK :: keys, Net{comb,var,atoms}) =
            Net{comb=ins1(keys,comb), var=var, atoms=atoms}
        | ins1 (VarK :: keys, Net{comb,var,atoms}) =
            Net{comb=comb, var=ins1(keys,var), atoms=atoms}
        | ins1 (AtomK a :: keys, Net{comb,var,atoms}) =
            let val atoms' = Symtab.map_default (a, empty) (fn net' => ins1 (keys, net')) atoms;
            in  Net{comb=comb, var=var, atoms=atoms'}  end
  in  ins1 (keys,net)  end;

fun insert_term eq (t, x) = insert eq (key_of_term t, x);

fun insert_safe eq entry net = insert eq entry net handle INSERT => net;
fun insert_term_safe eq entry net = insert_term eq entry net handle INSERT => net;


(*** Deletion from a discrimination net ***)

exception DELETE;       (*missing item in the net*)

(*Create a new Net node if it would be nonempty*)
fun newnet (args as {comb,var,atoms}) =
  if is_empty comb andalso is_empty var andalso Symtab.is_empty atoms
  then empty else Net args;

(*Deletes item x from the list at the node addressed by the keys.
  Raises DELETE if absent.  Collapses the net if possible.
  eq is the equality test for items. *)
fun delete eq (keys, x) net =
  let fun del1 ([], Leaf xs) =
            if member eq xs x then Leaf (remove eq x xs)
            else raise DELETE
        | del1 (keys, Leaf[]) = raise DELETE
        | del1 (CombK :: keys, Net{comb,var,atoms}) =
            newnet{comb=del1(keys,comb), var=var, atoms=atoms}
        | del1 (VarK :: keys, Net{comb,var,atoms}) =
            newnet{comb=comb, var=del1(keys,var), atoms=atoms}
        | del1 (AtomK a :: keys, Net{comb,var,atoms}) =
            let val atoms' =
              (case Symtab.lookup atoms a of
                NONE => raise DELETE
              | SOME net' =>
                  (case del1 (keys, net') of
                    Leaf [] => Symtab.delete a atoms
                  | net'' => Symtab.update (a, net'') atoms))
            in  newnet{comb=comb, var=var, atoms=atoms'}  end
  in  del1 (keys,net)  end;

fun delete_term eq (t, x) = delete eq (key_of_term t, x);

fun delete_safe eq entry net = delete eq entry net handle DELETE => net;
fun delete_term_safe eq entry net = delete_term eq entry net handle DELETE => net;


(*** Retrieval functions for discrimination nets ***)

exception ABSENT;

fun the_atom atoms a =
  (case Symtab.lookup atoms a of
    NONE => raise ABSENT
  | SOME net => net);

(*Return the list of items at the given node, [] if no such node*)
fun lookup (Leaf xs) [] = xs
  | lookup (Leaf _) (_ :: _) = []  (*non-empty keys and empty net*)
  | lookup (Net {comb, var, atoms}) (CombK :: keys) = lookup comb keys
  | lookup (Net {comb, var, atoms}) (VarK :: keys) = lookup var keys
  | lookup (Net {comb, var, atoms}) (AtomK a :: keys) =
      lookup (the_atom atoms a) keys handle ABSENT => [];


(*Skipping a term in a net.  Recursively skip 2 levels if a combination*)
fun net_skip (Leaf _) nets = nets
  | net_skip (Net{comb,var,atoms}) nets =
      fold_rev net_skip (net_skip comb []) (Symtab.fold (cons o #2) atoms (var::nets));


(** Matching and Unification **)

(*conses the linked net, if present, to nets*)
fun look1 (atoms, a) nets =
  the_atom atoms a :: nets handle ABSENT => nets;

(*Return the nodes accessible from the term (cons them before nets)
  "unif" signifies retrieval for unification rather than matching.
  Var in net matches any term.
  Abs or Var in object: if "unif", regarded as wildcard,
                                   else matches only a variable in net.
*)
fun matching unif t net nets =
  let fun rands _ (Leaf _, nets) = nets
        | rands t (Net{comb,atoms,...}, nets) =
            case t of
                f$t => fold_rev (matching unif t) (rands f (comb,[])) nets
              | Const(c,_) => look1 (atoms, c) nets
              | Free(c,_)  => look1 (atoms, c) nets
              | Bound i    => look1 (atoms, Name.bound i) nets
              | _          => nets
  in
     case net of
         Leaf _ => nets
       | Net{var,...} =>
             case head_of t of
                 Var _ => if unif then net_skip net nets
                          else var::nets           (*only matches Var in net*)
  (*If "unif" then a var instantiation in the abstraction could allow
    an eta-reduction, so regard the abstraction as a wildcard.*)
               | Abs _ => if unif then net_skip net nets
                          else var::nets           (*only a Var can match*)
               | _ => rands t (net, var::nets)  (*var could match also*)
  end;

fun extract_leaves l = maps (fn Leaf xs => xs) l;

(*return items whose key could match t, WHICH MUST BE BETA-ETA NORMAL*)
fun match_term net t =
    extract_leaves (matching false t net []);

(*return items whose key could unify with t*)
fun unify_term net t =
    extract_leaves (matching true t net []);


(** operations on nets **)

(*subtraction: collect entries of second net that are NOT present in first net*)
fun subtract eq net1 net2 =
  let
    fun subtr (Net _) (Leaf ys) = append ys
      | subtr (Leaf xs) (Leaf ys) =
          fold_rev (fn y => if member eq xs y then I else cons y) ys
      | subtr (Leaf _) (net as Net _) = subtr emptynet net
      | subtr (Net {comb = comb1, var = var1, atoms = atoms1})
            (Net {comb = comb2, var = var2, atoms = atoms2}) =
          subtr comb1 comb2
          #> subtr var1 var2
          #> Symtab.fold (fn (a, net) =>
            subtr (the_default emptynet (Symtab.lookup atoms1 a)) net) atoms2
  in subtr net1 net2 [] end;

fun entries net = subtract (K false) empty net;


(* merge *)

fun cons_fst x (xs, y) = (x :: xs, y);

fun dest (Leaf xs) = map (pair []) xs
  | dest (Net {comb, var, atoms}) =
      map (cons_fst CombK) (dest comb) @
      map (cons_fst VarK) (dest var) @
      maps (fn (a, net) => map (cons_fst (AtomK a)) (dest net)) (Symtab.dest atoms);

fun merge eq (net1, net2) =
  fold (insert_safe eq) (dest net2) net1;  (* FIXME non-canonical merge order!?! *)

fun content net = map #2 (dest net);

end;
