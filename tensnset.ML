(* A General way to provide an quickly updateable ordering for a set
of names. Average runtime is good (log n, where n is number of objects
in set), but at certain times it can be slower: linear in number of
objects. However, it only has this "bad" runtime at most once every n
operations. Thus the average time falls back to being log.

We allow insert-after style operations. These really are dynamically
ordered sets, where you can edit the ordering efficiently.

I cannot think of a way to get a smooth time usage; I conjecture it is
not possible.

Balanced (but not fully compactly) binary tree are used to define an
ordering for a set of objects. We can give the addresses of objects,
in the tree, as ID's, and we can then have insert-after and insert
before operations.

Note: this is also a way of doing dynamic precedence for a parser as
you get quick comparison and maintain the ability to do update. *)

signature TensorNameSet

structure TensorNameSet
(structure NTab : FNAME_TAB

)
= struct



datatype T = TNSet {poses : pos NTab.T,
                    ptab : ptab
