(* construct hilbert term from a DAG *)

structure Hilb2 =
struct
open RGGraph

datatype term =
	 Tens of term list | Comp of term list | V of Vertex.name |
	 Sigma of Permutation.T | Id of int

(* a component holds a term and a list of inputs in order *)
type component = Edge.name list * term * Edge.name list

(* tensor a list of components, gathering inputs *)
fun tensor clist = fold (fn (i1,t,o1) =>
			 fn (i1s, Tens ts, o1s) =>
			    (i1@i1s, Tens (t::ts), o1@o1s))
			clist ([], Tens [], [])

(* perform the composition t1 ; t2 *)
fun compose (i1, t1, o1) (i2, t2, o2) = let
    val oset = Edge.NSet.of_list o1
    val new_ins =  filter_out (Edge.NSet.contains oset) i2
    val ids = Id (length new_ins)
    val perm = Permutation.get_perm Edge.name_ord (o1 @ new_ins) i2
in
    (i1 @ new_ins, Comp [Tens [t1, ids], Sigma perm, t2], o2)
end


fun pretty_text (V n) = Pretty.str n
  | pretty_text (Id num) = Pretty.str ("id(" ^ (Int.toString num) ^ ")")
  | pretty_text (Sigma perm) = Pretty.block[Pretty.str "sig",
					    Pretty.str_list
						"[" "]"
						(map Int.toString perm)]
  | pretty_text (Comp lst) = Pretty.block
				 ([Pretty.str "("]@
				  (Pretty.separate
				       " o" (map pretty_text (rev lst)))@
				  [Pretty.str ")"])
  | pretty_text (Tens lst) = Pretty.block
				 ([Pretty.str "("]@
				  (Pretty.separate
				       " X" (map pretty_text lst))@
				  [Pretty.str ")"])

val print = Pretty.writeln o pretty_text
fun printc (_,t,_) = print t

end

val c1 = (["Ea", "Eb", "Ec"], Hilb2.V "c1", ["Ef", "Eg"])
val c2 = (["Ef", "Eh", "Eg"], Hilb2.V "c2", ["Eh", "Ei"])
