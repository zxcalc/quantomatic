(*  Title:      Pure/sorts.ML
    Author:     Markus Wenzel and Stefan Berghofer, TU Muenchen

The order-sorted algebra of type classes.

Classes denote (possibly empty) collections of types that are
partially ordered by class inclusion. They are represented
symbolically by strings.

Sorts are intersections of finitely many classes. They are represented
by lists of classes.  Normal forms of sorts are sorted lists of
minimal classes (wrt. current class inclusion).
*)

signature SORTS =
sig
  val make: sort list -> sort Ord_List.T
  val subset: sort Ord_List.T * sort Ord_List.T -> bool
  val union: sort Ord_List.T -> sort Ord_List.T -> sort Ord_List.T
  val subtract: sort Ord_List.T -> sort Ord_List.T -> sort Ord_List.T
  val remove_sort: sort -> sort Ord_List.T -> sort Ord_List.T
  val insert_sort: sort -> sort Ord_List.T -> sort Ord_List.T
  val insert_typ: typ -> sort Ord_List.T -> sort Ord_List.T
  val insert_typs: typ list -> sort Ord_List.T -> sort Ord_List.T
  val insert_term: term -> sort Ord_List.T -> sort Ord_List.T
  val insert_terms: term list -> sort Ord_List.T -> sort Ord_List.T
  type algebra
  val classes_of: algebra -> serial Graph.T
  val arities_of: algebra -> (class * sort list) list Symtab.table
  val all_classes: algebra -> class list
  val super_classes: algebra -> class -> class list
  val class_less: algebra -> class * class -> bool
  val class_le: algebra -> class * class -> bool
  val sort_eq: algebra -> sort * sort -> bool
  val sort_le: algebra -> sort * sort -> bool
  val sorts_le: algebra -> sort list * sort list -> bool
  val inter_sort: algebra -> sort * sort -> sort
  val minimize_sort: algebra -> sort -> sort
  val complete_sort: algebra -> sort -> sort
  val minimal_sorts: algebra -> sort list -> sort Ord_List.T
  val add_class: Context.pretty -> class * class list -> algebra -> algebra
  val add_classrel: Context.pretty -> class * class -> algebra -> algebra
  val add_arities: Context.pretty -> string * (class * sort list) list -> algebra -> algebra
  val empty_algebra: algebra
  val merge_algebra: Context.pretty -> algebra * algebra -> algebra
  val subalgebra: Context.pretty -> (class -> bool) -> (class * string -> sort list option)
    -> algebra -> (sort -> sort) * algebra
  type class_error
  val class_error: Context.pretty -> class_error -> string
  exception CLASS_ERROR of class_error
  val has_instance: algebra -> string -> sort -> bool
  val mg_domain: algebra -> string -> sort -> sort list   (*exception CLASS_ERROR*)
  val meet_sort: algebra -> typ * sort
    -> sort Vartab.table -> sort Vartab.table   (*exception CLASS_ERROR*)
  val meet_sort_typ: algebra -> typ * sort -> typ -> typ   (*exception CLASS_ERROR*)
  val of_sort: algebra -> typ * sort -> bool
  val of_sort_derivation: algebra ->
    {class_relation: typ -> 'a * class -> class -> 'a,
     type_constructor: string * typ list -> ('a * class) list list -> class -> 'a,
     type_variable: typ -> ('a * class) list} ->
    typ * sort -> 'a list   (*exception CLASS_ERROR*)
  val classrel_derivation: algebra ->
    ('a * class -> class -> 'a) -> 'a * class -> class -> 'a  (*exception CLASS_ERROR*)
  val witness_sorts: algebra -> string list -> (typ * sort) list -> sort list -> (typ * sort) list
end;

structure Sorts: SORTS =
struct


(** ordered lists of sorts **)

val make = Ord_List.make Term_Ord.sort_ord;
val subset = Ord_List.subset Term_Ord.sort_ord;
val union = Ord_List.union Term_Ord.sort_ord;
val subtract = Ord_List.subtract Term_Ord.sort_ord;

val remove_sort = Ord_List.remove Term_Ord.sort_ord;
val insert_sort = Ord_List.insert Term_Ord.sort_ord;

fun insert_typ (TFree (_, S)) Ss = insert_sort S Ss
  | insert_typ (TVar (_, S)) Ss = insert_sort S Ss
  | insert_typ (Type (_, Ts)) Ss = insert_typs Ts Ss
and insert_typs [] Ss = Ss
  | insert_typs (T :: Ts) Ss = insert_typs Ts (insert_typ T Ss);

fun insert_term (Const (_, T)) Ss = insert_typ T Ss
  | insert_term (Free (_, T)) Ss = insert_typ T Ss
  | insert_term (Var (_, T)) Ss = insert_typ T Ss
  | insert_term (Bound _) Ss = Ss
  | insert_term (Abs (_, T, t)) Ss = insert_term t (insert_typ T Ss)
  | insert_term (t $ u) Ss = insert_term t (insert_term u Ss);

fun insert_terms [] Ss = Ss
  | insert_terms (t :: ts) Ss = insert_terms ts (insert_term t Ss);



(** order-sorted algebra **)

(*
  classes: graph representing class declarations together with proper
    subclass relation, which needs to be transitive and acyclic.

  arities: table of association lists of all type arities; (t, ars)
    means that type constructor t has the arities ars; an element
    (c, Ss) of ars represents the arity t::(Ss)c.  "Coregularity" of
    the arities structure requires that for any two declarations
    t::(Ss1)c1 and t::(Ss2)c2 such that c1 <= c2 holds Ss1 <= Ss2.
*)

datatype algebra = Algebra of
 {classes: serial Graph.T,
  arities: (class * sort list) list Symtab.table};

fun classes_of (Algebra {classes, ...}) = classes;
fun arities_of (Algebra {arities, ...}) = arities;

fun make_algebra (classes, arities) =
  Algebra {classes = classes, arities = arities};

fun map_classes f (Algebra {classes, arities}) = make_algebra (f classes, arities);
fun map_arities f (Algebra {classes, arities}) = make_algebra (classes, f arities);


(* classes *)

fun all_classes (Algebra {classes, ...}) = Graph.all_preds classes (Graph.maximals classes);

val super_classes = Graph.immediate_succs o classes_of;


(* class relations *)

val class_less = Graph.is_edge o classes_of;
fun class_le algebra (c1, c2) = c1 = c2 orelse class_less algebra (c1, c2);


(* sort relations *)

fun sort_le algebra (S1, S2) =
  S1 = S2 orelse forall (fn c2 => exists (fn c1 => class_le algebra (c1, c2)) S1) S2;

fun sorts_le algebra (Ss1, Ss2) =
  ListPair.all (sort_le algebra) (Ss1, Ss2);

fun sort_eq algebra (S1, S2) =
  sort_le algebra (S1, S2) andalso sort_le algebra (S2, S1);


(* intersection *)

fun inter_class algebra c S =
  let
    fun intr [] = [c]
      | intr (S' as c' :: c's) =
          if class_le algebra (c', c) then S'
          else if class_le algebra (c, c') then intr c's
          else c' :: intr c's
  in intr S end;

fun inter_sort algebra (S1, S2) =
  sort_strings (fold (inter_class algebra) S1 S2);


(* normal forms *)

fun minimize_sort _ [] = []
  | minimize_sort _ (S as [_]) = S
  | minimize_sort algebra S =
      filter (fn c => not (exists (fn c' => class_less algebra (c', c)) S)) S
      |> sort_distinct string_ord;

fun complete_sort algebra =
  Graph.all_succs (classes_of algebra) o minimize_sort algebra;

fun minimal_sorts algebra raw_sorts =
  let
    fun le S1 S2 = sort_le algebra (S1, S2);
    val sorts = make (map (minimize_sort algebra) raw_sorts);
  in sorts |> filter_out (fn S => exists (fn S' => le S' S andalso not (le S S')) sorts) end;



(** build algebras **)

(* classes *)

fun err_dup_class c = error ("Duplicate declaration of class: " ^ quote c);

fun err_cyclic_classes pp css =
  error (cat_lines (map (fn cs =>
    "Cycle in class relation: " ^ Syntax.string_of_classrel (Syntax.init_pretty pp) cs) css));

fun add_class pp (c, cs) = map_classes (fn classes =>
  let
    val classes' = classes |> Graph.new_node (c, serial ())
      handle Graph.DUP dup => err_dup_class dup;
    val classes'' = classes' |> fold Graph.add_edge_trans_acyclic (map (pair c) cs)
      handle Graph.CYCLES css => err_cyclic_classes pp css;
  in classes'' end);


(* arities *)

local

fun for_classes _ NONE = ""
  | for_classes ctxt (SOME (c1, c2)) = " for classes " ^ Syntax.string_of_classrel ctxt [c1, c2];

fun err_conflict pp t cc (c, Ss) (c', Ss') =
  let val ctxt = Syntax.init_pretty pp in
    error ("Conflict of type arities" ^ for_classes ctxt cc ^ ":\n  " ^
      Syntax.string_of_arity ctxt (t, Ss, [c]) ^ " and\n  " ^
      Syntax.string_of_arity ctxt (t, Ss', [c']))
  end;

fun coregular pp algebra t (c, Ss) ars =
  let
    fun conflict (c', Ss') =
      if class_le algebra (c, c') andalso not (sorts_le algebra (Ss, Ss')) then
        SOME ((c, c'), (c', Ss'))
      else if class_le algebra (c', c) andalso not (sorts_le algebra (Ss', Ss)) then
        SOME ((c', c), (c', Ss'))
      else NONE;
  in
    (case get_first conflict ars of
      SOME ((c1, c2), (c', Ss')) => err_conflict pp t (SOME (c1, c2)) (c, Ss) (c', Ss')
    | NONE => (c, Ss) :: ars)
  end;

fun complete algebra (c, Ss) = map (rpair Ss) (c :: super_classes algebra c);

fun insert pp algebra t (c, Ss) ars =
  (case AList.lookup (op =) ars c of
    NONE => coregular pp algebra t (c, Ss) ars
  | SOME Ss' =>
      if sorts_le algebra (Ss, Ss') then ars
      else if sorts_le algebra (Ss', Ss)
      then coregular pp algebra t (c, Ss) (remove (op =) (c, Ss') ars)
      else err_conflict pp t NONE (c, Ss) (c, Ss'));

in

fun insert_ars pp algebra t = fold_rev (insert pp algebra t);

fun insert_complete_ars pp algebra (t, ars) arities =
  let val ars' =
    Symtab.lookup_list arities t
    |> fold_rev (insert_ars pp algebra t) (map (complete algebra) ars);
  in Symtab.update (t, ars') arities end;

fun add_arities pp arg algebra =
  algebra |> map_arities (insert_complete_ars pp algebra arg);

fun add_arities_table pp algebra =
  Symtab.fold (fn (t, ars) => insert_complete_ars pp algebra (t, ars));

end;


(* classrel *)

fun rebuild_arities pp algebra = algebra |> map_arities (fn arities =>
  Symtab.empty
  |> add_arities_table pp algebra arities);

fun add_classrel pp rel = rebuild_arities pp o map_classes (fn classes =>
  classes |> Graph.add_edge_trans_acyclic rel
    handle Graph.CYCLES css => err_cyclic_classes pp css);


(* empty and merge *)

val empty_algebra = make_algebra (Graph.empty, Symtab.empty);

fun merge_algebra pp
   (Algebra {classes = classes1, arities = arities1},
    Algebra {classes = classes2, arities = arities2}) =
  let
    val classes' = Graph.merge_trans_acyclic (op =) (classes1, classes2)
      handle Graph.DUP c => err_dup_class c
        | Graph.CYCLES css => err_cyclic_classes pp css;
    val algebra0 = make_algebra (classes', Symtab.empty);
    val arities' =
      (case (pointer_eq (classes1, classes2), pointer_eq (arities1, arities2)) of
        (true, true) => arities1
      | (true, false) =>  (*no completion*)
          (arities1, arities2) |> Symtab.join (fn t => fn (ars1, ars2) =>
            if pointer_eq (ars1, ars2) then raise Symtab.SAME
            else insert_ars pp algebra0 t ars2 ars1)
      | (false, true) =>  (*unary completion*)
          Symtab.empty
          |> add_arities_table pp algebra0 arities1
      | (false, false) => (*binary completion*)
          Symtab.empty
          |> add_arities_table pp algebra0 arities1
          |> add_arities_table pp algebra0 arities2);
  in make_algebra (classes', arities') end;


(* algebra projections *)  (* FIXME potentially violates abstract type integrity *)

fun subalgebra pp P sargs (algebra as Algebra {classes, arities}) =
  let
    val restrict_sort = minimize_sort algebra o filter P o Graph.all_succs classes;
    fun restrict_arity t (c, Ss) =
      if P c then
        (case sargs (c, t) of
          SOME sorts =>
            SOME (c, Ss |> map2 (curry (inter_sort algebra)) sorts |> map restrict_sort)
        | NONE => NONE)
      else NONE;
    val classes' = classes |> Graph.restrict P;
    val arities' = arities |> Symtab.map (map_filter o restrict_arity);
  in (restrict_sort, rebuild_arities pp (make_algebra (classes', arities'))) end;



(** sorts of types **)

(* errors -- performance tuning via delayed message composition *)

datatype class_error =
  No_Classrel of class * class |
  No_Arity of string * class |
  No_Subsort of sort * sort;

fun class_error pp =
  let val ctxt = Syntax.init_pretty pp in
    fn No_Classrel (c1, c2) => "No class relation " ^ Syntax.string_of_classrel ctxt [c1, c2]
     | No_Arity (a, c) => "No type arity " ^ Syntax.string_of_arity ctxt (a, [], [c])
     | No_Subsort (S1, S2) =>
        "Cannot derive subsort relation " ^
          Syntax.string_of_sort ctxt S1 ^ " < " ^ Syntax.string_of_sort ctxt S2
  end;

exception CLASS_ERROR of class_error;


(* instances *)

fun has_instance algebra a =
  forall (AList.defined (op =) (Symtab.lookup_list (arities_of algebra) a));

fun mg_domain algebra a S =
  let
    val ars = Symtab.lookup_list (arities_of algebra) a;
    fun dom c =
      (case AList.lookup (op =) ars c of
        NONE => raise CLASS_ERROR (No_Arity (a, c))
      | SOME Ss => Ss);
    fun dom_inter c Ss = ListPair.map (inter_sort algebra) (dom c, Ss);
  in
    (case S of
      [] => raise Fail "Unknown domain of empty intersection"
    | c :: cs => fold dom_inter cs (dom c))
  end;


(* meet_sort *)

fun meet_sort algebra =
  let
    fun inters S S' = inter_sort algebra (S, S');
    fun meet _ [] = I
      | meet (TFree (_, S)) S' =
          if sort_le algebra (S, S') then I
          else raise CLASS_ERROR (No_Subsort (S, S'))
      | meet (TVar (v, S)) S' =
          if sort_le algebra (S, S') then I
          else Vartab.map_default (v, S) (inters S')
      | meet (Type (a, Ts)) S = fold2 meet Ts (mg_domain algebra a S);
  in uncurry meet end;

fun meet_sort_typ algebra (T, S) =
  let val tab = meet_sort algebra (T, S) Vartab.empty;
  in Term.map_type_tvar (fn (v, _) => TVar (v, (the o Vartab.lookup tab) v)) end;


(* of_sort *)

fun of_sort algebra =
  let
    fun ofS (_, []) = true
      | ofS (TFree (_, S), S') = sort_le algebra (S, S')
      | ofS (TVar (_, S), S') = sort_le algebra (S, S')
      | ofS (Type (a, Ts), S) =
          let val Ss = mg_domain algebra a S in
            ListPair.all ofS (Ts, Ss)
          end handle CLASS_ERROR _ => false;
  in ofS end;


(* animating derivations *)

fun of_sort_derivation algebra {class_relation, type_constructor, type_variable} =
  let
    val arities = arities_of algebra;

    fun weaken T D1 S2 =
      let val S1 = map snd D1 in
        if S1 = S2 then map fst D1
        else
          S2 |> map (fn c2 =>
            (case D1 |> find_first (fn (_, c1) => class_le algebra (c1, c2)) of
              SOME d1 => class_relation T d1 c2
            | NONE => raise CLASS_ERROR (No_Subsort (S1, S2))))
      end;

    fun derive (_, []) = []
      | derive (Type (a, Us), S) =
          let
            val Ss = mg_domain algebra a S;
            val dom = map2 (fn U => fn S => derive (U, S) ~~ S) Us Ss;
          in
            S |> map (fn c =>
              let
                val Ss' = the (AList.lookup (op =) (Symtab.lookup_list arities a) c);
                val dom' = map (fn ((U, d), S') => weaken U d S' ~~ S') ((Us ~~ dom) ~~ Ss');
              in type_constructor (a, Us) dom' c end)
          end
      | derive (T, S) = weaken T (type_variable T) S;
  in derive end;

fun classrel_derivation algebra class_relation =
  let
    fun path (x, c1 :: c2 :: cs) = path (class_relation (x, c1) c2, c2 :: cs)
      | path (x, _) = x;
  in
    fn (x, c1) => fn c2 =>
      (case Graph.irreducible_paths (classes_of algebra) (c1, c2) of
        [] => raise CLASS_ERROR (No_Classrel (c1, c2))
      | cs :: _ => path (x, cs))
  end;


(* witness_sorts *)

fun witness_sorts algebra types hyps sorts =
  let
    fun le S1 S2 = sort_le algebra (S1, S2);
    fun get S2 (T, S1) = if le S1 S2 then SOME (T, S2) else NONE;
    fun mg_dom t S = SOME (mg_domain algebra t S) handle CLASS_ERROR _ => NONE;

    fun witn_sort _ [] solved_failed = (SOME (propT, []), solved_failed)
      | witn_sort path S (solved, failed) =
          if exists (le S) failed then (NONE, (solved, failed))
          else
            (case get_first (get S) solved of
              SOME w => (SOME w, (solved, failed))
            | NONE =>
                (case get_first (get S) hyps of
                  SOME w => (SOME w, (w :: solved, failed))
                | NONE => witn_types path types S (solved, failed)))

    and witn_sorts path x = fold_map (witn_sort path) x

    and witn_types _ [] S (solved, failed) = (NONE, (solved, S :: failed))
      | witn_types path (t :: ts) S solved_failed =
          (case mg_dom t S of
            SOME SS =>
              (*do not descend into stronger args (achieving termination)*)
              if exists (fn D => le D S orelse exists (le D) path) SS then
                witn_types path ts S solved_failed
              else
                let val (ws, (solved', failed')) = witn_sorts (S :: path) SS solved_failed in
                  if forall is_some ws then
                    let val w = (Type (t, map (#1 o the) ws), S)
                    in (SOME w, (w :: solved', failed')) end
                  else witn_types path ts S (solved', failed')
                end
          | NONE => witn_types path ts S solved_failed);

  in map_filter I (#1 (witn_sorts [] sorts ([], []))) end;

end;
