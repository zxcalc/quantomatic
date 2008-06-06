(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      gproof/tools/fnametab.ML
    Author:     Lucas Dixon, University of Edinburgh
                lucas.dixon@ed.ac.uk
                Initially written Oct 2005, Updated 10 Mar 2006        *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
    Table for holding things associated with nice fresh namers. 

    Each entry has a name - but we allow some names not to have
    entries: you can use up names without providing entries.

*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature FNAME_TAB =
sig

  type 'a T
  type name

  structure N : FNAMER sharing type N.name = name;
  structure ATab : TABLE sharing type ATab.key = name;

  val empty : 'a T
  val is_name_empty : 'a T -> bool 
  val is_element_empty : 'a T -> bool (* this implies is_name_empty *)

  val is_name_singleton : 'a T -> bool 
  val is_element_singleton : 'a T -> bool (* this implies is_name_singleton *)
  val tryget_singleton : 'a T -> (name * 'a) option

  val try_ins : (name * 'a) -> 'a T -> 'a T option
  val ins : (name * 'a) -> 'a T -> 'a T (* raises on duplicate *)
  val add : (name * 'a) -> 'a T -> (name * 'a T)
  val doadd : (name * 'a) -> 'a T -> 'a T (* add with new name, don't tell me *) 

  (* create new name and use it up: update must be used to set entry's data *)
  val new_name : name -> 'a T -> (name * 'a T)
  (* just use-up a list of names *)
  (* IMPROVE: avoid conversion to lists as uses up lots of memory:
     have a function in FNAMETAB to useup names from a name collection *)
  val useup_names : N.T -> 'a T -> 'a T

  val lookup : 'a T -> name -> 'a option
  val contains : 'a T -> name -> bool
  val contains_name : 'a T -> name -> bool (* some names may be used 
                                              without having entries *)

  val get : 'a T -> name -> 'a (* raises ATab.UNDEF *)
  val next_smaller : 'a T -> name -> (name * 'a) option
  val next_bigger : 'a T -> name -> (name * 'a) option
  val pull_elem : 'a T -> ((name * 'a) * 'a T) option
  val map_entry : ('a -> 'a) -> name -> 'a T -> 'a T
  val map_all : ('a -> 'b) -> 'a T -> 'b T
  val fold : ((name * 'a) -> 'b -> 'b) -> 'a T -> 'b -> 'b
  val update : (name * 'a) -> 'a T -> 'a T  (* insert, replacing old name *)
  val update_new : (name * 'a) -> 'a T -> 'a T 

  val delete : name -> 'a T -> 'a T

  val merge_disjoint : 'a T -> 'a T -> 'a T (* note: must be disjoint: raises ATab.DUP *)
  val merge_joint : (* exception on clash noneq elements *)
      ('a * 'a -> bool) (* return true if they are same (i.e. good) *)
      -> 'a T -> 'a T -> 'a T 

  (* values that are in the first but not the second *)
  val difference : 'a T -> 'a T -> 'a T 

  val dest : 'a T -> (name * 'a) list
  val keys : 'a T -> name list
  val values : 'a T -> 'a list
  val get_names : 'a T -> N.T;

  val ord : name * name -> order
  val name_eq : name * name -> bool
  val string_of_name : name -> string
  val pretty_name : name -> Pretty.T
  val pretty_ntree: 'a T -> Pretty.T
  val pretty : ('a -> Pretty.T) -> 'a T -> Pretty.T
  val print : ('a -> Pretty.T) -> 'a T -> unit
(*   val direct_pretty : 'a T -> Pretty.T
  val direct_print : 'a T -> unit *)
end;


functor FNameTabFun(N : FNAMER)
: FNAME_TAB 
= struct

  structure N = N;

  type name = N.N.name;

  val pretty_name  = N.N.pretty

(*   fun pretty_name (n,i) = 
      Pretty.block [N.pretty_name n, Pretty.str (Int.toString i)]; *)
  val string_of_name = Pretty.string_of o pretty_name;

  fun ord (a, b) = N.N.ord (a,b);

  fun name_eq (a, b) = (ord (a,b) = EQUAL);

(*       (Library.prod_ord N.ord Library.int_ord) (a,b); *)

  structure ATab = TableFun(type key = name val ord = ord );

  datatype 'a T = 
           Tab of {names : N.T,
                   tab : 'a ATab.table}

  fun get_tab (Tab rep) = #tab rep;
  fun get_names (Tab rep) = #names rep;

  val empty = Tab {names = N.empty,
                   tab = ATab.empty};

  fun is_name_empty (Tab {names, tab}) = N.is_empty names;
  fun is_element_empty (Tab {names, tab}) = ATab.is_empty tab;

  fun is_name_singleton (Tab {names, tab}) = N.is_singleton names;
  (* IMPROVE: could add this to table library *)
  fun tryget_singleton (Tab {names, tab}) = 
      (case N.tryget_singleton names 
        of NONE => NONE
         | SOME n => 
           (case ATab.lookup tab n of SOME x => SOME (n,x) 
                                    | NONE => NONE));
  fun is_element_singleton ntab = 
      (case tryget_singleton ntab of NONE => false | SOME _ => true);

  fun try_ins (x as (n,a)) (Tab rep) = 
      let val names = (#names rep) in
        case N.lookup names n
         of NONE => SOME (Tab{ names = N.add n names,
                               tab = ATab.update_new x (#tab rep) })
          | SOME _ => NONE
      end;

  (* N.ins_fresh and ATab.update_new raise exceptions on duplicates *) 
  fun ins (x as (n,a)) (Tab rep) = 
      (Tab{ names = N.ins_fresh n (#names rep),
            tab = ATab.update_new x (#tab rep) });

  fun map_entry f n (Tab rep) = 
      Tab{ names = #names rep,
           tab = ATab.map_entry n f (#tab rep) };

  fun map_all f (Tab rep) = 
      Tab{ names = #names rep,
           tab = ATab.map f (#tab rep) };

  fun fold f (Tab rep) = 
      ATab.fold f (#tab rep);

  fun update (x as (n,a)) (Tab rep) = 
      Tab{ names = N.add n (#names rep),
           tab = ATab.update x (#tab rep) };

  fun update_new (x as (n,a)) (Tab rep) = 
      Tab{ names = N.add n (#names rep),
           tab = ATab.update_new x (#tab rep) };

  fun delete n (Tab rep) = 
      Tab{ names = N.delete n (#names rep),
           tab = ATab.delete n (#tab rep) };

  fun add (x as (n,a)) (Tab rep) =
      let val names = #names rep in
        case N.lookup names n
         of NONE => (n, Tab{ names = N.add n names,
                             tab = ATab.update_new x (#tab rep) })
          | SOME (_,t) => 
            let val n' = N.N.suc t in
              (n', Tab{ names = N.add n' names,
                        tab = ATab.update_new (n',a) (#tab rep) })
            end
      end;

  fun doadd x = snd o add x;

  fun new_name n (Tab rep) = 
      let val (n2,names2) = N.add_new n (#names rep) in 
        (n2, Tab{ names = names2, tab = #tab rep })
      end;

  fun useup_names names (Tab rep) = 
      Tab{ names = N.union_merge names (#names rep),
           tab = #tab rep};

  fun lookup (Tab rep) n =
      ATab.lookup (#tab rep) n;

  (* implicit name argument *)
  fun contains_name (Tab rep) = N.contains (#names rep);

  fun contains (Tab rep) n =
      (case ATab.lookup (#tab rep) n of NONE => false 
                                      | _ => true);

  fun get (Tab rep) n =
      case ATab.lookup (#tab rep) n 
       of NONE => 
          raise ATab.UNDEF n
        | SOME x => x;

  (* FIXME & IMPROVE: may raise an exception if tab contains used
  names without entries: fix by adding a get_smaller to table.ML *)
  fun next_smaller tab n = 
      (case N.next_smaller (get_names tab) n 
        of NONE => NONE
         | SOME n2 => SOME (n2,get tab n2));
  fun next_bigger tab n = 
      (case N.next_bigger (get_names tab) n 
        of NONE => NONE
         | SOME n2 => SOME (n2,get tab n2));

(* assumes names = content *)
  fun pull_elem (tab as Tab rep) = 
      (case N.pull_local_bot (#names rep) of 
         NONE => NONE 
       | SOME (n,names2) => 
         SOME ((n,get tab n), 
               Tab {names = names2, tab = ATab.delete n (#tab rep)}));

  (* getting all entries... *)
  fun dest (Tab rep) = ATab.fold cons (#tab rep) [];
  fun keys (Tab rep) = ATab.fold (cons o fst) (#tab rep) [];
  fun values (Tab rep) = ATab.fold (cons o snd) (#tab rep) [];

  (* ? more efficient way? *)
  fun difference tab1 tab2 = 
      Basics.fold (fn k => if contains tab2 k then delete k else I) 
           (keys tab1) tab1;

  (* merging *)
  fun merge_disjoint t1 t2 =
      ATab.fold
        (fn (v as (n,a)) => fn t =>
            case try_ins v t
             of NONE => raise ATab.DUP n
              | SOME t2 => t2)
        (get_tab t1) t2;

  fun merge_joint chkf t1 t2 =
      ATab.fold
        (fn (v as (n,a)) => fn t => 
            case lookup t n
             of NONE => update_new (n,a) t
              | SOME a2 => 
                if chkf (a,a2) then t else 
                raise ATab.DUP n)
        (get_tab t1) t2;

  (* pretty printing *)
  fun pretty f t = 
      Pretty.chunks
      [Pretty.str "Name Mappings: ",
       Pretty.chunks 
         (map 
            (fn (k,v) => 
                Pretty.block 
                  [ pretty_name k, 
                    Pretty.str " => ",
                    f v ])
            (ATab.dest (get_tab t)))];

  fun pretty_ntree t = 
      Pretty.chunks
      [Pretty.str "Names: ",
       N.pretty (get_names t)];

  fun print pf = Pretty.writeln o (pretty pf)

(* 
  fun direct_pretty x = pretty (Pretty.str o PolyML.makestring) x;
  fun direct_print x = Pretty.writeln (direct_pretty x)
*)
  

end;


structure IntNameTab = FNameTabFun(IntFNamer);
structure StrNameTab = FNameTabFun(StrFNamer);
structure StrIntNameTab = FNameTabFun(StrIntFNamer);

(* 
install_pp (make_pp ["StrNameTab", "'a T"] 
                    (Pretty.pprint o StrNameTab.direct_pretty));
install_pp (make_pp ["StrIntNameTab", "'a T"] 
                    (Pretty.pprint o StrIntNameTab.direct_pretty));
*)
