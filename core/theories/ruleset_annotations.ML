(**
 * Annotations on a ruleset
 *)
signature RULESET_ANNOTATIONS =
sig
  structure GraphAnnotations : GRAPH_ANNOTATIONS;
  (* The annotation structure *)
  type T;
  type data = GraphAnnotations.data;

  (* With no annotations *)
  val init: T;

  val get_ruleset_annotation : T -> data;
  val get_rule_annotation : T -> R.name -> data;
  val get_rule_lhs_annotation : T -> R.name -> GraphAnnotations.T;
  val get_rule_rhs_annotation : T -> R.name -> GraphAnnotations.T;

  val get_rule_annotation_tab : T -> (data*(GraphAnnotations.T*GraphAnnotations.T)) R.NTab.T;

  val set_ruleset_annotation : data -> T -> T;
  val set_rule_annotation : R.name -> data -> T -> T;
  val set_rule_lhs_annotation : R.name -> GraphAnnotations.T -> T -> T;
  val set_rule_rhs_annotation : R.name -> GraphAnnotations.T -> T -> T;

  val update_ruleset_annotation : (data -> data) -> T -> T;
  val update_rule_annotation : R.name -> (data -> data) -> T -> T;
  val update_rule_lhs_annotation : R.name -> (GraphAnnotations.T -> GraphAnnotations.T) -> T -> T;
  val update_rule_rhs_annotation : R.name -> (GraphAnnotations.T -> GraphAnnotations.T) -> T -> T;

  (* first overwrites second *)
  val merge : T -> T -> T;

  val remove_rule_annotation : R.name -> T -> T;
  val rename_rule : R.name -> R.name -> T -> T;

  (* Arg1 is rules to retain *)
  val cleanup : R.NSet.T -> T -> T;

  val pretty : T -> Pretty.T;
  val print : T -> unit;
  val pretty_data : data -> Pretty.T;
  val print_data : data -> unit;
end;

functor RulesetAnnotations(
  structure GraphAnnotations : GRAPH_ANNOTATIONS
) : RULESET_ANNOTATIONS =
struct
  structure GraphAnnotations = GraphAnnotations;
  type data = GraphAnnotations.data;
  structure GA = GraphAnnotations;
  type T = (data * ((data * (GA.T * GA.T)) R.NTab.T));

  val init = (GA.empty_data,R.NTab.empty);

  fun get_ruleset_annotation (rsa,_) = rsa;
  fun get_rule_annotation (_,ratab) r =
    case R.NTab.lookup ratab r
      of SOME (ra,_) => ra
       | NONE => GA.empty_data;
  fun get_rule_lhs_annotation (_,ratab) r =
    case R.NTab.lookup ratab r
      of SOME (_,(ga,_)) => ga
       | NONE => GA.init;
  fun get_rule_rhs_annotation (_,ratab) r =
    case R.NTab.lookup ratab r
      of SOME (_,(_,ga)) => ga
       | NONE => GA.init;

  fun get_rule_annotation_tab (_,ratab) = ratab;

  fun set_ruleset_annotation d (_,ratab) = (d,ratab);
  fun set_rule_annotation r d (rsa,ratab) =
    (rsa,
     R.NTab.map_entry (fn (_,gas) => (d,gas)) r ratab
     handle R.NTab.Tab.UNDEF _ =>
       R.NTab.update_new (r,(d,(GA.init,GA.init))) ratab);
  fun set_rule_lhs_annotation r ann (rsa,ratab) =
    (rsa,
     R.NTab.map_entry (fn (ra,(_,rhs)) => (ra,(ann,rhs))) r ratab
     handle R.NTab.Tab.UNDEF _ =>
       R.NTab.update_new (r,(GA.empty_data,(ann,GA.init))) ratab);
  fun set_rule_rhs_annotation r ann (rsa,ratab) =
    (rsa,
     R.NTab.map_entry (fn (ra,(lhs,_)) => (ra,(lhs,ann))) r ratab
     handle R.NTab.Tab.UNDEF _ =>
       R.NTab.update_new (r,(GA.empty_data,(GA.init,ann))) ratab);

  val update_ruleset_annotation = apfst;
  fun update_rule_annotation r f (rsa,rtab) =
    (rsa,
     R.NTab.map_entry (fn (rann,gas) => (f rann,gas)) r rtab
     handle R.NTab.Tab.UNDEF _ =>
       R.NTab.update_new (r,(f GA.empty_data,(GA.init,GA.init))) rtab);
  fun update_rule_lhs_annotation r f (rsa,rtab) =
    (rsa,
     R.NTab.map_entry (fn (rann,(lhs,rhs)) => (rann,(f lhs,rhs))) r rtab
     handle R.NTab.Tab.UNDEF _ =>
       R.NTab.update_new (r,(GA.empty_data,(f GA.init,GA.init))) rtab);
  fun update_rule_rhs_annotation r f (rsa,rtab) =
    (rsa,
     R.NTab.map_entry (fn (rann,(lhs,rhs)) => (rann,(lhs,f rhs))) r rtab
     handle R.NTab.Tab.UNDEF _ =>
       R.NTab.update_new (r,(GA.empty_data,(GA.init,f GA.init))) rtab);

  fun merge (rsa,rtab1) (_,rtab2) =
    (rsa, R.NTab.merge_joint (K true) rtab2 rtab1);
  fun remove_rule_annotation r (rsa,rtab) = (rsa, R.NTab.delete r rtab);
  fun rename_rule old new (rsa,rtab) =
    case R.NTab.try_rename1 old new rtab
      of SOME rtab' => (rsa,rtab')
       | NONE =>
           let
             val rtab1 = R.NTab.delete new rtab
             val rtab2 = the (R.NTab.try_rename1 old new rtab1)
           in
             (rsa,rtab2)
           end;

  fun cleanup rset (ras,rtab) =
    (ras, R.NTab.fold
          (fn (k,_) => if R.NSet.contains rset k then I else R.NTab.delete k)
          rtab rtab);

  val pretty_data = GraphAnnotations.pretty_data;
  val print_data = GraphAnnotations.print_data;
  fun pretty_rule (ran,(lhs,rhs)) =
    Pretty.chunks [
      Pretty.block [
        Pretty.str "Rule Annotation: ",
        pretty_data ran
      ],
      Pretty.block [
        Pretty.str "LHS Annotations: ",
        GraphAnnotations.pretty lhs
      ],
      Pretty.block [
        Pretty.str "RHS Annotations: ",
        GraphAnnotations.pretty rhs
      ]
    ];
  fun pretty ann =
    Pretty.chunks
      [Pretty.str "Ruleset Annotations {",
       Pretty.block
        [Pretty.str " ",
         Pretty.chunks
         [Pretty.block
          [Pretty.str "Ruleset Annotation: ",
           pretty_data (get_ruleset_annotation ann)],
          Pretty.block
          [Pretty.str "Rule Annotations: ",
          R.NTab.pretty pretty_rule (get_rule_annotation_tab ann)]]],
       Pretty.str "}"];
  val print = Pretty.writeln o pretty;
end;

(**
 * Table of annotations on a ruleset
 *
 * Each component has an associated table of strings.
 *)
signature RULESET_STRING_TABLE_ANNOTATIONS =
sig
  include RULESET_ANNOTATIONS where type data = string Symtab.table;

  val lookup_ruleset_property : T -> string -> string option;
  val lookup_rule_property : T -> R.name -> string -> string option;

  val set_ruleset_property : (string * string) -> T -> T;
  val set_rule_property : R.name -> (string * string) -> T -> T;

  val remove_ruleset_property : string -> T -> T;
  val remove_rule_property : R.name -> string -> T -> T;
end;

structure RulesetStringTableAnnotations : RULESET_STRING_TABLE_ANNOTATIONS =
struct
  structure Ann = RulesetAnnotations(
    structure GraphAnnotations = GraphStringTableAnnotations
  );
  open Ann;

  val lookup_ruleset_property = Symtab.lookup o Ann.get_ruleset_annotation;
  val lookup_rule_property = Symtab.lookup oo Ann.get_rule_annotation;

  val set_ruleset_property = Ann.update_ruleset_annotation o Symtab.update;
  fun set_rule_property r = (Ann.update_rule_annotation r) o Symtab.update;

  val remove_ruleset_property = Ann.update_ruleset_annotation o Symtab.delete;
  fun remove_rule_property r = (Ann.update_rule_annotation r) o Symtab.delete;
end;

