(*  Title:      Pure/Tools/named_thms.ML
    Author:     Makarius

Named collections of theorems in canonical order.
*)

signature NAMED_THMS =
sig
  val member: Proof.context -> thm -> bool
  val get: Proof.context -> thm list
  val add_thm: thm -> Context.generic -> Context.generic
  val del_thm: thm -> Context.generic -> Context.generic
  val add: attribute
  val del: attribute
  val setup: theory -> theory
end;

functor Named_Thms(val name: binding val description: string): NAMED_THMS =
struct

structure Data = Generic_Data
(
  type T = thm Item_Net.T;
  val empty = Thm.full_rules;
  val extend = I;
  val merge = Item_Net.merge;
);

val member = Item_Net.member o Data.get o Context.Proof;

val content = Item_Net.content o Data.get;
val get = content o Context.Proof;

val add_thm = Data.map o Item_Net.update;
val del_thm = Data.map o Item_Net.remove;

val add = Thm.declaration_attribute add_thm;
val del = Thm.declaration_attribute del_thm;

val setup =
  Attrib.setup name (Attrib.add_del add del) ("declaration of " ^ description) #>
  Global_Theory.add_thms_dynamic (name, content);

end;
