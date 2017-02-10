(*  Title:      Pure/ML_Bootstrap.thy
    Author:     Makarius

ML bootstrap environment -- with access to low-level structures!
*)

theory ML_Bootstrap
imports Pure
begin

subsection \<open>Standard ML environment for virtual bootstrap\<close>

setup \<open>Context.theory_map ML_Env.init_bootstrap\<close>

SML_import \<open>
  structure Output_Primitives = Output_Primitives_Virtual;
  structure Thread_Data = Thread_Data_Virtual;
  fun ML_system_pp (_: FixedInt.int -> 'a -> 'b -> PolyML_Pretty.pretty) = ();
\<close>


subsection \<open>Final setup of global ML environment\<close>

ML \<open>Proofterm.proofs := 0\<close>

ML \<open>
  Context.setmp_generic_context NONE
    ML \<open>
      List.app ML_Name_Space.forget_structure ML_Name_Space.hidden_structures;
      structure PolyML = struct structure IntInf = PolyML.IntInf end;
    \<close>
\<close>

ML \<open>@{assert} (not (can ML \<open>open RunCall\<close>))\<close>


subsection \<open>Switch to bootstrap environment\<close>

setup \<open>Config.put_global ML_Env.SML_environment true\<close>

end
