(*  Title:      Pure/more_unify.ML
    Author:     Makarius

Add-ons to Higher-Order Unification, outside the inference kernel.
*)

signature UNIFY =
sig
  include UNIFY
  val matchers: Context.generic -> (term * term) list -> Envir.env Seq.seq
  val matches_list: Context.generic -> term list -> term list -> bool
end;

structure Unify: UNIFY =
struct

(*Pattern matching*)
fun first_order_matchers thy pairs (Envir.Envir {maxidx, tenv, tyenv}) =
  let val (tyenv', tenv') = fold (Pattern.first_order_match thy) pairs (tyenv, tenv)
  in Seq.single (Envir.Envir {maxidx = maxidx, tenv = tenv', tyenv = tyenv'}) end
  handle Pattern.MATCH => Seq.empty;

(*General matching -- keep variables disjoint*)
fun matchers _ [] = Seq.single (Envir.empty ~1)
  | matchers context pairs =
      let
        val thy = Context.theory_of context;

        val maxidx = fold (Term.maxidx_term o #2) pairs ~1;
        val offset = maxidx + 1;
        val pairs' = map (apfst (Logic.incr_indexes ([], [], offset))) pairs;
        val maxidx' = fold (fn (t, u) => Term.maxidx_term t #> Term.maxidx_term u) pairs' ~1;

        val pat_tvars = fold (Term.add_tvars o #1) pairs' [];
        val pat_vars = fold (Term.add_vars o #1) pairs' [];

        val decr_indexesT =
          Term.map_atyps (fn T as TVar ((x, i), S) =>
            if i > maxidx then TVar ((x, i - offset), S) else T | T => T);
        val decr_indexes =
          Term.map_types decr_indexesT #>
          Term.map_aterms (fn t as Var ((x, i), T) =>
            if i > maxidx then Var ((x, i - offset), T) else t | t => t);

        fun norm_tvar env ((x, i), S) =
          let
            val tyenv = Envir.type_env env;
            val T' = Envir.norm_type tyenv (TVar ((x, i), S));
          in
            if (case T' of TVar (v, _) => v = (x, i) | _ => false) then NONE
            else SOME ((x, i - offset), (S, decr_indexesT T'))
          end;

        fun norm_var env ((x, i), T) =
          let
            val tyenv = Envir.type_env env;
            val T' = Envir.norm_type tyenv T;
            val t' = Envir.norm_term env (Var ((x, i), T'));
          in
            if (case t' of Var (v, _) => v = (x, i) | _ => false) then NONE
            else SOME ((x, i - offset), (decr_indexesT T', decr_indexes t'))
          end;

        fun result env =
          if Envir.above env maxidx then   (* FIXME proper handling of generated vars!? *)
            SOME (Envir.Envir {maxidx = maxidx,
              tyenv = Vartab.make (map_filter (norm_tvar env) pat_tvars),
              tenv = Vartab.make (map_filter (norm_var env) pat_vars)})
          else NONE;

        val empty = Envir.empty maxidx';
      in
        Seq.append
          (Seq.map_filter result (Unify.smash_unifiers context pairs' empty))
          (first_order_matchers thy pairs empty)
      end;

fun matches_list context ps os =
  length ps = length os andalso is_some (Seq.pull (matchers context (ps ~~ os)));


open Unify;

end;

