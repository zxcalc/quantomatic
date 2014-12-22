(*  Title:      Pure/pattern.ML
    Author:     Tobias Nipkow, Christine Heinzelmann, and Stefan Berghofer, TU Muenchen

Unification of Higher-Order Patterns.

See also:
Tobias Nipkow. Functional Unification of Higher-Order Patterns.
In Proceedings of the 8th IEEE Symposium Logic in Computer Science, 1993.

TODO: optimize red by special-casing it
*)

signature PATTERN =
sig
  val unify_trace_failure_default: bool Unsynchronized.ref
  val unify_trace_failure_raw: Config.raw
  val unify_trace_failure: bool Config.T
  val match: theory -> term * term -> Type.tyenv * Envir.tenv -> Type.tyenv * Envir.tenv
  val first_order_match: theory -> term * term
    -> Type.tyenv * Envir.tenv -> Type.tyenv * Envir.tenv
  val matches: theory -> term * term -> bool
  val matchess: theory -> term list * term list -> bool
  val equiv: theory -> term * term -> bool
  val matches_subterm: theory -> term * term -> bool
  val unify_types: theory -> typ * typ -> Envir.env -> Envir.env
  val unify: theory -> term * term -> Envir.env -> Envir.env
  val first_order: term -> bool
  val pattern: term -> bool
  val match_rew: theory -> term -> term * term -> (term * term) option
  val rewrite_term: theory -> (term * term) list -> (term -> term option) list -> term -> term
  val rewrite_term_top: theory -> (term * term) list -> (term -> term option) list -> term -> term
  exception Unif
  exception MATCH
  exception Pattern
end;

structure Pattern: PATTERN =
struct

exception Unif;
exception Pattern;

val unify_trace_failure_default = Unsynchronized.ref false;
val unify_trace_failure_raw =
  Config.declare_global ("unify_trace_failure", @{here})
    (fn _ => Config.Bool (! unify_trace_failure_default));
val unify_trace_failure = Config.bool unify_trace_failure_raw;

fun string_of_term thy env binders t =
  Syntax.string_of_term_global thy
    (Envir.norm_term env (subst_bounds (map Free binders, t)));

fun bname binders i = fst (nth binders i);
fun bnames binders is = space_implode " " (map (bname binders) is);

fun typ_clash thy (tye,T,U) =
  if Config.get_global thy unify_trace_failure
  then let val t = Syntax.string_of_typ_global thy (Envir.norm_type tye T)
           and u = Syntax.string_of_typ_global thy (Envir.norm_type tye U)
       in tracing("The following types do not unify:\n" ^ t ^ "\n" ^ u) end
  else ()

fun clash thy a b =
  if Config.get_global thy unify_trace_failure then tracing("Clash: " ^ a ^ " =/= " ^ b) else ()

fun boundVar binders i =
  "bound variable " ^ bname binders i ^ " (depth " ^ string_of_int i ^ ")";

fun clashBB thy binders i j =
  if Config.get_global thy unify_trace_failure
  then clash thy (boundVar binders i) (boundVar binders j)
  else ()

fun clashB thy binders i s =
  if Config.get_global thy unify_trace_failure
  then clash thy (boundVar binders i) s
  else ()

fun proj_fail thy (env,binders,F,_,is,t) =
  if Config.get_global thy unify_trace_failure
  then let val f = Term.string_of_vname F
           val xs = bnames binders is
           val u = string_of_term thy env binders t
           val ys = bnames binders (subtract (op =) is (loose_bnos t))
       in tracing("Cannot unify variable " ^ f ^
               " (depending on bound variables " ^ xs ^ ")\nwith term " ^ u ^
               "\nTerm contains additional bound variable(s) " ^ ys)
       end
  else ()

fun ocheck_fail thy (F,t,binders,env) =
  if Config.get_global thy unify_trace_failure
  then let val f = Term.string_of_vname F
           val u = string_of_term thy env binders t
       in tracing("Variable " ^ f ^ " occurs in term\n" ^ u ^
                  "\nCannot unify!\n")
       end
  else ()

fun occurs(F,t,env) =
    let fun occ(Var (G, T))   = (case Envir.lookup env (G, T) of
                                 SOME(t) => occ t
                               | NONE    => F=G)
          | occ(t1$t2)      = occ t1 orelse occ t2
          | occ(Abs(_,_,t)) = occ t
          | occ _           = false
    in occ t end;


fun mapbnd f =
    let fun mpb d (Bound(i))     = if i < d then Bound(i) else Bound(f(i-d)+d)
          | mpb d (Abs(s,T,t))   = Abs(s,T,mpb(d+1) t)
          | mpb d ((u1 $ u2))    = (mpb d u1)$(mpb d u2)
          | mpb _ atom           = atom
    in mpb 0 end;

fun idx [] j     = raise Unif
  | idx(i::is) j = if (i:int) =j then length is else idx is j;

fun mkabs (binders,is,t)  =
    let fun mk(i::is) = let val (x,T) = nth binders i
                        in Abs(x,T,mk is) end
          | mk []     = t
    in mk is end;

val incr = mapbnd (fn i => i+1);

fun ints_of []             = []
  | ints_of (Bound i ::bs) =
      let val is = ints_of bs
      in if member (op =) is i then raise Pattern else i::is end
  | ints_of _              = raise Pattern;

fun ints_of' env ts = ints_of (map (Envir.head_norm env) ts);


fun app (s,(i::is)) = app (s$Bound(i),is)
  | app (s,[])      = s;

fun red (Abs(_,_,s)) (i::is) js = red s is (i::js)
  | red t            []      [] = t
  | red t            is      jn = app (mapbnd (nth jn) t,is);


(* split_type ([T1,....,Tn]---> T,n,[]) = ([Tn,...,T1],T) *)
fun split_type (T,0,Ts)                    = (Ts,T)
  | split_type (Type ("fun",[T1,T2]),n,Ts) = split_type (T2,n-1,T1::Ts)
  | split_type _                           = raise Fail "split_type";

fun type_of_G env (T, n, is) =
  let
    val tyenv = Envir.type_env env;
    val (Ts, U) = split_type (Envir.norm_type tyenv T, n, []);
  in map (nth Ts) is ---> U end;

fun mk_hnf (binders,is,G,js) = mkabs (binders, is, app(G,js));

fun mk_new_hnf(env,binders,is,F as (a,_),T,js) =
  let val (env',G) = Envir.genvar a (env,type_of_G env (T,length is,js))
  in Envir.update ((F, T), mk_hnf (binders, is, G, js)) env' end;


(*predicate: downto0 (is, n) <=> is = [n, n - 1, ..., 0]*)
fun downto0 (i :: is, n) = i = n andalso downto0 (is, n - 1)
  | downto0 ([], n) = n = ~1;

(*mk_proj_list(is) = [ |is| - k | 1 <= k <= |is| and is[k] >= 0 ]*)
fun mk_proj_list is =
    let fun mk(i::is,j) = if is_some i then j :: mk(is,j-1) else mk(is,j-1)
          | mk([],_)    = []
    in mk(is,length is - 1) end;

fun proj(s,env,binders,is) =
    let fun trans d i = if i<d then i else (idx is (i-d))+d;
        fun pr(s,env,d,binders) = (case Envir.head_norm env s of
              Abs(a,T,t) => let val (t',env') = pr(t,env,d+1,((a,T)::binders))
                            in (Abs(a,T,t'),env') end
            | t => (case strip_comb t of
                (c as Const _,ts) =>
                         let val (ts',env') = prs(ts,env,d,binders)
                         in (list_comb(c,ts'),env') end
                 | (f as Free _,ts) =>
                         let val (ts',env') = prs(ts,env,d,binders)
                         in (list_comb(f,ts'),env') end
                 | (Bound(i),ts) =>
                         let val j = trans d i
                             val (ts',env') = prs(ts,env,d,binders)
                         in (list_comb(Bound j,ts'),env') end
                 | (Var(F as (a,_),Fty),ts) =>
                      let val js = ints_of' env ts;
                          val js' = map (try (trans d)) js;
                          val ks = mk_proj_list js';
                          val ls = map_filter I js'
                          val Hty = type_of_G env (Fty,length js,ks)
                          val (env',H) = Envir.genvar a (env,Hty)
                          val env'' =
                            Envir.update ((F, Fty), mk_hnf (binders, js, H, ks)) env'
                      in (app(H,ls),env'') end
                 | _  => raise Pattern))
        and prs(s::ss,env,d,binders) =
              let val (s',env1) = pr(s,env,d,binders)
                  val (ss',env2) = prs(ss,env1,d,binders)
              in (s'::ss',env2) end
          | prs([],env,_,_) = ([],env)
   in if downto0(is,length binders - 1) then (s,env)
      else pr(s,env,0,binders)
   end;


(* mk_ff_list(is,js) = [ length(is) - k | 1 <= k <= |is| and is[k] = js[k] ] *)
fun mk_ff_list(is,js) =
    let fun mk([],[],_)        = []
          | mk(i::is,j::js, k) = if (i:int) = j then k :: mk(is,js,k-1)
                                        else mk(is,js,k-1)
          | mk _               = raise Fail "mk_ff_list"
    in mk(is,js,length is-1) end;

fun flexflex1(env,binders,F,Fty,is,js) =
  if is=js then env
  else let val ks = mk_ff_list(is,js)
       in mk_new_hnf(env,binders,is,F,Fty,ks) end;

fun flexflex2(env,binders,F,Fty,is,G,Gty,js) =
  let fun ff(F,Fty,is,G as (a,_),Gty,js) =
            if subset (op =) (js, is)
            then let val t= mkabs(binders,is,app(Var(G,Gty),map (idx is) js))
                 in Envir.update ((F, Fty), t) env end
            else let val ks = inter (op =) js is
                     val Hty = type_of_G env (Fty,length is,map (idx is) ks)
                     val (env',H) = Envir.genvar a (env,Hty)
                     fun lam(is) = mkabs(binders,is,app(H,map (idx is) ks));
                 in Envir.update ((G, Gty), lam js) (Envir.update ((F, Fty), lam is) env')
                 end;
  in if Term_Ord.indexname_ord (G,F) = LESS then ff(F,Fty,is,G,Gty,js) else ff(G,Gty,js,F,Fty,is) end

fun unify_types thy (T, U) (env as Envir.Envir {maxidx, tenv, tyenv}) =
  if T = U then env
  else
    let val (tyenv', maxidx') = Sign.typ_unify thy (U, T) (tyenv, maxidx)
    in Envir.Envir {maxidx = maxidx', tenv = tenv, tyenv = tyenv'} end
    handle Type.TUNIFY => (typ_clash thy (tyenv, T, U); raise Unif);

fun unif thy binders (s,t) env = case (Envir.head_norm env s, Envir.head_norm env t) of
      (Abs(ns,Ts,ts),Abs(nt,Tt,tt)) =>
         let val name = if ns = "" then nt else ns
         in unif thy ((name,Ts)::binders) (ts,tt) (unify_types thy (Ts, Tt) env) end
    | (Abs(ns,Ts,ts),t) => unif thy ((ns,Ts)::binders) (ts,(incr t)$Bound(0)) env
    | (t,Abs(nt,Tt,tt)) => unif thy ((nt,Tt)::binders) ((incr t)$Bound(0),tt) env
    | p => cases thy (binders,env,p)

and cases thy (binders,env,(s,t)) = case (strip_comb s,strip_comb t) of
       ((Var(F,Fty),ss),(Var(G,Gty),ts)) =>
         if F = G then flexflex1(env,binders,F,Fty,ints_of' env ss,ints_of' env ts)
                  else flexflex2(env,binders,F,Fty,ints_of' env ss,G,Gty,ints_of' env ts)
      | ((Var(F,Fty),ss),_)           => flexrigid thy (env,binders,F,Fty,ints_of' env ss,t)
      | (_,(Var(F,Fty),ts))           => flexrigid thy (env,binders,F,Fty,ints_of' env ts,s)
      | ((Const c,ss),(Const d,ts))   => rigidrigid thy (env,binders,c,d,ss,ts)
      | ((Free(f),ss),(Free(g),ts))   => rigidrigid thy (env,binders,f,g,ss,ts)
      | ((Bound(i),ss),(Bound(j),ts)) => rigidrigidB thy (env,binders,i,j,ss,ts)
      | ((Abs(_),_),_)                => raise Pattern
      | (_,(Abs(_),_))                => raise Pattern
      | ((Const(c,_),_),(Free(f,_),_)) => (clash thy c f; raise Unif)
      | ((Const(c,_),_),(Bound i,_))   => (clashB thy binders i c; raise Unif)
      | ((Free(f,_),_),(Const(c,_),_)) => (clash thy f c; raise Unif)
      | ((Free(f,_),_),(Bound i,_))    => (clashB thy binders i f; raise Unif)
      | ((Bound i,_),(Const(c,_),_))   => (clashB thy binders i c; raise Unif)
      | ((Bound i,_),(Free(f,_),_))    => (clashB thy binders i f; raise Unif)


and rigidrigid thy (env,binders,(a,Ta),(b,Tb),ss,ts) =
      if a<>b then (clash thy a b; raise Unif)
      else env |> unify_types thy (Ta,Tb) |> fold (unif thy binders) (ss~~ts)

and rigidrigidB thy (env,binders,i,j,ss,ts) =
     if i <> j then (clashBB thy binders i j; raise Unif)
     else fold (unif thy binders) (ss~~ts) env

and flexrigid thy (params as (env,binders,F,Fty,is,t)) =
      if occurs(F,t,env) then (ocheck_fail thy (F,t,binders,env); raise Unif)
      else (let val (u,env') = proj(t,env,binders,is)
            in Envir.update ((F, Fty), mkabs (binders, is, u)) env' end
            handle Unif => (proj_fail thy params; raise Unif));

fun unify thy = unif thy [];



(*** Matching ***)

exception MATCH;

fun typ_match thy TU tyenv = Sign.typ_match thy TU tyenv
  handle Type.TYPE_MATCH => raise MATCH;

(*First-order matching;
  The pattern and object may have variables in common.
  Instantiation does not affect the object, so matching ?a with ?a+1 works.
  Object is eta-contracted on the fly (by eta-expanding the pattern).
  Precondition: the pattern is already eta-contracted!
  Types are matched on the fly*)
fun first_order_match thy =
  let
    fun mtch k (instsp as (tyinsts,insts)) = fn
        (Var(ixn,T), t)  =>
          if k > 0 andalso Term.is_open t then raise MATCH
          else (case Envir.lookup1 insts (ixn, T) of
                  NONE => (typ_match thy (T, fastype_of t) tyinsts,
                           Vartab.update_new (ixn, (T, t)) insts)
                | SOME u => if Envir.aeconv (t, u) then instsp else raise MATCH)
      | (Free (a,T), Free (b,U)) =>
          if a=b then (typ_match thy (T,U) tyinsts, insts) else raise MATCH
      | (Const (a,T), Const (b,U))  =>
          if a=b then (typ_match thy (T,U) tyinsts, insts) else raise MATCH
      | (Bound i, Bound j)  =>  if  i=j  then  instsp  else raise MATCH
      | (Abs(_,T,t), Abs(_,U,u))  =>
          mtch (k + 1) (typ_match thy (T,U) tyinsts, insts) (t,u)
      | (f$t, g$u) => mtch k (mtch k instsp (f,g)) (t, u)
      | (t, Abs(_,U,u))  =>  mtch (k + 1) instsp ((incr t)$(Bound 0), u)
      | _ => raise MATCH
  in fn tu => fn env => mtch 0 env tu end;


(* Matching of higher-order patterns *)

fun match_bind(itms,binders,ixn,T,is,t) =
  let val js = loose_bnos t
  in if null is
     then if null js then Vartab.update_new (ixn, (T, t)) itms else raise MATCH
     else if subset (op =) (js, is)
          then let val t' = if downto0(is,length binders - 1) then t
                            else mapbnd (idx is) t
               in Vartab.update_new (ixn, (T, mkabs (binders, is, t'))) itms end
          else raise MATCH
  end;

fun match thy (po as (pat,obj)) envir =
let
  (* Pre: pat and obj have same type *)
  fun mtch binders (pat,obj) (env as (iTs,itms)) =
    case pat of
      Abs(ns,Ts,ts) =>
        (case obj of
           Abs(nt,Tt,tt) => mtch ((nt,Tt)::binders) (ts,tt) env
         | _ => let val Tt = Envir.subst_type iTs Ts
                in mtch((ns,Tt)::binders) (ts,(incr obj)$Bound(0)) env end)
    | _ => (case obj of
              Abs(nt,Tt,tt) =>
                mtch((nt,Tt)::binders) ((incr pat)$Bound(0),tt) env
            | _ => cases(binders,env,pat,obj))

  and cases(binders,env as (iTs,itms),pat,obj) =
    let val (ph,pargs) = strip_comb pat
        fun rigrig1(iTs,oargs) = fold (mtch binders) (pargs~~oargs) (iTs,itms)
          handle ListPair.UnequalLengths => raise MATCH
        fun rigrig2((a:string,Ta),(b,Tb),oargs) =
              if a <> b then raise MATCH
              else rigrig1(typ_match thy (Ta,Tb) iTs, oargs)
    in case ph of
         Var(ixn,T) =>
           let val is = ints_of pargs
           in case Envir.lookup1 itms (ixn, T) of
                NONE => (iTs,match_bind(itms,binders,ixn,T,is,obj))
              | SOME u => if Envir.aeconv (obj, red u is []) then env
                          else raise MATCH
           end
       | _ =>
           let val (oh,oargs) = strip_comb obj
           in case (ph,oh) of
                (Const c,Const d) => rigrig2(c,d,oargs)
              | (Free f,Free g)   => rigrig2(f,g,oargs)
              | (Bound i,Bound j) => if i<>j then raise MATCH
                                     else rigrig1(iTs,oargs)
              | (Abs _, _)        => raise Pattern
              | (_, Abs _)        => raise Pattern
              | _                 => raise MATCH
           end
    end;

  val pT = fastype_of pat
  and oT = fastype_of obj
  val envir' = apfst (typ_match thy (pT, oT)) envir;
in mtch [] po envir' handle Pattern => first_order_match thy po envir' end;

fun matches thy po = (match thy po (Vartab.empty, Vartab.empty); true) handle MATCH => false;

fun matchess thy (ps, os) =
  length ps = length os andalso
    ((fold (match thy) (ps ~~ os) (Vartab.empty, Vartab.empty); true) handle MATCH => false);

fun equiv thy (t, u) = matches thy (t, u) andalso matches thy (u, t);


(* Does pat match a subterm of obj? *)
fun matches_subterm thy (pat, obj) =
  let
    fun msub bounds obj = matches thy (pat, obj) orelse
      (case obj of
        Abs (x, T, t) => msub (bounds + 1) (snd (Term.dest_abs (Name.bound bounds, T, t)))
      | t $ u => msub bounds t orelse msub bounds u
      | _ => false)
  in msub 0 obj end;

fun first_order(Abs(_,_,t)) = first_order t
  | first_order(t $ u) = first_order t andalso first_order u andalso
                         not(is_Var t)
  | first_order _ = true;

fun pattern (Abs (_, _, t)) = pattern t
  | pattern t =
      let val (head, args) = strip_comb t in
        if is_Var head then
          forall is_Bound args andalso not (has_duplicates (op aconv) args)
        else forall pattern args
      end;


(* rewriting -- simple but fast *)

fun match_rew thy tm (tm1, tm2) =
  let val rtm = the_default tm2 (Term.rename_abs tm1 tm tm2) in
    SOME (Envir.subst_term (match thy (tm1, tm) (Vartab.empty, Vartab.empty)) rtm, rtm)
      handle MATCH => NONE
  end;

fun gen_rewrite_term bot thy rules procs tm =
  let
    val skel0 = Bound 0;

    fun variant_absfree bounds (x, T, t) =
      let
        val (x', t') = Term.dest_abs (Name.bound bounds, T, t);
        fun abs u = Abs (x, T, abstract_over (Free (x', T), u));
      in (abs, t') end;

    fun rew (Abs (_, _, body) $ t) = SOME (subst_bound (t, body), skel0)
      | rew tm =
          (case get_first (match_rew thy tm) rules of
            NONE => Option.map (rpair skel0) (get_first (fn p => p tm) procs)
          | x => x);

    fun rew_sub r bounds skel (tm1 $ tm2) = (case tm1 of
            Abs (_, _, body) =>
              let val tm' = subst_bound (tm2, body)
              in SOME (the_default tm' (rew_sub r bounds skel0 tm')) end
          | _ =>
            let val (skel1, skel2) = (case skel of
                skel1 $ skel2 => (skel1, skel2)
              | _ => (skel0, skel0))
            in case r bounds skel1 tm1 of
                SOME tm1' => (case r bounds skel2 tm2 of
                    SOME tm2' => SOME (tm1' $ tm2')
                  | NONE => SOME (tm1' $ tm2))
              | NONE => (case r bounds skel2 tm2 of
                    SOME tm2' => SOME (tm1 $ tm2')
                  | NONE => NONE)
            end)
      | rew_sub r bounds skel (Abs body) =
          let
            val (abs, tm') = variant_absfree bounds body;
            val skel' = (case skel of Abs (_, _, skel') => skel' | _ => skel0)
          in case r (bounds + 1) skel' tm' of
              SOME tm'' => SOME (abs tm'')
            | NONE => NONE
          end
      | rew_sub _ _ _ _ = NONE;

    fun rew_bot bounds (Var _) _ = NONE
      | rew_bot bounds skel tm = (case rew_sub rew_bot bounds skel tm of
          SOME tm1 => (case rew tm1 of
              SOME (tm2, skel') => SOME (the_default tm2 (rew_bot bounds skel' tm2))
            | NONE => SOME tm1)
        | NONE => (case rew tm of
              SOME (tm1, skel') => SOME (the_default tm1 (rew_bot bounds skel' tm1))
            | NONE => NONE));

    fun rew_top bounds _ tm = (case rew tm of
          SOME (tm1, _) => (case rew_sub rew_top bounds skel0 tm1 of
              SOME tm2 => SOME (the_default tm2 (rew_top bounds skel0 tm2))
            | NONE => SOME tm1)
        | NONE => (case rew_sub rew_top bounds skel0 tm of
              SOME tm1 => SOME (the_default tm1 (rew_top bounds skel0 tm1))
            | NONE => NONE));

  in the_default tm ((if bot then rew_bot else rew_top) 0 skel0 tm) end;

val rewrite_term = gen_rewrite_term true;
val rewrite_term_top = gen_rewrite_term false;

end;

