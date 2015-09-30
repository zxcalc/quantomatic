(*  Title:      Pure/tactical.ML
    Author:     Lawrence C Paulson, Cambridge University Computer Laboratory

Tacticals.
*)

infix 1 THEN THEN' THEN_ALL_NEW;
infix 0 ORELSE APPEND ORELSE' APPEND';
infix 0 THEN_ELSE;

signature TACTICAL =
sig
  type tactic = thm -> thm Seq.seq
  val THEN: tactic * tactic -> tactic
  val ORELSE: tactic * tactic -> tactic
  val APPEND: tactic * tactic -> tactic
  val THEN_ELSE: tactic * (tactic*tactic) -> tactic
  val THEN': ('a -> tactic) * ('a -> tactic) -> 'a -> tactic
  val ORELSE': ('a -> tactic) * ('a -> tactic) -> 'a -> tactic
  val APPEND': ('a -> tactic) * ('a -> tactic) -> 'a -> tactic
  val all_tac: tactic
  val no_tac: tactic
  val DETERM: tactic -> tactic
  val COND: (thm -> bool) -> tactic -> tactic -> tactic
  val TRY: tactic -> tactic
  val EVERY: tactic list -> tactic
  val EVERY': ('a -> tactic) list -> 'a -> tactic
  val EVERY1: (int -> tactic) list -> tactic
  val FIRST: tactic list -> tactic
  val FIRST': ('a -> tactic) list -> 'a -> tactic
  val FIRST1: (int -> tactic) list -> tactic
  val RANGE: (int -> tactic) list -> int -> tactic
  val print_tac: Proof.context -> string -> tactic
  val pause_tac: tactic
  val trace_REPEAT: bool Unsynchronized.ref
  val suppress_tracing: bool Unsynchronized.ref
  val tracify: bool Unsynchronized.ref -> tactic -> tactic
  val traced_tac: (thm -> (thm * thm Seq.seq) option) -> tactic
  val REPEAT_DETERM_N: int -> tactic -> tactic
  val REPEAT_DETERM: tactic -> tactic
  val REPEAT: tactic -> tactic
  val REPEAT_DETERM1: tactic -> tactic
  val REPEAT1: tactic -> tactic
  val FILTER: (thm -> bool) -> tactic -> tactic
  val CHANGED: tactic -> tactic
  val CHANGED_PROP: tactic -> tactic
  val ALLGOALS: (int -> tactic) -> tactic
  val SOMEGOAL: (int -> tactic) -> tactic
  val FIRSTGOAL: (int -> tactic) -> tactic
  val HEADGOAL: (int -> thm -> 'a Seq.seq) -> thm -> 'a Seq.seq
  val REPEAT_SOME: (int -> tactic) -> tactic
  val REPEAT_DETERM_SOME: (int -> tactic) -> tactic
  val REPEAT_FIRST: (int -> tactic) -> tactic
  val REPEAT_DETERM_FIRST: (int -> tactic) -> tactic
  val TRYALL: (int -> tactic) -> tactic
  val CSUBGOAL: ((cterm * int) -> tactic) -> int -> tactic
  val SUBGOAL: ((term * int) -> tactic) -> int -> tactic
  val ASSERT_SUBGOAL: (int -> tactic) -> int -> tactic
  val CHANGED_GOAL: (int -> tactic) -> int -> tactic
  val SOLVED': (int -> tactic) -> int -> tactic
  val THEN_ALL_NEW: (int -> tactic) * (int -> tactic) -> int -> tactic
  val REPEAT_ALL_NEW: (int -> tactic) -> int -> tactic
  val PRIMSEQ: (thm -> thm Seq.seq) -> tactic
  val PRIMITIVE: (thm -> thm) -> tactic
  val SINGLE: tactic -> thm -> thm option
  val CONVERSION: conv -> int -> tactic
end;

structure Tactical : TACTICAL =
struct

(**** Tactics ****)

(*A tactic maps a proof tree to a sequence of proof trees:
    if length of sequence = 0 then the tactic does not apply;
    if length > 1 then backtracking on the alternatives can occur.*)

type tactic = thm -> thm Seq.seq;


(*** LCF-style tacticals ***)

(*the tactical THEN performs one tactic followed by another*)
fun (tac1 THEN tac2) st = Seq.maps tac2 (tac1 st);


(*The tactical ORELSE uses the first tactic that returns a nonempty sequence.
  Like in LCF, ORELSE commits to either tac1 or tac2 immediately.
  Does not backtrack to tac2 if tac1 was initially chosen. *)
fun (tac1 ORELSE tac2) st =
    case Seq.pull(tac1 st) of
        NONE       => tac2 st
      | sequencecell => Seq.make(fn()=> sequencecell);


(*The tactical APPEND combines the results of two tactics.
  Like ORELSE, but allows backtracking on both tac1 and tac2.
  The tactic tac2 is not applied until needed.*)
fun (tac1 APPEND tac2) st =
  Seq.append (tac1 st) (Seq.make(fn()=> Seq.pull (tac2 st)));

(*Conditional tactic.
        tac1 ORELSE tac2 = tac1 THEN_ELSE (all_tac, tac2)
        tac1 THEN tac2   = tac1 THEN_ELSE (tac2, no_tac)
*)
fun (tac THEN_ELSE (tac1, tac2)) st =
    case Seq.pull(tac st) of
        NONE    => tac2 st                                   (*failed; try tactic 2*)
      | seqcell => Seq.maps tac1 (Seq.make(fn()=> seqcell)); (*succeeded; use tactic 1*)


(*Versions for combining tactic-valued functions, as in
     SOMEGOAL (resolve_tac rls THEN' assume_tac) *)
fun (tac1 THEN' tac2) x = tac1 x THEN tac2 x;
fun (tac1 ORELSE' tac2) x = tac1 x ORELSE tac2 x;
fun (tac1 APPEND' tac2) x = tac1 x APPEND tac2 x;

(*passes all proofs through unchanged;  identity of THEN*)
fun all_tac st = Seq.single st;

(*passes no proofs through;  identity of ORELSE and APPEND*)
fun no_tac st  = Seq.empty;


(*Make a tactic deterministic by chopping the tail of the proof sequence*)
fun DETERM tac = Seq.DETERM tac;

(*Conditional tactical: testfun controls which tactic to use next.
  Beware: due to eager evaluation, both thentac and elsetac are evaluated.*)
fun COND testfun thenf elsef = (fn prf =>
    if testfun prf then  thenf prf   else  elsef prf);

(*Do the tactic or else do nothing*)
fun TRY tac = tac ORELSE all_tac;

(*** List-oriented tactics ***)

local
  (*This version of EVERY avoids backtracking over repeated states*)

  fun EVY (trail, []) st =
        Seq.make (fn()=> SOME(st,
                        Seq.make (fn()=> Seq.pull (evyBack trail))))
    | EVY (trail, tac::tacs) st =
          case Seq.pull(tac st) of
              NONE    => evyBack trail              (*failed: backtrack*)
            | SOME(st',q) => EVY ((st',q,tacs)::trail, tacs) st'
  and evyBack [] = Seq.empty (*no alternatives*)
    | evyBack ((st',q,tacs)::trail) =
          case Seq.pull q of
              NONE        => evyBack trail
            | SOME(st,q') => if Thm.eq_thm (st',st)
                             then evyBack ((st',q',tacs)::trail)
                             else EVY ((st,q',tacs)::trail, tacs) st
in

(* EVERY [tac1,...,tacn]   equals    tac1 THEN ... THEN tacn   *)
fun EVERY tacs = EVY ([], tacs);
end;


(* EVERY' [tac1,...,tacn] i  equals    tac1 i THEN ... THEN tacn i   *)
fun EVERY' tacs i = EVERY (map (fn f => f i) tacs);

(*Apply every tactic to 1*)
fun EVERY1 tacs = EVERY' tacs 1;

(* FIRST [tac1,...,tacn]   equals    tac1 ORELSE ... ORELSE tacn   *)
fun FIRST tacs = fold_rev (curry op ORELSE) tacs no_tac;

(* FIRST' [tac1,...,tacn] i  equals    tac1 i ORELSE ... ORELSE tacn i   *)
fun FIRST' tacs = fold_rev (curry op ORELSE') tacs (K no_tac);

(*Apply first tactic to 1*)
fun FIRST1 tacs = FIRST' tacs 1;

(*Apply tactics on consecutive subgoals*)
fun RANGE [] _ = all_tac
  | RANGE (tac :: tacs) i = RANGE tacs (i + 1) THEN tac i;


(*** Tracing tactics ***)

(*Print the current proof state and pass it on.*)
fun print_tac ctxt msg st =
 (tracing (msg ^ "\n" ^ Pretty.string_of (Pretty.chunks (Goal_Display.pretty_goals ctxt st)));
  Seq.single st);

(*Pause until a line is typed -- if non-empty then fail. *)
fun pause_tac st =
  (tracing "** Press RETURN to continue:";
   if TextIO.inputLine TextIO.stdIn = SOME "\n" then Seq.single st
   else (tracing "Goodbye";  Seq.empty));

exception TRACE_EXIT of thm
and TRACE_QUIT;

(*Tracing flags*)
val trace_REPEAT= Unsynchronized.ref false
and suppress_tracing = Unsynchronized.ref false;

(*Handle all tracing commands for current state and tactic *)
fun exec_trace_command flag (tac, st) =
   case TextIO.inputLine TextIO.stdIn of
       SOME "\n" => tac st
     | SOME "f\n" => Seq.empty
     | SOME "o\n" => (flag := false;  tac st)
     | SOME "s\n" => (suppress_tracing := true;  tac st)
     | SOME "x\n" => (tracing "Exiting now";  raise (TRACE_EXIT st))
     | SOME "quit\n" => raise TRACE_QUIT
     | _     => (tracing
"Type RETURN to continue or...\n\
\     f    - to fail here\n\
\     o    - to switch tracing off\n\
\     s    - to suppress tracing until next entry to a tactical\n\
\     x    - to exit at this point\n\
\     quit - to abort this tracing run\n\
\** Well? "     ;  exec_trace_command flag (tac, st));


(*Extract from a tactic, a thm->thm seq function that handles tracing*)
fun tracify flag tac st =
  if !flag andalso not (!suppress_tracing) then
    (tracing (Pretty.string_of (Pretty.chunks
        (Goal_Display.pretty_goals (Syntax.init_pretty_global (Thm.theory_of_thm st)) st @
          [Pretty.str "** Press RETURN to continue:"])));
     exec_trace_command flag (tac, st))
  else tac st;

(*Create a tactic whose outcome is given by seqf, handling TRACE_EXIT*)
fun traced_tac seqf st =
    (suppress_tracing := false;
     Seq.make (fn()=> seqf st
                         handle TRACE_EXIT st' => SOME(st', Seq.empty)));


(*Deterministic REPEAT: only retains the first outcome;
  uses less space than REPEAT; tail recursive.
  If non-negative, n bounds the number of repetitions.*)
fun REPEAT_DETERM_N n tac =
  let val tac = tracify trace_REPEAT tac
      fun drep 0 st = SOME(st, Seq.empty)
        | drep n st =
           (case Seq.pull(tac st) of
                NONE       => SOME(st, Seq.empty)
              | SOME(st',_) => drep (n-1) st')
  in  traced_tac (drep n)  end;

(*Allows any number of repetitions*)
val REPEAT_DETERM = REPEAT_DETERM_N ~1;

(*General REPEAT: maintains a stack of alternatives; tail recursive*)
fun REPEAT tac =
  let val tac = tracify trace_REPEAT tac
      fun rep qs st =
        case Seq.pull(tac st) of
            NONE       => SOME(st, Seq.make(fn()=> repq qs))
          | SOME(st',q) => rep (q::qs) st'
      and repq [] = NONE
        | repq(q::qs) = case Seq.pull q of
            NONE       => repq qs
          | SOME(st,q) => rep (q::qs) st
  in  traced_tac (rep [])  end;

(*Repeat 1 or more times*)
fun REPEAT_DETERM1 tac = DETERM tac THEN REPEAT_DETERM tac;
fun REPEAT1 tac = tac THEN REPEAT tac;


(** Filtering tacticals **)

fun FILTER pred tac st = Seq.filter pred (tac st);

(*Accept only next states that change the theorem somehow*)
fun CHANGED tac st =
  let fun diff st' = not (Thm.eq_thm (st, st'));
  in Seq.filter diff (tac st) end;

(*Accept only next states that change the theorem's prop field
  (changes to signature, hyps, etc. don't count)*)
fun CHANGED_PROP tac st =
  let fun diff st' = not (Thm.eq_thm_prop (st, st'));
  in Seq.filter diff (tac st) end;


(*** Tacticals based on subgoal numbering ***)

(*For n subgoals, performs tac(n) THEN ... THEN tac(1)
  Essential to work backwards since tac(i) may add/delete subgoals at i. *)
fun ALLGOALS tac st =
  let fun doall 0 = all_tac
        | doall n = tac(n) THEN doall(n-1)
  in  doall (Thm.nprems_of st) st  end;

(*For n subgoals, performs tac(n) ORELSE ... ORELSE tac(1)  *)
fun SOMEGOAL tac st =
  let fun find 0 = no_tac
        | find n = tac(n) ORELSE find(n-1)
  in  find (Thm.nprems_of st) st  end;

(*For n subgoals, performs tac(1) ORELSE ... ORELSE tac(n).
  More appropriate than SOMEGOAL in some cases.*)
fun FIRSTGOAL tac st =
  let fun find (i,n) = if i>n then no_tac else  tac(i) ORELSE find (i+1,n)
  in  find (1, Thm.nprems_of st) st  end;

(*First subgoal only.*)
fun HEADGOAL tac = tac 1;

(*Repeatedly solve some using tac. *)
fun REPEAT_SOME tac = REPEAT1 (SOMEGOAL (REPEAT1 o tac));
fun REPEAT_DETERM_SOME tac = REPEAT_DETERM1 (SOMEGOAL (REPEAT_DETERM1 o tac));

(*Repeatedly solve the first possible subgoal using tac. *)
fun REPEAT_FIRST tac = REPEAT1 (FIRSTGOAL (REPEAT1 o tac));
fun REPEAT_DETERM_FIRST tac = REPEAT_DETERM1 (FIRSTGOAL (REPEAT_DETERM1 o tac));

(*For n subgoals, tries to apply tac to n,...1  *)
fun TRYALL tac = ALLGOALS (TRY o tac);


(*Make a tactic for subgoal i, if there is one.  *)
fun CSUBGOAL goalfun i st =
  (case SOME (Thm.cprem_of st i) handle THM _ => NONE of
    SOME goal => goalfun (goal, i) st
  | NONE => Seq.empty);

fun SUBGOAL goalfun =
  CSUBGOAL (fn (goal, i) => goalfun (Thm.term_of goal, i));

fun ASSERT_SUBGOAL (tac: int -> tactic) i st = (Logic.get_goal (Thm.prop_of st) i; tac i st);

(*Returns all states that have changed in subgoal i, counted from the LAST
  subgoal.  For stac, for example.*)
fun CHANGED_GOAL tac i st =
  SUBGOAL (fn (t, _) =>
    let
      val np = Thm.nprems_of st;
      val d = np - i;  (*distance from END*)
      fun diff st' =
        Thm.nprems_of st' - d <= 0 orelse  (*the subgoal no longer exists*)
        not (Envir.aeconv (t, Thm.term_of (Thm.cprem_of st' (Thm.nprems_of st' - d))));
    in Seq.filter diff o tac i end) i st;

(*Returns all states where some subgoals have been solved.  For
  subgoal-based tactics this means subgoal i has been solved
  altogether -- no new subgoals have emerged.*)
fun SOLVED' tac i st =
  tac i st |> Seq.filter (fn st' => Thm.nprems_of st' < Thm.nprems_of st);

(*Apply second tactic to all subgoals emerging from the first --
  following usual convention for subgoal-based tactics.*)
fun (tac1 THEN_ALL_NEW tac2) i st =
  st |> (tac1 i THEN (fn st' =>
    Seq.INTERVAL tac2 i (i + Thm.nprems_of st' - Thm.nprems_of st) st'));

(*Repeatedly dig into any emerging subgoals.*)
fun REPEAT_ALL_NEW tac =
  tac THEN_ALL_NEW (TRY o (fn i => REPEAT_ALL_NEW tac i));

(*Makes a tactic whose effect on a state is given by thmfun: thm->thm seq.*)
fun PRIMSEQ thmfun st =  thmfun st handle THM _ => Seq.empty;

(*Makes a tactic whose effect on a state is given by thmfun: thm->thm.*)
fun PRIMITIVE thmfun = PRIMSEQ (Seq.single o thmfun);

(*Inverse (more or less) of PRIMITIVE*)
fun SINGLE tacf = Option.map fst o Seq.pull o tacf

(*Conversions as tactics*)
fun CONVERSION cv i st = Seq.single (Conv.gconv_rule cv i st)
  handle THM _ => Seq.empty
    | CTERM _ => Seq.empty
    | TERM _ => Seq.empty
    | TYPE _ => Seq.empty;

end;

open Tactical;
