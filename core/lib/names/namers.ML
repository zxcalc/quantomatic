(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      gproof/namers.ML
    Author:     Lucas Dixon, University of Edinburgh
                lucas.dixon@ed.ac.uk
                Initially written Oct 2005, Updated 10 Mar 2006        *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
    Basic kinds of names and their lifting into FreshNamers.

    Requirements: 
    order must be transitive, and monotonic w.r.t. succ
    pred (succ x) = x
    
    A useful problem to think of: 
    if "za" = succ("z") and "c7" < "z" then it must be that "c7" < "za"
    standard string-orders (length then alpha, and basic alpha) fail 
    for this notion of succ.
    
    AK: if length then alpha, shouldn't c7 be bigger than z?

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(* string names that go up by changing ...zzz => ...aaaa, (the last n
"z"s to n +1 "a"s) and otherwise incrementing the last letter if its
alpha. If not alpha, we add an "a" to the end of it. *)

(* names are string * int - int is incremeneted and represents 
   the last lower-case alpha part of the string *)
structure StrSNamer 
 : SNAMER where type src = string
= struct

type src = string 

exception exp of string;

datatype name = 
         StrName of string * int (* int is printed as alpha *) 
       | StrIntName of string * int; (* int is printed as int *)


val bot_char = #"a"; (* top char *)
val top_char = #"z"; (* top char *)
val char_root = (Char.ord bot_char) - 1;
val int_base = (Char.ord top_char) - char_root; (* cannot be 0 *)

(* integers as chrlist, working in the above base with the given root 
   e.g. "a" = 0, "b" = 1, "ab" = int_base + 1, "aab" = int_base ^ 2 + 1, etc
*)
fun chrlist_of_int i = 
    let 
      fun chrlist_of' 0 l = l
        | chrlist_of' i l = 
          let val (q,r) = IntInf.quotRem (i, int_base) (* Maybe use Integer.div_mod ? what's the difference? *)
          in if r = 0 then 
               chrlist_of' (q - 1) (top_char::l)
             else 
               chrlist_of' q ((Char.chr (char_root + r)) :: l)
          end
    in chrlist_of' i [] end;

(* assumes characters between a and z. *)
(* IMPROVE? should check that c is in the int_base range? *)
fun int_of_chrlist [] = 0
  | int_of_chrlist (h::l) = 
    let
      val i0 = (Char.ord h) - char_root
      fun int_of' [] a = a
        | int_of' (c::l) a = 
          let val i = (Char.ord c) - char_root
          in int_of' l ((a * int_base) + i) end;
    in (int_of' l i0) end;

(* accumulate the alpha chars at the start of a given string: we use
this on a string that has been reversed so that we get all the alpha
chars at the end of the string. *)
fun is_digit c = (c >= #"0" andalso c <= #"9");
fun is_alpha c = (c >= #"a" andalso c <= #"z");

fun split_prefix chkf a [] = ([], a)
  | split_prefix chkf a (l as (h :: t)) = 
    if chkf h then split_prefix chkf (h::a) t
    else (l, a);

fun mk s = 
    let val cl = String.explode s
    in
      case (rev cl) 
       of [] => StrName (s,0) (* default is stringname... *)
        | (c::cl2) => 
          if is_alpha c then 
            let val (sl,el) = split_prefix is_alpha [c] cl2
            in StrName(String.implode (rev sl), int_of_chrlist el) end
          else if is_digit c then 
            let val (sl,el) = split_prefix is_digit [c] cl2
            in StrIntName(String.implode (rev sl), 
                          case el of [] => 0
                                   | _ => the (Int.fromString (String.implode el))) end
          else StrName (s,0) (* default for no postfix is string-postfix *)
    end;

fun dest (StrName (s,i)) = (s ^ (String.implode (chrlist_of_int i)))
  | dest (StrIntName (s,i)) = (s ^ (Int.toString i));

val pretty = Pretty.str o dest; 

fun suc' (a,b) = (a, b + 1);
fun pred' (x as (a,0)) = NONE
  | pred' (a,b) = SOME (a, b - 1);

fun suc (StrName a) = StrName (suc' a)
  | suc (StrIntName a) = StrIntName (suc' a);

fun optappl f NONE = NONE
  | optappl f (SOME x) = SOME (f x);

fun pred (StrName a) = optappl StrName (pred' a)
  | pred (StrIntName a) = optappl StrIntName (pred' a);

(* check string order first, then integer suffix *)
val ord' = Library.prod_ord String.compare Int.compare;

fun ord (StrName a, StrName b) = ord' (a,b)
  | ord (StrIntName a, StrIntName b) = ord' (a,b)
  | ord (StrName _, StrIntName _) = LESS
  | ord (StrIntName _, StrName _) = GREATER;

fun eq n1n2 = (case ord n1n2 of EQUAL => true | _ => false);

end; (* structure *)




(* NOTE: probably not good to use - very slow - better to use StrSNamer, defined above *)
structure StrNamer 
: NAMER 
= struct

exception exp of string;

type name = string;
val pretty = Pretty.str;

fun is_alpha c = (c >= #"a" andalso c <= #"z");

(* We count the final letters as these are the digits that change
when we reach the final letter(s). eg. zzz => aaaa *)
fun count_z nzs (#"z"::t) = count_z (nzs + 1) t
  | count_z nzs l = (nzs,l);
fun count_a nas (#"a"::t) = count_a (nas + 1) t 
  | count_a nas l = (nas,l);
(* Note: only applied to alpha chars *)

(* counting for digits, currently unused: TODO: add numerical endings? *)
fun count_9 nzs (#"9"::t) = count_9 (nzs + 1) t
  | count_9 nzs l = (nzs,l);
fun count_0 nas (#"0"::t) = count_0 (nas + 1) t 
  | count_0 nas l = (nas,l);

(* adds an "a" to the end of a string if no alpha ending part, if
we get a bunch of "z"'s, then we replace them by the number + 1 of
"a"'s. Else we just inc the last char's value *)
fun alpha_succ l =
    let val (n, rest) = count_z 0 (rev l)
    in
      rev (case rest 
            of [] => List.tabulate (n + 1, K (#"a"))
             | (h::t) => (List.tabulate (n, K (#"a"))) 
                                 @ ((Char.succ h) :: t))
    end;

(* if l ends with "a"'s we replace it by one less "z"'s, else dec last
char value. If no chars, do nothing - we are allowed pred to not give
a new value *)
fun alpha_pred l =
    let val (n, rest) = count_a 0 (rev l)
    in
      rev (case rest of [] => (if n > 1 then List.tabulate (n - 1, K (#"z")) 
                                  else [])
                         | (h :: t) => (List.tabulate (n, K (#"z")))
                                       @ ((Char.pred h) :: t))
    end;

(* accumulate the alpha chars at the start of a given string: we use
this on a string that has been reversed so that we get all the alpha
chars at the end of the string. *)
fun alpha_revstart a [] = (a,[]) (* all alpha num *)
  | alpha_revstart a (l as (h :: t)) = 
    if is_alpha h then alpha_revstart (h::a) t
    else (a, l);
(* apply a function to the alpha-end part of a string *)
fun appto_alpha_ending f cl =
    let val (ending,revrest) = alpha_revstart [] (rev cl)
    in (rev revrest) @ (f ending) end;

val suc = String.implode 
          o (appto_alpha_ending alpha_succ) 
          o String.explode;

fun pred "" = NONE 
	| pred s = SOME ((String.implode 
										o (appto_alpha_ending alpha_pred) 
										o String.explode) s);

(* break a string into it's alpha ending and whatever before it *)
fun nonalpha_alpha_pair s = 
    let val (alphaending, revstart) = 
            alpha_revstart [] (rev (String.explode s))
    in (String.implode (rev revstart), String.implode alphaending) end;


val string_ord = String.compare;
fun len_based_string_ord (s1, s2) =
  (case Library.int_ord (size s1, size s2) 
    of EQUAL => string_ord (s1, s2) | ord => ord);

(* compares a string based on the alpha ending, but first use standard
fast_string check on non-alpha start. *)
fun ord (a,b) = 
    let val (a1,a2) = nonalpha_alpha_pair a
      val (b1,b2) = nonalpha_alpha_pair b
    in 
      (* Use length based ord for postfix part which must respect Suc
         operations, and ordinary left-to-right comparison for the
         string prefix so that prefixes look alphabetical.
      *)
      case string_ord (a1, b1)
       of EQUAL => len_based_string_ord (a2,b2)
        | x => x
    end;

fun eq n1n2 = (case String.compare n1n2 of EQUAL => true | _ => false)

end; (* structure *)


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
structure StrIntNamer : NAMER
  = struct
  type name = string * int 
  fun suc (x,i) = (x, i + 1);
  fun pred (x,0) = NONE
    | pred (x,i) = SOME (x,i - 1);
  val ord = Library.prod_ord Library.fast_string_ord Library.int_ord;
  fun pretty (n,i) = 
      Pretty.block 
        ([Pretty.str n]
         @ (if i <> 0 then [Pretty.str (Int.toString i)] else []));
  fun eq n1n2 = (case ord n1n2 of EQUAL => true | _ => false);
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
structure IntNamer : NAMER
  = struct
  type name = int 
  fun suc x = x + 1;
  fun pred 0 = NONE
		| pred x = SOME (x - 1);
  val ord = Library.int_ord;
  fun pretty x = Pretty.str (Int.toString x);
  fun eq n1n2 = (case ord n1n2 of EQUAL => true | _ => false);
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
functor SumNamer(
  structure Namer1 : NAMER
  structure Namer2 : NAMER
) =
struct
  datatype name = IN1 of Namer1.name | IN2 of Namer2.name
  type src = name
  fun suc (IN1 n) = IN1 (Namer1.suc n)
    | suc (IN2 n) = IN2 (Namer2.suc n)

  fun pred (IN1 n) = Option.map IN1 (Namer1.pred n)
    | pred (IN2 n) = Option.map IN2 (Namer2.pred n)

  fun ord (IN1 n1, IN1 n2) = Namer1.ord (n1,n2)
    | ord (IN2 n1, IN2 n2) = Namer2.ord (n1,n2)
    | ord (IN1 _, IN2 _) = GREATER
    | ord (IN2 _, IN1 _) = LESS

  fun eq ns = (ord ns = EQUAL)

  fun pretty (IN1 n) = Pretty.block [Pretty.str "IN1(", Namer1.pretty n, Pretty.str ")"]
    | pretty (IN2 n) = Pretty.block [Pretty.str "IN2(", Namer2.pretty n, Pretty.str ")"]

  val mk = I
  val dest = I
end

