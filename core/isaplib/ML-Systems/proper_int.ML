(*  Title:      Pure/ML-Systems/proper_int.ML
    ID:         $Id: proper_int.ML,v 1.3 2007/09/16 13:36:57 wenzelm Exp $
    Author:     Makarius

SML basis with type int representing proper integers, not machine
words.
*)

val ml_system_fix_ints = true;

val mk_int = IntInf.fromInt: Int.int -> IntInf.int;
val dest_int = IntInf.toInt: IntInf.int -> Int.int;


(* Int *)

structure OrigInt = Int;
structure OrigIntInf = IntInf;
type int = IntInf.int;

structure IntInf =
struct
  open IntInf;
  fun fromInt (a: int) = a;
  fun toInt (a: int) = a;
  val log2 = mk_int o IntInf.log2;
  val sign = mk_int o IntInf.sign;
end;

structure Int = IntInf;


(* List *)

structure List =
struct
  open List;
  fun length a = mk_int (List.length a);
  fun nth (a, b) = List.nth (a, dest_int b);
  fun take (a, b) = List.take (a, dest_int b);
  fun drop (a, b) = List.drop (a, dest_int b);
  fun tabulate (a, b) = List.tabulate (dest_int a, b o mk_int);
end;

val length = List.length;


(* Array *)

structure Array =
struct
  open Array;
  val maxLen = mk_int Array.maxLen;
  fun array (a, b) = Array.array (dest_int a, b);
  fun tabulate (a, b) = Array.tabulate (dest_int a, b o mk_int);
  fun length a = mk_int (Array.length a);
  fun sub (a, b) = Array.sub (a, dest_int b);
  fun update (a, b, c) = Array.update (a, dest_int b, c);
  fun copy {src, dst, di} = Array.copy {src = src, dst = dst, di = dest_int di};
  fun copyVec {src, dst, di} = Array.copyVec {src = src, dst = dst, di = dest_int di};
  fun appi a b = Array.appi (fn (x, y) => a (mk_int x, y)) b;
  fun modifyi a b = Array.modifyi (fn (x, y) => a (mk_int x, y)) b;
  fun foldli a b c = Array.foldli (fn (x, y, z) => a (mk_int x, y, z)) b c;
  fun foldri a b c = Array.foldri (fn (x, y, z) => a (mk_int x, y, z)) b c;
  fun findi a b =
    (case Array.findi (fn (x, y) => a (mk_int x, y)) b of
      NONE => NONE
    | SOME (c, d) => SOME (mk_int c, d));
end;


(* Vector *)

structure Vector =
struct
  open Vector;
  val maxLen = mk_int Vector.maxLen;
  fun tabulate (a, b) = Vector.tabulate (dest_int a, b o mk_int);
  fun length a = mk_int (Vector.length a);
  fun sub (a, b) = Vector.sub (a, dest_int b);
  fun update (a, b, c) = Vector.update (a, dest_int b, c);
  fun appi a b = Vector.appi (fn (x, y) => a (mk_int x, y)) b;
  fun mapi a b = Vector.mapi (fn (x, y) => a (mk_int x, y)) b;
  fun foldli a b c = Vector.foldli (fn (x, y, z) => a (mk_int x, y, z)) b c;
  fun foldri a b c = Vector.foldri (fn (x, y, z) => a (mk_int x, y, z)) b c;
  fun findi a b =
    (case Vector.findi (fn (x, y) => a (mk_int x, y)) b of
      NONE => NONE
    | SOME (c, d) => SOME (mk_int c, d));
end;


(* Char *)

structure Char =
struct
  open Char;
  val maxOrd = mk_int Char.maxOrd;
  val chr = Char.chr o dest_int;
  val ord = mk_int o Char.ord;
end;

val chr = Char.chr;
val ord = Char.ord;


(* String *)

structure String =
struct
  open String;
  val maxSize = mk_int String.maxSize;
  val size = mk_int o String.size;
  fun sub (a, b) = String.sub (a, dest_int b);
  fun extract (a, b, c) = String.extract (a, dest_int b, Option.map dest_int c);
  fun substring (a, b, c) = String.substring (a, dest_int b, dest_int c);
end;

val size = String.size;
val substring = String.substring;


(* Substring *)

structure Substring =
struct
  open Substring;
  fun sub (a, b) = Substring.sub (a, dest_int b);
  val size = mk_int o Substring.size;
  fun base a = let val (b, c, d) = Substring.base a in (b, mk_int c, mk_int d) end;
  fun extract (a, b, c) = Substring.extract (a, dest_int b, Option.map dest_int c);
  fun substring (a, b, c) = Substring.substring (a, dest_int b, dest_int c);
  fun triml a b = Substring.triml (dest_int a) b;
  fun trimr a b = Substring.trimr (dest_int a) b;
  fun slice (a, b, c) = Substring.slice (a, dest_int b, Option.map dest_int c);
  fun splitAt (a, b) = Substring.splitAt (a, dest_int b);
end;


(* Word *)

structure Word =
struct
  open Word;
  val wordSize = mk_int Word.wordSize;
  val toInt = mk_int o Word.toInt;
  val toIntX = mk_int o Word.toIntX;
  val fromInt = Word.fromInt o dest_int;
end;


(* Real *)

structure Real =
struct
  open Real;
  val radix = mk_int Real.radix;
  val precision = mk_int Real.precision;
  fun sign a = mk_int (Real.sign a);
  fun toManExp a = let val {man, exp} = Real.toManExp a in {man = man, exp = mk_int exp} end;
  fun fromManExp {man, exp} = Real.fromManExp {man = man, exp = dest_int exp};
  val ceil = mk_int o Real.ceil;
  val floor = mk_int o Real.floor;
  val real = Real.fromInt o dest_int;
  val round = mk_int o Real.round;
  val trunc = mk_int o Real.trunc;
  fun toInt a b = mk_int (Real.toInt a b);
  fun fromInt a = Real.fromInt (dest_int a);
end;

val ceil = Real.ceil;
val floor = Real.floor;
val real = Real.real;
val round = Real.round;
val trunc = Real.trunc;


(* TextIO *)

structure TextIO =
struct
  open TextIO;
  fun inputN (a, b) = TextIO.inputN (a, dest_int b);
  fun canInput (a, b) = Option.map mk_int (TextIO.canInput (a, dest_int b));
end;


(* Time *)

structure Time =
struct
  open Time;
  fun fmt a b = Time.fmt (dest_int a) b;
end;


(* Posix *)

structure Posix =
struct
  open Posix;
  structure IO =
  struct
    open IO;
    fun mkTextWriter {appendMode, chunkSize, fd, initBlkMode, name} =
      IO.mkTextWriter {appendMode = appendMode, chunkSize = dest_int chunkSize,
        fd = fd, initBlkMode = initBlkMode, name = name};
  end;
end;

