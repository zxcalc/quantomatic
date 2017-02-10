(*  Title:      Pure/ML-Systems/proper_int.ML
    Author:     Makarius

SML basis with type int representing proper integers, not machine
words.
*)

val mk_int = IntInf.fromInt: Int.int -> IntInf.int;
val dest_int = IntInf.toInt: IntInf.int -> Int.int;


(* Int *)

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


(* CharVector *)

structure CharVector =
struct
  open CharVector;
  fun tabulate (a, b) = CharVector.tabulate (dest_int a, b o mk_int);
end;


(* Word8VectorSlice *)

structure Word8VectorSlice =
struct
  open Word8VectorSlice;
  val length = mk_int o Word8VectorSlice.length;
  fun subslice (a, b, c) = Word8VectorSlice.subslice (a, dest_int b, Option.map dest_int c);
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


(* StringCvt *)

structure StringCvt =
struct
  open StringCvt;
  datatype realfmt = EXACT | FIX of int option | GEN of int option | SCI of int option;
  fun realfmt fmt = Real.fmt
    (case fmt of
      EXACT => StringCvt.EXACT
    | FIX NONE => StringCvt.FIX NONE
    | FIX (SOME b) => StringCvt.FIX (SOME (dest_int b))
    | GEN NONE => StringCvt.GEN NONE
    | GEN (SOME b) => StringCvt.GEN (SOME (dest_int b))
    | SCI NONE => StringCvt.SCI NONE
    | SCI (SOME b) => StringCvt.SCI (SOME (dest_int b)));
  fun padRight a b c = StringCvt.padRight a (dest_int b) c;
  fun padLeft a b c = StringCvt.padLeft a (dest_int b) c;
end;


(* Word *)

structure Word =
struct
  open Word;
  val wordSize = mk_int Word.wordSize;
  val toInt = Word.toLargeInt;
  val toIntX = Word.toLargeIntX;
  val fromInt = Word.fromLargeInt;
end;

structure Word8 =
struct
  open Word8;
  val wordSize = mk_int Word8.wordSize;
  val toInt = Word8.toLargeInt;
  val toIntX = Word8.toLargeIntX;
  val fromInt = Word8.fromLargeInt;
end;

structure Word32 =
struct
  open Word32;
  val wordSize = mk_int Word32.wordSize;
  val toInt = Word32.toLargeInt;
  val toIntX = Word32.toLargeIntX;
  val fromInt = Word32.fromLargeInt;
end;

structure LargeWord =
struct
  open LargeWord;
  val wordSize = mk_int LargeWord.wordSize;
  val toInt = LargeWord.toLargeInt;
  val toIntX = LargeWord.toLargeIntX;
  val fromInt = LargeWord.fromLargeInt;
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
  val fmt = StringCvt.realfmt;
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


(* BinIO *)

structure BinIO =
struct
  open BinIO;
  fun inputN (a, b) = BinIO.inputN (a, dest_int b);
  fun canInput (a, b) = Option.map mk_int (BinIO.canInput (a, dest_int b));
end;


(* Time *)

structure Time =
struct
  open Time;
  fun fmt a b = Time.fmt (dest_int a) b;
end;


(* Sockets *)

structure INetSock =
struct
  open INetSock;
  fun toAddr (a, b) = INetSock.toAddr (a, dest_int b);
  fun fromAddr adr = let val (a, b) = INetSock.fromAddr adr in (a, mk_int b) end;
end;


(* OS.FileSys *)

structure OS =
struct
  open OS;
  structure FileSys =
  struct
    open FileSys;
    fun fileSize a = mk_int (FileSys.fileSize a);
  end;
end;
