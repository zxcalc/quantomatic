(*  Title:      Pure/ML-Systems/overloading_smlnj.ML
    Author:     Makarius

Overloading in SML/NJ (cf. smlnj/base/system/smlnj/init/pervasive.sml).
*)

Control.overloadKW := true;

overload ~ : ('a -> 'a) as
  IntInf.~ and Int31.~ and Int32.~ and Int64.~ and
  Word.~ and Word8.~ and Word32.~ and Word64.~ and Real.~;
overload + : ('a * 'a -> 'a) as
  IntInf.+ and Int31.+ and Int32.+ and Int64.+ and
  Word.+ and Word8.+ and Word32.+ and Word64.+ and Real.+;
overload - : ('a * 'a -> 'a) as
  IntInf.- and Int31.- and Int32.- and Int64.- and
  Word.- and Word8.- and Word32.- and Word64.- and Real.-;
overload * : ('a * 'a -> 'a) as
  IntInf.* and Int31.* and Int32.* and Int64.* and
  Word.* and Word8.* and Word32.* and Word64.* and Real.*;
overload div: ('a * 'a -> 'a) as
  IntInf.div and Int31.div and Int32.div and Int64.div and
  Word.div and Word8.div and Word32.div and Word64.div;
overload mod: ('a * 'a -> 'a) as
  IntInf.mod and Int31.mod and Int32.mod and Int64.mod and
  Word.mod and Word8.mod and Word32.mod and Word64.mod;
overload < : ('a * 'a -> bool) as
  IntInf.< and Int31.< and Int32.< and Int64.< and Real.< and
  Word.< and Word8.< and Word32.< and Word64.< and Char.< and String.<;
overload <= : ('a * 'a -> bool) as
  IntInf.<= and Int31.<= and Int32.<= and Int64.<= and Real.<= and
  Word.<= and Word8.<= and Word32.<= and Word64.<= and Char.<= and String.<=;
overload > : ('a * 'a -> bool) as
  IntInf.> and Int31.> and Int32.> and Int64.> and Real.> and
  Word.> and Word8.> and Word32.> and Word64.> and Char.> and String.>;
overload >= : ('a * 'a -> bool) as
  IntInf.>= and Int31.>= and Int32.>= and Int64.>= and Real.>= and
  Word.>= and Word8.>= and Word32.>= and Word64.>= and Char.>= and String.>=;
overload abs: ('a -> 'a) as IntInf.abs and Int31.abs and Int32.abs and Int64.abs and Real.abs;

Control.overloadKW := false;
