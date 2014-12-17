(*  Title:      Pure/General/sha1_polyml.ML
    Author:     Sascha Boehme, TU Muenchen

Digesting strings according to SHA-1 (see RFC 3174) -- based on an
external implementation in C with a fallback to an internal
implementation.
*)

structure SHA1: SHA1 =
struct

(* digesting *)

fun hex_digit i = if i < 10 then chr (ord "0" + i) else chr (ord "a" + i - 10);

fun hex_string arr i =
  let val c = CInterface.fromCchar (CInterface.offset i CInterface.Cchar arr)
  in (op ^) (pairself hex_digit (Integer.div_mod (Char.ord c) 16)) end

val lib_path =
  ("$ML_HOME/" ^ (if ML_System.platform_is_cygwin then "sha1.dll" else "libsha1.so"))
  |> Path.explode;

val STRING_INPUT_BYTES =
  CInterface.mkConversion undefined (CInterface.toCbytes o Byte.stringToBytes)
    (CInterface.Cpointer CInterface.Cchar);

fun digest_external str =
  let
    val digest = CInterface.alloc 20 CInterface.Cchar;
    val _ =
      CInterface.call3 (CInterface.get_sym (File.platform_path lib_path) "sha1_buffer")
        (STRING_INPUT_BYTES, CInterface.LONG, CInterface.POINTER)
        CInterface.POINTER (str, size str, CInterface.address digest);
  in fold (suffix o hex_string digest) (0 upto 19) "" end;

fun digest_string str = digest_external str
  handle CInterface.Foreign msg =>
    (warning (msg ^ "\nUsing slow ML implementation of SHA1.digest"); SHA1.rep (SHA1.digest str));


(* type digest *)

datatype digest = Digest of string;

val digest = Digest o digest_string;
fun rep (Digest s) = s;

val fake = Digest;

end;
