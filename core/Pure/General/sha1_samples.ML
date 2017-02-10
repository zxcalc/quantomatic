(*  Title:      Pure/General/sha1_samples.ML
    Author:     Makarius

Some SHA1 samples found in the wild.
*)

signature SHA1_SAMPLES =
sig
  val test: unit -> unit
end;

structure SHA1_Samples: SHA1_SAMPLES =
struct

fun check (msg, key) =
  let val key' = SHA1.rep (SHA1.digest msg) in
    if key = key' then ()
    else
      raise Fail ("SHA1 library integrity test failed on " ^ quote msg ^ ":\n" ^
        key ^ " expected, but\n" ^ key' ^ " was found")
  end;

fun test () =
  List.app check
   [("", "da39a3ee5e6b4b0d3255bfef95601890afd80709"),
    ("a", "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"),
    ("abc", "a9993e364706816aba3e25717850c26c9cd0d89d"),
    ("abcdefghijklmnopqrstuvwxyz", "32d10c7b8cf96570ca04ce37f2a19d84240d3a89"),
    ("The quick brown fox jumps over the lazy dog", "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12"),
    (replicate_string 100 "\000", "ed4a77d1b56a118938788fc53037759b6c501e3d"),
    ("a\000b", "4a3dec2d1f8245280855c42db0ee4239f917fdb8"),
    ("\000\001", "3f29546453678b855931c174a97d6c0894b8f546")];

val _ = test ();

end;
