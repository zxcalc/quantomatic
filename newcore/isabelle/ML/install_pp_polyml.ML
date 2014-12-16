(*  Title:      Pure/ML/install_pp_polyml.ML
    Author:     Makarius

Extra toplevel pretty-printing for Poly/ML.
*)

PolyML.install_pp
  (fn (str, _, _, _) => fn depth => fn (print: 'a * int -> unit) => fn (x: 'a future) =>
    (case Future.peek x of
      NONE => str "<future>"
    | SOME (Exn.Exn _) => str "<failed>"
    | SOME (Exn.Result y) => print (y, depth)));

PolyML.install_pp
  (fn (str, _, _, _) => fn depth => fn (print: 'a * int -> unit) => fn (x: 'a lazy) =>
    (case Lazy.peek x of
      NONE => str "<lazy>"
    | SOME (Exn.Exn _) => str "<failed>"
    | SOME (Exn.Result y) => print (y, depth)));

