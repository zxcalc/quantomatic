(*  Title:      Pure/ML-Systems/polyml.ML
    ID:         $Id: polyml.ML,v 1.69 2008/05/14 09:05:11 wenzelm Exp $

Compatibility wrapper for Poly/ML (after 5.1).
*)

use "ML-Systems/polyml_common.ML";
use "ML-Systems/multithreading_polyml.ML";

val pointer_eq = PolyML.pointerEq;


(* runtime compilation *)

local

fun drop_newline s =
  if String.isSuffix "\n" s then String.substring (s, 0, size s - 1)
  else s;

in

fun use_text (tune: string -> string) str_of_pos (start_line, name) (print, err) verbose txt =
  let
    val current_line = ref start_line;
    val in_buffer = ref (String.explode (tune txt));
    val out_buffer = ref ([]: string list);
    fun output () = drop_newline (implode (rev (! out_buffer)));

    fun get () =
      (case ! in_buffer of
        [] => NONE
      | c :: cs =>
          (in_buffer := cs; if c = #"\n" then current_line := ! current_line + 1 else (); SOME c));
    fun put s = out_buffer := s :: ! out_buffer;
    fun message (msg, is_err, line) =
      (if is_err then "Error: " else "Warning: ") ^ drop_newline msg ^ str_of_pos line name ^ "\n";

    val parameters =
     [PolyML.Compiler.CPOutStream put,
      PolyML.Compiler.CPLineNo (fn () => ! current_line),
      PolyML.Compiler.CPErrorMessageProc (put o message)];
    val _ =
      (while not (List.null (! in_buffer)) do
        PolyML.compiler (get, parameters) ())
      handle exn =>
       (put ("Exception- " ^ General.exnMessage exn ^ " raised");
        err (output ()); raise exn);
  in if verbose then print (output ()) else () end;

fun use_file tune str_of_pos output verbose name =
  let
    val instream = TextIO.openIn name;
    val txt = Exn.release (Exn.capture TextIO.inputAll instream before TextIO.closeIn instream);
  in use_text tune str_of_pos (1, name) output verbose txt end;

end;

