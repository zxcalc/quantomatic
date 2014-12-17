(*  Title:      Pure/ML-Systems/compiler_polyml.ML

Basic runtime compilation for Poly/ML (cf. Pure/ML/ml_compiler_polyml.ML).
*)

local

fun drop_newline s =
  if String.isSuffix "\n" s then String.substring (s, 0, size s - 1)
  else s;

in

fun use_text ({tune_source, name_space, str_of_pos, print, error, ...}: use_context)
    (start_line, name) verbose txt =
  let
    val line = Unsynchronized.ref start_line;
    val in_buffer =
      Unsynchronized.ref (String.explode (tune_source (ml_positions start_line name txt)));
    val out_buffer = Unsynchronized.ref ([]: string list);
    fun output () = drop_newline (implode (rev (! out_buffer)));

    fun get () =
      (case ! in_buffer of
        [] => NONE
      | c :: cs => (in_buffer := cs; if c = #"\n" then line := ! line + 1 else (); SOME c));
    fun put s = out_buffer := s :: ! out_buffer;
    fun put_message {message = msg1, hard, location = {startLine = line, ...}, context} =
     (put (if hard then "Error: " else "Warning: ");
      PolyML.prettyPrint (put, 76) msg1;
      (case context of NONE => () | SOME msg2 => PolyML.prettyPrint (put, 76) msg2);
      put ("At" ^ str_of_pos line name ^ "\n"));

    val parameters =
     [PolyML.Compiler.CPOutStream put,
      PolyML.Compiler.CPNameSpace name_space,
      PolyML.Compiler.CPErrorMessageProc put_message,
      PolyML.Compiler.CPLineNo (fn () => ! line),
      PolyML.Compiler.CPFileName name,
      PolyML.Compiler.CPPrintInAlphabeticalOrder false];
    val _ =
      (while not (List.null (! in_buffer)) do
        PolyML.compiler (get, parameters) ())
      handle exn =>
        if Exn.is_interrupt exn then reraise exn
        else
         (put ("Exception- " ^ General.exnMessage exn ^ " raised");
          error (output ()); reraise exn);
  in if verbose then print (output ()) else () end;

fun use_file context verbose name =
  let
    val instream = TextIO.openIn name;
    val txt = Exn.release (Exn.capture TextIO.inputAll instream before TextIO.closeIn instream);
  in use_text context (1, name) verbose txt end;

end;

