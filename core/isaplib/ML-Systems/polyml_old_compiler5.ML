(*  Title:      Pure/ML-Systems/polyml_old_compiler5.ML
    ID:         $Id: polyml_old_compiler5.ML,v 1.5 2008/05/14 09:09:07 wenzelm Exp $

Runtime compilation -- for old PolyML.compilerEx (version 5.0, 5.1).
*)

fun use_text (tune: string -> string) _ (line, name) (print, err) verbose txt =
  let
    val in_buffer = ref (explode (tune txt));
    val out_buffer = ref ([]: string list);
    fun output () = implode (rev (case ! out_buffer of "\n" :: cs => cs | cs => cs));

    val current_line = ref line;
    fun get () =
      (case ! in_buffer of
        [] => ""
      | c :: cs => (in_buffer := cs; if c = "\n" then current_line := ! current_line + 1 else (); c));
    fun put s = out_buffer := s :: ! out_buffer;

    fun exec () =
      (case ! in_buffer of
        [] => ()
      | _ => (PolyML.compilerEx (get, put, fn () => ! current_line, name) (); exec ()));
  in
    exec () handle exn => (err (output ()); raise exn);
    if verbose then print (output ()) else ()
  end;

fun use_file tune str_of_pos output verbose name =
  let
    val instream = TextIO.openIn name;
    val txt = Exn.release (Exn.capture TextIO.inputAll instream before TextIO.closeIn instream);
  in use_text tune str_of_pos (1, name) output verbose txt end;

