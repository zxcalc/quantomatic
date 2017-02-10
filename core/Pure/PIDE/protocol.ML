(*  Title:      Pure/PIDE/protocol.ML
    Author:     Makarius

Protocol message formats for interactive proof documents.
*)

structure Protocol: sig end =
struct

val _ =
  Isabelle_Process.protocol_command "Prover.echo"
    (fn args => List.app writeln args);

val _ =
  Isabelle_Process.protocol_command "Prover.options"
    (fn [options_yxml] =>
      let val options = Options.decode (YXML.parse_body options_yxml) in
        Options.set_default options;
        Future.ML_statistics := true;
        Multithreading.trace := Options.int options "threads_trace";
        Multithreading.max_threads_update (Options.int options "threads");
        Goal.parallel_proofs := (if Options.int options "parallel_proofs" > 0 then 3 else 0)
      end);

val _ =
  Isabelle_Process.protocol_command "Document.define_blob"
    (fn [digest, content] => Document.change_state (Document.define_blob digest content));

val _ =
  Isabelle_Process.protocol_command "Document.define_command"
    (fn [id, name, blobs_yxml, text] =>
      let
        val blobs =
          YXML.parse_body blobs_yxml |>
            let open XML.Decode in
              list (variant
               [fn ([], a) => Exn.Res (pair string (option string) a),
                fn ([], a) => Exn.Exn (ERROR (string a))])
            end;
      in
        Document.change_state (Document.define_command (Document_ID.parse id) name blobs text)
      end);

val _ =
  Isabelle_Process.protocol_command "Document.discontinue_execution"
    (fn [] => Execution.discontinue ());

val _ =
  Isabelle_Process.protocol_command "Document.cancel_exec"
    (fn [exec_id] => Execution.cancel (Document_ID.parse exec_id));

val _ =
  Isabelle_Process.protocol_command "Document.update"
    (fn [old_id_string, new_id_string, edits_yxml] => Document.change_state (fn state =>
      let
        val _ = Execution.discontinue ();

        val old_id = Document_ID.parse old_id_string;
        val new_id = Document_ID.parse new_id_string;
        val edits =
          YXML.parse_body edits_yxml |>
            let open XML.Decode in
              list (pair string
                (variant
                 [fn ([], a) => Document.Edits (list (pair (option int) (option int)) a),
                  fn ([], a) =>
                    let
                      val (master, (name, (imports, (keywords, errors)))) =
                        pair string (pair string (pair (list string)
                          (pair (list (pair string
                            (option (pair (pair string (list string)) (list string)))))
                            (list string)))) a;
                      val imports' = map (rpair Position.none) imports;
                      val header = Thy_Header.make (name, Position.none) imports' keywords;
                    in Document.Deps (master, header, errors) end,
                  fn (a :: b, c) =>
                    Document.Perspective (bool_atom a, map int_atom b,
                      list (pair int (pair string (list string))) c)]))
            end;

        val (removed, assign_update, state') = Document.update old_id new_id edits state;
        val _ = List.app Execution.terminate removed;
        val _ = Execution.purge removed;
        val _ = List.app Isabelle_Process.reset_tracing removed;

        val _ =
          Output.protocol_message Markup.assign_update
            [(new_id, assign_update) |>
              let open XML.Encode
              in pair int (list (pair int (list int))) end
              |> YXML.string_of_body];
      in Document.start_execution state' end));

val _ =
  Isabelle_Process.protocol_command "Document.remove_versions"
    (fn [versions_yxml] => Document.change_state (fn state =>
      let
        val versions =
          YXML.parse_body versions_yxml |>
            let open XML.Decode in list int end;
        val state1 = Document.remove_versions versions state;
        val _ = Output.protocol_message Markup.removed_versions [versions_yxml];
      in state1 end));

val _ =
  Isabelle_Process.protocol_command "Document.dialog_result"
    (fn [serial, result] =>
      Active.dialog_result (Markup.parse_int serial) result
        handle exn => if Exn.is_interrupt exn then () (*sic!*) else reraise exn);

val _ =
  Isabelle_Process.protocol_command "use_theories"
    (fn id :: master_dir :: thys =>
      let
        val result =
          Exn.capture (fn () =>
            Thy_Info.use_theories
              {document = false, last_timing = K NONE, master_dir = Path.explode master_dir}
              (map (rpair Position.none) thys)) ();
        val ok =
          (case result of
            Exn.Res _ => true
          | Exn.Exn exn => (Runtime.exn_error_message exn; false));
    in Output.protocol_message (Markup.use_theories_result id ok) [] end);

val _ =
  Isabelle_Process.protocol_command "ML_System.share_common_data"
    (fn [] => ML_System.share_common_data ());

end;

