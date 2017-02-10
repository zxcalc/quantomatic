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
      Isabelle_Process.init_protocol_options (Options.decode (YXML.parse_body options_yxml)));

val _ =
  Isabelle_Process.protocol_command "Document.define_blob"
    (fn [digest, content] => Document.change_state (Document.define_blob digest content));

val _ =
  Isabelle_Process.protocol_command "Document.define_command"
    (fn id :: name :: blobs_yxml :: toks_yxml :: sources =>
      let
        val (blobs, blobs_index) =
          YXML.parse_body blobs_yxml |>
            let
              val message =
                YXML.string_of_body o Protocol_Message.command_positions id;
              open XML.Decode;
            in
              pair
                (list (variant
                 [fn ([], a) => Exn.Res (pair string (option string) a),
                  fn ([], a) => Exn.Exn (ERROR (message a))]))
                int
            end;
        val toks =
          (YXML.parse_body toks_yxml |> let open XML.Decode in list (pair int int) end) ~~ sources;
      in
        Document.change_state
          (Document.define_command (Document_ID.parse id) name blobs blobs_index toks)
      end);

val _ =
  Isabelle_Process.protocol_command "Document.discontinue_execution"
    (fn [] => Execution.discontinue ());

val _ =
  Isabelle_Process.protocol_command "Document.cancel_exec"
    (fn [exec_id] => Execution.cancel (Document_ID.parse exec_id));

val _ =
  Isabelle_Process.protocol_command "Document.update"
    (Future.task_context "Document.update" (Future.new_group NONE)
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
                              (pair (pair string (list string)) (list string))))
                              (list YXML.string_of_body)))) a;
                        val imports' = map (rpair Position.none) imports;
                        val keywords' = map (fn (x, y) => ((x, Position.none), y)) keywords;
                        val header = Thy_Header.make (name, Position.none) imports' keywords';
                      in Document.Deps {master = master, header = header, errors = errors} end,
                    fn (a :: b, c) =>
                      Document.Perspective (bool_atom a, map int_atom b,
                        list (pair int (pair string (list string))) c)]))
              end;

          val (removed, assign_update, state') = Document.update old_id new_id edits state;
          val _ =
            (singleton o Future.forks)
             {name = "Document.update/remove", group = NONE,
              deps = maps Future.group_snapshot (maps Execution.peek removed),
              pri = Task_Queue.urgent_pri + 2, interrupts = false}
             (fn () => (Execution.purge removed; List.app Isabelle_Process.reset_tracing removed));

          val _ =
            Output.protocol_message Markup.assign_update
              [(new_id, assign_update) |>
                let open XML.Encode
                in pair int (list (pair int (list int))) end
                |> YXML.string_of_body];
        in Document.start_execution state' end)));

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
      Active.dialog_result (Value.parse_int serial) result
        handle exn => if Exn.is_interrupt exn then () (*sic!*) else Exn.reraise exn);

val _ =
  Isabelle_Process.protocol_command "ML_Heap.share_common_data"
    (fn [] => ML_Heap.share_common_data ());

end;

