(* STUB *)

fun run () =
let
  fun echo_loop src =
  let
    val ([json], src') = Source.get src
  in (TextIO.print (Json.encode json); echo_loop src')
  end
in
(
  (*ControllerProtocol.run_in_stdio ();*)
  echo_loop (Json.json_source TextIO.stdIn);
  OS.Process.exit OS.Process.success
)
end

