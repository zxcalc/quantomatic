structure Console = struct
    fun print (m) =
        let
            val json_obj = JSON.empty
                        |> JSON.add ("type", JSON.Int 0)
                        |> JSON.add ("output", (JSON.String m))
        in
            PolyChrome.send_request (JSON.encode json_obj)
        end
end;