structure Profiling
= struct
    val data = ref ([]:string list)
    
    fun profile message = let
        val time = Int.toString (Time.toMilliseconds (Time.now()))
        in data := !data @ [time ^ ";" ^ message] end
        
    fun profile2 m = ()
    
    fun writeTextFile (outfile:string) (data:string) =
        let
            val outs = TextIO.openAppend outfile
            val _ = TextIO.output (outs,data)
        in
            ()
        end
    
    fun profilingDataToStr nil = ""
      | profilingDataToStr (x::l) = ((x ^ "\n") ^ (profilingDataToStr l))
    
    fun writeProfilingReport () = (print "Writing profiling report.\n"; writeTextFile "../../profiling/output.txt" (profilingDataToStr (!data)))
end