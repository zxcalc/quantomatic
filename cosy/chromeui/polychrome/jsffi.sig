signature JSFFI =
sig
    type fptr
    
    exception Error of unit
    
    structure Tab : NAME_TAB
    structure Name : SSTR_NAMES
    
    
    (* this is used in combination with the exec_js_r and exec_js*)
    structure arg :
    sig
        val string : string -> JSON.T list
        val reference : fptr -> JSON.T list
        val real : real -> JSON.T list
        val object : JSON.T -> JSON.T list
        val null : unit -> JSON.T list
        val list : JSON.T list -> JSON.T list
        val int : int -> JSON.T list
        val callback : string -> JSON.T list
        val bool : bool -> JSON.T list
    end
    
    (* these are used to call JS functions
       when using exec_js_r, it is up to the implementation of the wrapper
       function to convert the returned string to an appropriate type *)
    (* args: an fptr to an object, a function name, an argument list *)
    (* e.g. exec_js_r "document|" "getElementById" [arg.string "something"] *)
    val exec_js_r : string -> string -> JSON.T list list -> string
    val exec_js : string -> string -> JSON.T list list -> unit
    val exec_js_get : string -> string -> JSON.T list list -> string
    val exec_js_set : string -> string -> JSON.T list list -> unit
    
    (* this must be called after handling each event *)
    val ready : unit -> unit
    
    (* these are used for keeping the temporary memory, used for
       storing javascript objects, tidy *)
    structure Memory :
    sig
        val switchNs : string -> unit
        val switchDefaultNs : unit -> unit
        val addFunctionReference : string -> fptr
        val addFunctionReferenceOW : string -> fptr
        val removeReference : string -> unit
        val deleteNs : string -> unit
        val clearNs : string -> unit
        val clearDefaultNs : unit -> unit
    end
    
end