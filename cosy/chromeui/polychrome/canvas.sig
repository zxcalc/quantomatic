signature CANVAS =
sig

    datatype Context = Context of jsffi.fptr
    
    val getContext : DOM.HTMLElement -> string -> Context
    val stroke : Context -> unit
    val fillRect : Context -> int -> int -> int -> int -> unit
    val moveTo : Context -> int -> int -> unit
    val fill : Context -> unit
    val lineTo : Context -> int -> int -> unit
    val getStrokeStyle : Context -> string
    val beginPath : Context -> unit
    val fillStyle : Context -> string -> unit
    val getFillStyle : Context -> string
    val canvasHeight : Context -> int
    val arc : Context -> int -> int -> real -> real -> real -> bool -> unit
    val getLineWidth : Context -> string
    val canvasWidth : Context -> int
    val setStrokeStyle : Context -> string -> unit
    val setFillStyle : Context -> string -> unit
    val setLineWidth : Context -> real -> unit

end