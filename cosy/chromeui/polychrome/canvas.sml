structure Canvas : CANVAS =
struct
    local open jsffi in
    
    datatype Context = Context of fptr

    fun getContext (DOM.HTMLElement e) context = Context (exec_js_r e "getContext" [arg.string context])
    fun beginPath (Context c) = exec_js c "beginPath" []
    fun setFillStyle (Context c) style = exec_js_set c "fillStyle" [arg.string style]
    fun getFillStyle (Context c) = exec_js_get c "fillStyle" []
    fun setStrokeStyle (Context c) style = exec_js_set c "strokeStyle" [arg.string style]
    fun getStrokeStyle (Context c) = exec_js_get c "strokeStyle" []
    fun getLineWidth (Context c) = exec_js_get c "lineWidth" []
    fun setLineWidth (Context c) width = exec_js_set c "lineWidth" [arg.real width]
    fun moveTo (Context c) x y = exec_js c "moveTo" [arg.int x, arg.int y]
    fun lineTo (Context c) x y = exec_js c "lineTo" [arg.int x, arg.int y]
    fun stroke (Context c) = exec_js c "stroke" []
    fun arc (Context c) x y radius startAng endAng clockwise = exec_js c "arc" [arg.int x, arg.int y, arg.real radius, arg.real startAng, arg.real endAng, arg.bool clockwise]
    fun fill (Context c) = exec_js c "fill" []
    fun fillRect (Context c) x y w h = exec_js c "fillRect" [arg.int x, arg.int y, arg.int w, arg.int h]
    fun fillStyle (Context c) style = exec_js_set c "fillStyle" [arg.string style]
    fun canvasWidth (Context c) = Option.valOf (Int.fromString (exec_js_get c "canvas.width" []))
    fun canvasHeight (Context c) = Option.valOf (Int.fromString (exec_js_get c "canvas.height" []))
    
    end
end