structure DOM : DOM =
struct

    local open jsffi in
    
    datatype HTMLElement = HTMLElement of fptr
    datatype HTMLCollection = HTMLCollection of fptr
    datatype Document = Document of fptr
    datatype Window = Window of fptr
    datatype EventListener = EventListener of string;
    datatype Timeout = Timeout of string;
    datatype Interval = Interval of string;
    datatype Event = Event of fptr
    datatype EventType = click | change | keypress | keyup | mouseover | mouseout | mousemove
    datatype EventCallback = EventCallback of Event -> unit
    datatype TimerCallback = TimerCallback of unit -> unit;
    
    fun parse_element e = case e of "null" => NONE | x => SOME (HTMLElement x)
    fun parse_element_list l =
            String.tokens (fn (#",") => true | _ => false) l
            |> map (fn (x) => HTMLElement x)

    fun string_of_eventtype click = "click"
      | string_of_eventtype change = "change"
      | string_of_eventtype keypress = "keypress"
      | string_of_eventtype keyup = "keyup"
      | string_of_eventtype mouseover = "mouseover"
      | string_of_eventtype mouseout = "mouseout"
      | string_of_eventtype mousemove = "mousemove"
      
    (* we'll keep event callbacks here *)
    val eventCallbackTab = Unsynchronized.ref (Tab.empty : (HTMLElement * EventType * EventCallback) Tab.T)
    fun handle_event id event = let
            val (_, _, EventCallback f) = (Tab.get (!eventCallbackTab) (Name.mk id)) handle UNDEF => (raise Error ()) (* TODO, more informative error?*)
            val _ = f (Event event)
            val _ = Memory.removeReference event (* clean up the memory *)
            val _ = ready ()
        in () end
    
    (* we'll keep timeout and interval callbacks here *)
    val timerCallbackTab = Unsynchronized.ref (Tab.empty : (TimerCallback * fptr * int) Tab.T)
    fun handle_timeout f_id = let
            val (TimerCallback f, _, _) = Tab.get (!timerCallbackTab) (Name.mk f_id) handle UNDEF => (raise Error ()) (* TODO, more informative error?*)
            val _ = f ()
            (* clean up *)
            val _ = (timerCallbackTab := (Tab.delete (Name.mk f_id) (!timerCallbackTab)))
            val _ = Memory.removeReference f_id
            val _ = ready ()
        in () end
    fun handle_interval f_id = let
            val (TimerCallback f, _, _) = Tab.get (!timerCallbackTab) (Name.mk f_id) handle UNDEF => (raise Error ()) (* TODO, more informative error?*)
            val _ = ready ()
        in f () end
    
    val document = Document "document|"
    val window = Window "window|"
    
    fun fptr_of_HTMLElement (HTMLElement fptr) = fptr
    
    (* window methods *)
    fun alert (Window w) message = exec_js w "alert" [arg.string message]
    (* document methods *)
    fun getElementById (Document d) id = parse_element (exec_js_r d "getElementById" [arg.string id])
    fun getElementsByTagName (Document d) tag = HTMLCollection (exec_js_r d "getElementsByTagName" [arg.string tag])
    fun createElement (Document d) tag = HTMLElement (exec_js_r d "createElement" [arg.string tag])
    fun createTextNode (Document d) text = HTMLElement (exec_js_r d "createTextNode" [arg.string text])
    (* element methods *)
    fun childNodes (HTMLElement e) = parse_element_list (exec_js_get e "childNodes" [])
    fun parentNode (HTMLElement e) = parse_element (exec_js_get e "parentNode" [])
    fun firstChild (HTMLElement e) = parse_element (exec_js_get e "firstChild" [])
    fun lastChild (HTMLElement e) = parse_element (exec_js_get e "lastChild" [])
    fun nextSibling (HTMLElement e) = parse_element (exec_js_get e "nextSibling" [])
    fun previousSibling (HTMLElement e) = parse_element (exec_js_get e "previousSibling" [])
    fun setInnerHTML (HTMLElement e) value = exec_js_set e "innerHTML" [arg.string value]
    fun getInnerHTML (HTMLElement e) = exec_js_get e "innerHTML" []
    fun setValue (HTMLElement e) value = exec_js_set e "value" [arg.string value]
    fun getValue (HTMLElement e) = exec_js_get e "value" []
    fun getAttribute (HTMLElement e) attr = exec_js_r e "getAttribute" [arg.string attr]
    fun setAttribute (HTMLElement e) (attr, value) = exec_js e "setAttribute" [arg.string attr, arg.string value]
    fun removeAttribute (HTMLElement e) attr = exec_js e "removeAttribute" [arg.string attr]
    fun appendChild (HTMLElement parent) (HTMLElement child) = exec_js parent "appendChild" [arg.reference child]
    fun removeChild (HTMLElement parent) (HTMLElement child) = exec_js parent "removeChild" [arg.reference child]
    fun replaceChild (HTMLElement parent) (HTMLElement child_new) (HTMLElement child_old) = exec_js parent "replaceChild" [arg.reference child_new, arg.reference child_old]
    fun setStyle (HTMLElement e) (attr, value) = exec_js_set e ("style."^attr) [arg.string value]
    fun getStyle (HTMLElement e) attr = exec_js_get e ("style."^attr) []
    
    (* extras *)
    fun getHTMLCollectionItem (HTMLCollection x) n = HTMLElement (exec_js_get x (Int.toString n) [])

    (* events *)
    fun getClientX (Event e) = Option.valOf (Int.fromString (exec_js_get e "clientX" []))
    fun getClientY (Event e) = Option.valOf (Int.fromString (exec_js_get e "clientY" []))
    fun addEventListener_ (HTMLElement e) et f add_function_reference = let
            val callback = "val _ = DOM.handle_event {id} {arg} ;"
            val id = add_function_reference callback
            val entry = (HTMLElement e, et, f)
            val _ = (eventCallbackTab := Tab.ins (Name.mk id, entry) (!eventCallbackTab))
            val _ = exec_js e "addEventListener" [arg.string (string_of_eventtype et), arg.reference id, arg.bool false]
        in EventListener id end
    fun addEventListener e et f = addEventListener_ e et f Memory.addFunctionReference
    fun addEventListenerOW e et f = addEventListener_ e et f Memory.addFunctionReferenceOW
    fun removeEventListener (EventListener id) = let
            val (HTMLElement e, et, _) = (Tab.get (!eventCallbackTab) (Name.mk id))
                          handle UNDEF => (raise PolyChrome.DOMExn "Undefined listener");
            val _ = (eventCallbackTab := (Tab.delete (Name.mk id) (!eventCallbackTab)))
            val _ = exec_js e "removeEventListener" [arg.string (string_of_eventtype et), arg.reference id, arg.bool false]
            val _ = Memory.removeReference id
        in () end
    
    (* timers *)
    fun setTimeout (Window w) f time = let
            val callback = "val _ = DOM.handle_timeout {id} ;"
            val f_id = Memory.addFunctionReference callback
            val timeout_id = exec_js_r w "setTimeout" [arg.reference f_id, arg.int time]
            val entry = (f, timeout_id, time)
            val _ = (timerCallbackTab := Tab.ins (Name.mk f_id, entry) (!timerCallbackTab))
        in Timeout f_id end
    fun clearTimeout (Window w) (Timeout f_id) = let
            val contains = (Tab.contains (!timerCallbackTab) (Name.mk f_id))
            val _ = case contains of true => (let
                val (_, timeout_id, time) = (Tab.get (!timerCallbackTab) (Name.mk f_id))
                          handle UNDEF => (raise Error ());
                val _ = exec_js w "clearTimeout" [arg.string timeout_id]
                (* cleanup *)
                val _ = (timerCallbackTab := (Tab.delete (Name.mk f_id) (!timerCallbackTab)))
                val _ = Memory.removeReference f_id
                in () end
            )
            | false => ()
        in () end
        
    fun setInterval (Window w) f time = let
            val callback = "val _ = DOM.handle_interval {id} ;"
            val f_id = Memory.addFunctionReference callback
            val interval_id = exec_js_r w "setInterval" [arg.reference f_id, arg.int time]
            val entry = (f, interval_id, time)
            val _ = (timerCallbackTab := Tab.ins (Name.mk f_id, entry) (!timerCallbackTab))
        in Timeout f_id end
    fun clearInterval (Window w) (Interval f_id) = let
            val contains = (Tab.contains (!timerCallbackTab) (Name.mk f_id))
            val _ = case contains of true => (let
                val (_, interval_id, time) = (Tab.get (!timerCallbackTab) (Name.mk f_id))
                          handle UNDEF => (raise Error ());
                val _ = exec_js w "clearInterval" [arg.string interval_id]
                (* cleanup *)
                val _ = (timerCallbackTab := (Tab.delete (Name.mk f_id) (!timerCallbackTab)))
                val _ = Memory.removeReference f_id
                in () end
            )
            | false => ()
        in () end
    
    end
end

structure JS =
struct

  local open jsffi in
  
  structure arg = jsffi.arg

  (* generic JS object getters/setters *)
  fun get (obj:fptr) attr = exec_js_get obj attr []
  fun set (obj:fptr) (attr, value) = exec_js_set obj attr [value]
  
  (* calling js functions given a fptr to that function *)
  fun call fptr args = (jsffi.exec_js fptr "" args; jsffi.ready ())
  fun call_r fptr args = let
      val r = jsffi.exec_js_r fptr "" args
      val _ = jsffi.ready ()
    in r end
  
  end
end