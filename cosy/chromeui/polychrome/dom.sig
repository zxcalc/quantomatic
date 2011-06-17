signature DOM =
sig

	datatype Event = Event of jsffi.fptr
	datatype Window = Window of jsffi.fptr
	datatype EventListener = EventListener of string
	datatype HTMLElement = HTMLElement of jsffi.fptr
	datatype Timeout = Timeout of string
	datatype Interval = Interval of string
	datatype TimerCallback = TimerCallback of unit -> unit
	datatype EventType =
		change
	  | click
	  | keypress
	  | keyup
	  | mousemove
	  | mouseout
	  | mouseover
	datatype HTMLCollection = HTMLCollection of jsffi.fptr
	datatype EventCallback = EventCallback of Event -> unit
	datatype Document = Document of jsffi.fptr
	
	val document : Document
	val window : Window
	
	val fptr_of_HTMLElement : HTMLElement -> jsffi.fptr
	val string_of_eventtype : EventType -> string
	val parse_element : string -> HTMLElement option
	val parse_element_list : string -> HTMLElement list
	
	val getElementById : Document -> string -> HTMLElement option
	val getElementsByTagName : Document -> string -> HTMLCollection
	val parentNode : HTMLElement -> HTMLElement option
	val lastChild : HTMLElement -> HTMLElement option
	val removeChild : HTMLElement -> HTMLElement -> unit
	val previousSibling : HTMLElement -> HTMLElement option
	val getStyle : HTMLElement -> string -> string
	val replaceChild : HTMLElement -> HTMLElement -> HTMLElement -> unit
	val nextSibling : HTMLElement -> HTMLElement option
	val setStyle : HTMLElement -> string * string -> unit
	val firstChild : HTMLElement -> HTMLElement option
	val appendChild : HTMLElement -> HTMLElement -> unit
	val childNodes : HTMLElement -> HTMLElement list
	val createElement : Document -> string -> HTMLElement
	val createTextNode : Document -> string -> HTMLElement
	val getAttribute : HTMLElement -> string -> string
	val setAttribute : HTMLElement -> string * string -> unit
	val removeAttribute : HTMLElement -> string -> unit
	val getInnerHTML : HTMLElement -> string
	val setInnerHTML : HTMLElement -> string -> unit
	val getValue : HTMLElement -> string
	val setValue : HTMLElement -> string -> unit
	val getHTMLCollectionItem : HTMLCollection -> int -> HTMLElement
	
	val addEventListener :
	   HTMLElement -> EventType -> EventCallback -> EventListener
	val addEventListenerOW :
	   HTMLElement -> EventType -> EventCallback -> EventListener
	val removeEventListener : EventListener -> unit
	val setInterval : Window -> TimerCallback -> int -> Timeout	
	val clearInterval : Window -> Interval -> unit
	val setTimeout : Window -> TimerCallback -> int -> Timeout
	val clearTimeout : Window -> Timeout -> unit
	
	val handle_event : string -> jsffi.fptr -> unit
	val handle_interval : string -> unit
	val handle_timeout : string -> unit
	
	val getClientX : Event -> int
	val getClientY : Event -> int	
	
	val alert : Window -> string -> unit

end