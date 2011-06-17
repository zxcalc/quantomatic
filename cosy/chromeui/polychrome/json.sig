signature JSON =
sig

	exception notobj_exn of unit

	structure Tab : NAME_TAB
	structure Name : SSTR_NAMES

	datatype T =
		Bool of bool
	  | Int of int
	  | List of T list
	  | Null
	  | Object of T Tab.T
	  | Real of real
	  | String of string
	
	val empty : T
	val update : string * T -> T -> T
	val add : string * T -> T -> T
	val lookup : T -> string -> T option
	val get : T -> string -> T
	val delete : string -> T -> T
	val encode : T -> string

end