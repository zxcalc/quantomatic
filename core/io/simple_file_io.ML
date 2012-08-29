(*This signature provides easy access to reading
and writing to a file*)
signature MJB_FILE =
sig
	exception FileNotForReading of string (*Thrown if the wrong file is used in a function*)
	exception FileNotForWriting of string (*The string is the path name in full*)
	exception FileClosed of string (*Raised when an operation is performed on a closed
	file, with the exception of close itself.*)
	(*The type of a file descriptor*)
	type file
	(*Open a file to read from it*)
	val read : string -> file
	(*Create a file to write to, or erase all inside the file if it already exists*)
	val write : string -> file
	(*Try to append to the end of a file. The file is created if it does not exist*)
	val append : string -> file
	(*returns SOME(ln), where ln is the next line of input in the stream strm.
	Specifically, ln returns all characters from the current position up to and
	including the next newline (#"\n") character. If it detects an end-of-stream
	before the next newline, it returns the characters read appended with a newline.
	Thus, ln is guaranteed to always be new-line terminated (and thus nonempty).
	If the current stream position is the end-of-stream, then it returns NONE.
	It raises Size if the length of the line exceeds the length of the longest string. *)
	val readLine : file -> string option
	(*Write a string to a file*)
	val writeStr : file -> string -> unit
	(*Close the given file*)
	val close : file -> unit
end;

(*A simple implementation. This is NOT thread safe*)
structure MJB_File :> MJB_FILE =
struct
	exception FileNotForReading of string (*Thrown if the wrong file is used in a function*)
	exception FileNotForWriting of string (*The string is the path name in full*)
	exception FileClosed of string (*Raised when an operation is performed on a closed
	file, with the exception of close itself.*)
	local open TextIO; open Thread; in
	(*The type of a file descriptor*)
	datatype fileStream = O of outstream | I of instream
	(*Boolean is for whether or not it is open*)
	type file = fileStream * string * (bool Unsynchronized.ref)
	
	fun getPath path = if (OS.Path.isAbsolute path) then path else (OS.FileSys.getDir()) ^ path
	(*Open a file to read from it*)
	fun read path = (I (TextIO.openIn path),getPath path,Unsynchronized.ref true) 
	(*Create a file to write to, or erase all inside the file if it already exists*)
	fun write path = (O (TextIO.openOut path),getPath path,Unsynchronized.ref true)
	(*Try to append to the end of a file. The file is created if it does not exist*)
	fun append path = (O (TextIO.openAppend path),getPath path,Unsynchronized.ref true)
	(*returns SOME(ln), where ln is the next line of input in the stream strm.
	Specifically, ln returns all characters from the current position up to and
	including the next newline (#"\n") character. If it detects an end-of-stream
	before the next newline, it returns the characters read appended with a newline.
	Thus, ln is guaranteed to always be new-line terminated (and thus nonempty).
	If the current stream position is the end-of-stream, then it returns NONE.
	It raises Size if the length of the line exceeds the length of the longest string. *)
	fun readLine (I stream,_,Unsynchronized.ref true) = TextIO.inputLine stream
          | readLine (_,p,Unsynchronized.ref true) = raise FileNotForReading p
	  | readLine (_,p,_) = raise FileClosed p
	(*Write a string to a file*)
	fun writeStr (O stream,_,Unsynchronized.ref true) str = TextIO.output (stream,str)
	  | writeStr (_,p,Unsynchronized.ref true) _ = raise FileNotForWriting p
          | writeStr (_,p,_) _ = raise FileClosed p
	(*Close the given file*)
	fun close (I stream,_,t) = (t:=false;closeIn stream)
          | close (O stream,_,t) = (t:=false;closeOut stream)
	end
end;
