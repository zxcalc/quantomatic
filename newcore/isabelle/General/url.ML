(*  Title:      Pure/General/url.ML
    Author:     Markus Wenzel, TU Muenchen

Basic URLs, see RFC 1738 and RFC 2396.
*)

signature URL =
sig
  datatype T =
    File of Path.T |
    RemoteFile of string * Path.T |
    Http of string * Path.T |
    Ftp of string * Path.T
  val append: T -> T -> T
  val implode: T -> string
  val explode: string -> T
end;

structure Url: URL =
struct

(* type url *)

datatype T =
  File of Path.T |
  RemoteFile of string * Path.T |
  Http of string * Path.T |
  Ftp of string * Path.T;


(* append *)

fun append (File p)            (File p') = File (Path.append p p')
  | append (RemoteFile (h, p)) (File p') = RemoteFile (h, Path.append p p')
  | append (Http (h, p))       (File p') = Http (h, Path.append p p')
  | append (Ftp (h, p))        (File p') = Ftp (h, Path.append p p')
  | append _ url = url;


(* implode *)

fun implode_path p = if Path.is_current p then "" else Path.implode p;

fun implode_url (File p) = implode_path p
  | implode_url (RemoteFile (h, p)) = "file://" ^ h ^ implode_path p
  | implode_url (Http (h, p)) = "http://" ^ h ^ implode_path p
  | implode_url (Ftp (h, p)) = "ftp://" ^ h ^ implode_path p;


(* explode *)

local

val scan_host =
  (Scan.many1 (fn s => s <> "/" andalso Symbol.is_regular s) >> implode) --|
  Scan.ahead ($$ "/" || Scan.one Symbol.is_eof);

val scan_path = Scan.many Symbol.is_regular >> (Path.explode o implode);
val scan_path_root = Scan.many Symbol.is_regular >> (Path.explode o implode o cons "/");

val scan_url =
  Scan.unless (Scan.this_string "file:" ||
    Scan.this_string "http:" || Scan.this_string "ftp:") scan_path >> File ||
  Scan.this_string "file:///" |-- scan_path_root >> File ||
  Scan.this_string "file://localhost/" |-- scan_path_root >> File ||
  Scan.this_string "file://" |-- scan_host -- scan_path >> RemoteFile ||
  Scan.this_string "file:/" |-- scan_path_root >> File ||
  Scan.this_string "http://" |-- scan_host -- scan_path >> Http ||
  Scan.this_string "ftp://" |-- scan_host -- scan_path >> Ftp;

in

fun explode_url s = Symbol.scanner "Malformed URL" scan_url (Symbol.explode s);

end;


(*final declarations of this structure!*)
val implode = implode_url;
val explode = explode_url;

end;
