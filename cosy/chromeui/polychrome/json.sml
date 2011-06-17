structure JSON : JSON
= struct

    structure Name = SStrName;
    structure Tab = Name.NTab;

    (*datatype name = Root | Name of string;*)
    datatype T = Object of (T Tab.T)
              | List of (T list)
              | String of string
              | Int of int
              | Bool of bool
              | Real of real
              | Null;

    val empty = Object Tab.empty;
    

    exception notobj_exn of unit;
    
    (** escapes ", \ and \n **)
    fun escape s =
      let
        fun helper #"\"" = [#"\\", #"\""]
          | helper #"\\" = [#"\\", #"\\"]
          | helper #"\n" = [#"\\", #"n"]
          | helper x = [x]
      in
        String.implode
          (fold_rev (curry op@) (map helper (String.explode s)) [])
      end
    
    (*replaces ~ to - *)
    fun convert_minus s = String.implode
            (map (fn (#"~") => #"-" | x => x) (String.explode s))
    
    
    (*table functions *)
    fun add (name:string, value) (Object tab) =
        let
            val (_, t) = Tab.add (Name.mk name, value) tab;
        in
            Object t
        end
      | add _ _ = raise notobj_exn ()
    
    fun update (n:string, v) (Object tab) =
            (Object (Tab.update ((Name.mk n), v) tab))
      | update _ _ = raise notobj_exn ()
    
    fun delete (n:string) (Object tab) =
            (Object (Tab.delete (Name.mk n) tab))
      | delete _ _ = raise notobj_exn ()
      
    fun get (Object tab) (n:string) =
            Tab.get tab (Name.mk n)
      | get _ _ = raise notobj_exn ()
      
    fun lookup (Object tab) (n:string) =
            Tab.lookup tab (Name.mk n)
      | lookup _ _ = raise notobj_exn ()
    
    

    fun enc_name (name) = "\"" ^ (Name.string_of_name name) ^ "\":"
    and enc_value (String value) = "\"" ^ escape value ^ "\""
      | enc_value (Int value) = convert_minus (Int.toString value)
      | enc_value (Bool value) = Bool.toString value
      | enc_value (Real value) = Real.toString value
      | enc_value (List value) = enc_list value
      | enc_value (Null) = "null"
      | enc_value (obj) = (encode obj)
    and enc_list [] = "[]"
      | enc_list l =
        let
            val e = fold_rev (fn a => fn b => (enc_value a) ^ "," ^ b) l ""
        in
            "[" ^ String.substring(e, 0, (String.size e)-1) ^ "]"
        end
    and enc_1 (name, value) x = (enc_name name) ^ (enc_value value) ^ "," ^ x
    and encode (Object tab) =
        let
            val e = Tab.fold enc_1 tab "";
        in
            "{" ^ String.substring(e, 0, (String.size e)-1) ^ "}"
        end
end;

(*
val x = JSON.empty
|> JSON.add ("type", JSON.Int 2)
|> JSON.add ("f", (JSON.String "getElementById"))
|> JSON.add ("arg1", (JSON.String "foo"))
|> JSON.add ("arg2", (JSON.Bool true))
|> JSON.update ("arg1", JSON.Null);

val y = JSON.empty;
val y = JSON.add ("test1", (JSON.String "foo")) y;
val y = JSON.add ("test2", (JSON.Bool true)) y;

val x = JSON.add ("obj", y) x;

TextIO.print (JSON.encode x);
val test = JSON.encode x
*)
