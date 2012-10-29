structure Petri_Data =    
struct
  datatype data = COPY | SWITCH | BUF0 | BUF1
  val default = COPY
  fun int_of d    = case d of COPY => 0 | SWITCH => 1 | BUF0 => 2 | BUF1 => 3
  fun string_of d = case d of COPY => "COPY" | SWITCH => "SWITCH" | BUF0 => "BUF0" | BUF1 => "BUF1"
  fun of_string s = case s of "COPY" => SOME COPY | "SWITCH" => SOME SWITCH | "BUF0" => SOME BUF0 | "BUF1" => SOME BUF1
                            | _ => NONE
end;

structure Petri_VertexData = EnumUnifiableData(Petri_Data)
structure Petri_VertexComponentData_Param = EnumComponentData(Petri_Data)
