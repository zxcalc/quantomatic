structure GHZW_Data =    
struct
  datatype data = GHZ | W | TICK | ZERO;
  val default = GHZ
  fun int_of d    = case d of GHZ => 0 | W => 1 | TICK => 2 | ZERO => 3
  fun string_of d = case d of GHZ => "GHZ" | W => "W" | TICK => "TICK" | ZERO => "ZERO"
  fun of_string s = case s of "GHZ" => SOME GHZ | "W" => SOME W | "TICK" => SOME TICK | "ZERO" => SOME ZERO
                            | _ => NONE
end;

structure GHZW_VertexData = EnumUnifiableData(GHZW_Data)
structure GHZW_VertexComponentData_Param = EnumComponentData(GHZW_Data)
