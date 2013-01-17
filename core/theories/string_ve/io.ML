structure StringVE_InputJSON : INPUT_JSON =
struct
  open JsonInputUtils
  type data = StringData.data
  fun input (Json.Object obj) =
        (case Json.lookup obj "value"
           of SOME(Json.String(s)) => s
            | _  => raise bad_input_exp ("Expected: field \"value\" containing string", "type"))
    | input _ = raise bad_input_exp ("Expected object","type")
end

structure StringVE_OutputJSON : OUTPUT_JSON =
struct
  open JsonOutputUtils
  type data = StringData.data
  fun output s = Json.mk_record [("type", "string"), ("value", s)]
end