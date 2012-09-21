signature OUTPUT_JSON = OUTPUT where type T = Json.json;

functor OutputNTabJSON (
  structure Name : SNAME where type src = string
  structure OutputValue : OUTPUT_JSON
) : OUTPUT_JSON
= struct
  structure N = Name;
  open JsonUtils;
  type data = OutputValue.data N.NTab.T;

  fun update_value (n,v) = Json.update (N.dest n,OutputValue.output v)

  fun output tab =
    Json.Object (N.NTab.fold update_value tab Json.empty_obj)
end;

