signature GRAPH_COMPONENT_DATA_IO =
sig
  type nvdata
  type edata

  structure IVDataInputJSON : INPUT_JSON
    where type data = nvdata
  structure IVDataOutputJSON : OUTPUT_JSON
    where type data = nvdata
  structure EDataInputJSON : INPUT_JSON
    where type data = edata
  structure EDataOutputJSON : OUTPUT_JSON
    where type data = edata
  structure DotStyle : DOT_STYLE
    where type nvdata = nvdata
end
