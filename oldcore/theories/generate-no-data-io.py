#!/usr/bin/env python3

# eg: ./generate-no-data-theory.py {0} ghzw GHZ W TICK ZERO > ghzw/theory.ML

import sys

code_name = sys.argv[1]
types = sys.argv[3:]
maxlen = len(max(types,key=len))

print('(* Generated using {0} *)'.format(' '.join(sys.argv)))

print('''structure {0}_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type nvdata = {0}_Data.nvdata
  type edata = {0}_Data.edata

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils
    type data = nvdata
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun get_type t =
          (case to_lower t'''.format(code_name))
pad = ' '*(maxlen - len(types[0]))
print('             of "{2}" {3}=> {0}_Data.{1}'.format(code_name,types[0],types[0].lower(),pad))
for t in types[1:]:
    pad = ' '*(maxlen - len(t))
    print('              | "{2}" {3}=> {0}_Data.{1}'.format(code_name,t,t.lower(),pad))
print('''              | _      => raise bad_input_exp ("Unknown vertex type "^t,""))
    fun input (Json.String t) = get_type t
      | input (Json.Object obj) =
         (get_type (get_string obj "type")
            handle bad_input_exp (m,l) =>
              raise bad_input_exp (m, prepend_prop "type" l))
      | input _ = raise bad_input_exp ("Expected string","type")
  end
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils
    type data = nvdata'''.format(code_name))
pad = ' '*(maxlen - len(types[0]))
print('    fun typestr {0}_Data.{1} {2}= "{1}"'.format(code_name,types[0],pad))
for t in types[1:]:
    pad = ' '*(maxlen - len(t))
    print('      | typestr {0}_Data.{1} {2}= "{1}"'.format(code_name,t,pad))
print('''    fun output d = Json.mk_record [("type",typestr d)]
  end
  structure EDataInputJSON = InputUnitJSON
  structure EDataOutputJSON = OutputUnitJSON

  structure DotStyle : DOT_STYLE =
  struct
    type nvdata = nvdata
    (* TODO: alter these: *)'''.format(code_name))
print('    fun style_for_ivertex_data {0}_Data.{1} ='.format(code_name,types[0]))
print('          "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"')
for t in types[1:]:
    print('      | style_for_ivertex_data {0}_Data.{1} ='.format(code_name,t))
    print('          "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"')
print('''  end
end

(* Use this for convenience if you don't need annotations *)
structure {0}_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = {0}_Theory
  structure GraphComponentDataIO = {0}_ComponentDataIO
)
'''.format(code_name))

sys.stderr.write("Don't forget to set the Dot styles\n")

