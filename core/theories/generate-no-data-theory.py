#!/usr/bin/env python3

# eg: ./generate-no-data-theory.py GHZW ghzw GHZ W TICK ZERO > ghzw/theory.ML

import sys

code_name = sys.argv[1]
pretty_name = sys.argv[2]
types = sys.argv[3:]
maxlen = len(max(types,key=len))

print('''structure {0}_Data =
struct
  val pretty_theory_name = Pretty.str "{1}"
  type psubst = unit
  type subst  = psubst

  datatype nvdata = {2}
  val default_nvdata = {3}

  val nvdata_typestrings = ["{4}"]
  fun default_nvdata_of_typestring s ='''
      .format(code_name, pretty_name, " | ".join(types), types[0], '","'.join(types)))
pad = ' '*(maxlen - len(types[0]))
print('    case s of "{0}" {1}=> {0}'.format(types[0],pad))
for t in types[1:]:
    pad = ' '*(maxlen - len(t))
    print('            | "{0}" {1}=> {0}'.format(t,pad))
print('            | _ => raise unknown_typestring_exp s')
print('  fun typestring_of_nvdata d =')
pad = ' '*(maxlen - len(types[0]))
print('    case d of {0} {1}=> "{0}"'.format(types[0],pad))
for t in types[1:]:
    pad = ' '*(maxlen - len(t))
    print('            | {0} {1}=> "{0}"'.format(t,pad))
print('''
  fun nvdata_eq (a,b) = a = b

  val pretty_nvdata = Pretty.str o typestring_of_nvdata
  
  fun match_nvdata (x,y) () = if nvdata_eq (x,y) then SOME () else NONE

  fun subst_in_nvdata _ = I

  open EmptyEdgeData

  fun init_psubst_from_data _ = ()
  val solve_psubst = Seq.single
end

structure {0}_Theory = GraphicalTheory(structure Data = {0}_Data)
'''.format(code_name))

