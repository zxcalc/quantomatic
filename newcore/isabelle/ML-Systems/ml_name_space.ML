(*  Title:      Pure/ML-Systems/ml_name_space.ML
    Author:     Makarius

ML name space -- dummy version of Poly/ML 5.2 facility.
*)

structure ML_Name_Space =
struct

type valueVal = unit;
type typeVal = unit;
type fixityVal = unit;
type structureVal = unit;
type signatureVal = unit;
type functorVal = unit;

type T =
 {lookupVal:    string -> valueVal option,
  lookupType:   string -> typeVal option,
  lookupFix:    string -> fixityVal option,
  lookupStruct: string -> structureVal option,
  lookupSig:    string -> signatureVal option,
  lookupFunct:  string -> functorVal option,
  enterVal:     string * valueVal -> unit,
  enterType:    string * typeVal -> unit,
  enterFix:     string * fixityVal -> unit,
  enterStruct:  string * structureVal -> unit,
  enterSig:     string * signatureVal -> unit,
  enterFunct:   string * functorVal -> unit,
  allVal:       unit -> (string * valueVal) list,
  allType:      unit -> (string * typeVal) list,
  allFix:       unit -> (string * fixityVal) list,
  allStruct:    unit -> (string * structureVal) list,
  allSig:       unit -> (string * signatureVal) list,
  allFunct:     unit -> (string * functorVal) list};

val global: T =
 {lookupVal = fn _ => NONE,
  lookupType = fn _ => NONE,
  lookupFix = fn _ => NONE,
  lookupStruct = fn _ => NONE,
  lookupSig = fn _ => NONE,
  lookupFunct = fn _ => NONE,
  enterVal = fn _ => (),
  enterType = fn _ => (),
  enterFix = fn _ => (),
  enterStruct = fn _ => (),
  enterSig = fn _ => (),
  enterFunct = fn _ => (),
  allVal = fn _ => [],
  allType = fn _ => [],
  allFix = fn _ => [],
  allStruct = fn _ => [],
  allSig = fn _ => [],
  allFunct = fn _ => []};

end;
