(*  Title:      Pure/ML-Systems/universal.ML
    Author:     Makarius

Universal values via tagged union.  Emulates structure Universal
from Poly/ML 5.1.
*)

signature UNIVERSAL =
sig
  type universal
  type 'a tag
  val tag: unit -> 'a tag
  val tagIs: 'a tag -> universal -> bool
  val tagInject: 'a tag -> 'a -> universal
  val tagProject: 'a tag -> universal -> 'a
end;

structure Universal: UNIVERSAL =
struct

type universal = exn;

datatype 'a tag = Tag of
 {is: universal -> bool,
  inject: 'a -> universal,
  project: universal -> 'a};

fun tag () =
  let exception Universal of 'a in
   Tag {
    is = fn Universal _ => true | _ => false,
    inject = Universal,
    project = fn Universal x => x}
  end;

fun tagIs (Tag {is, ...}) = is;
fun tagInject (Tag {inject, ...}) = inject;
fun tagProject (Tag {project, ...}) = project;

end;

