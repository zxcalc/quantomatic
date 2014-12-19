structure TestMapsUtil =
struct
  open Testing
  (* create three distinct kinds of names *)
  structure ABCNames :> sig
    structure A : SSTR_NAME
    structure B : SSTR_NAME
    structure C : SSTR_NAME
  end =
  struct
    structure A = SStrName
    structure B = SStrName
    structure C = SStrName
  end
  
  open ABCNames
  
  structure ATab = NameTable(structure Dom = A)
  
  structure ABInj = NameInjection(
    structure Dom = A
    structure Cod = B
  )
  
  structure BAInj = NameInjection(
    structure Dom = B
    structure Cod = A
  )
  
  structure AAInj = NameInjection(
    structure Dom = A
    structure Cod = A
  )
  
  structure BBInj = NameInjection(
    structure Dom = B
    structure Cod = B
  )
  
  structure ABFn = NameFunction(
    structure Dom = A
    structure Cod = B
  )
  
  structure BCFn = NameFunction(
    structure Dom = B
    structure Cod = C
  )
  
  structure ACFn = NameFunction(
    structure Dom = A
    structure Cod = C
  )
  
  structure ABRel = NameRelation(
    structure Dom = A
    structure Cod = B
  )
  
  structure BCRel = NameRelation(
    structure Dom = B
    structure Cod = C
  )
  
  structure ACRel = NameRelation(
    structure Dom = A
    structure Cod = C
  )

  structure ASub = NameSubstitution(structure Name = A)
  structure BSub = NameSubstitution(structure Name = B)
  structure CSub = NameSubstitution(structure Name = C)

  
  
  fun catch_coerce_fail f = (f (); false)
  handle Map.bad_coercion_exp () => true
  
  fun catch_multi_fail f = (f (); false)
  handle Map.multiple_values_exp () => true
  
  fun catch_dup_fail f = (f (); false)
  handle Map.duplicate_exp () => true
  
  (* some names *)
  val (a1,a2,a3,a4,a5) = (A.mk "a1", A.mk "a2", A.mk "a3", A.mk "a4", A.mk "a5")
  val (b1,b2,b3,b4,b5) = (B.mk "b1", B.mk "b2", B.mk "b3", B.mk "b4", B.mk "b5")
  val (c1,c2,c3,c4,c5) = (C.mk "c1", C.mk "c2", C.mk "c3", C.mk "c4", C.mk "c5") 
  
end