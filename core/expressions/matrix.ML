(**
  * A simple matrix implementation. Primarily for doing Guass elimination.
  *)

signature MATRIX =
sig
  type T
  (* The type of matrix elements; see coeff.ML *)
  structure F : FIELD

  (* Takes a size (rows,cols) and a function from positions to elements *)
  val mk : (int * int) -> (int * int -> F.T) -> T
  (* Takes a list of rows, each row being a list of elements *)
  val from_lists : F.T list list -> T

  (* Perform Guassian elimination for the given number of unknowns.
   *
   * Will return NONE if the system has no solutions; otherwise the
   * result will be in reduced row echelon form. *)
  val gauss : int -> T -> T option

  (* Raised by calling gauss_update on a matrix not in
   * reduced row echelon form (RREF) *)
  exception not_in_rref_exp of T

  (* Try to add a single additional row to a matrix already in reduced
   * row echelon form (RREF).
   *
   * Like with gauss, will return NONE if the system becomes inconsistent,
   * otherwise the result will be in RREF.
   *
   * Raises not_in_rref_exp if given a matrix not in RREF. *)
  val gauss_update : int -> (int -> F.T) -> T -> T option

  (* Get a single element; takes a position in (row,col) form *)
  val get : T -> int * int -> F.T

  val pretty : T -> Pretty.T
  val print : T -> unit
end

functor Matrix(Field: FIELD) =
struct
  structure F = Field
  type T = (Field.T Vector.vector) Vector.vector

  fun mk (rows,cols) f =
    Vector.tabulate (rows, fn i => Vector.tabulate (cols, fn j => f(i,j)))

  val empty = mk (0,0) (K Field.zero)

  fun from_lists lists = Vector.fromList (map Vector.fromList lists)

  fun get m (i,j) = Vector.sub(Vector.sub(m,i),j)

  fun num_rows m = Vector.length m
  fun num_cols m = case Vector.find (K true) m of SOME r => Vector.length r | NONE => 0

  exception not_in_rref_exp of T

  fun find_pivot row = Vector.findi (fn (_,e) => not (Field.is_zero e)) row
  fun mult_row d row = Vector.map (fn e => Field.mult d e) row
  fun subtr_row row1 row2 = Vector.mapi (fn (i,e) => Field.subtr e (Vector.sub (row2,i))) row1

  exception inconsistent_exp of unit

  fun gauss_update' free new_row m = let
    val (row_opt, mlist) =
      Vector.foldl (fn (current_row, (row_opt,accum)) => (
        case row_opt
          of SOME row =>
            let
              val current_pi = case find_pivot current_row
                                 of SOME (x,y) => x | NONE => raise not_in_rref_exp m
            in
             case find_pivot row
               of SOME (pi, pval) =>
                    if pi >= free
                    then raise inconsistent_exp ()
                    else
                      if pi < current_pi then
                        (NONE, current_row :: mult_row (Field.divide Field.one pval) row :: accum)
                      else if pi = current_pi then
                        (SOME (subtr_row row (mult_row pval current_row)), current_row :: accum)
                      else (* pi > current_pi *)
                        (SOME row, current_row :: accum)
                | NONE => (NONE, current_row :: accum)
            end
           | NONE => (NONE, current_row :: accum)
      )) (SOME (new_row), []) m
  in
    SOME (Vector.fromList (rev (
      case row_opt
        of SOME row => (
              case find_pivot row
                of SOME (pi,pval) =>
                     if pi >= free
                     then raise inconsistent_exp ()
                     else mult_row (Field.divide Field.one pval) row :: mlist
                 | NONE => mlist (* don't add a row if it's all zeros *)
           )
         | NONE => mlist
    )))
  end
  handle inconsistent_exp () => NONE

  fun gauss_update free rowf m = gauss_update' free (Vector.tabulate (num_cols m,rowf)) m
  fun gauss_update_list free row_list m = gauss_update' free (Vector.fromList row_list) m

  fun gauss free = Vector.foldl (fn (row, m_opt) => (
      case m_opt
        of SOME m => gauss_update' free row m
         | NONE => NONE
    )) (SOME empty)
 
  fun pretty m =
    Pretty.chunks (Vector.foldr (fn (row,rs) =>
      Pretty.list "[" "]" (Vector.foldr (fn (e,cs) =>
        Field.pretty e :: cs
      ) [] row) :: rs
    ) [] m)

  fun print m = Pretty.writeln (pretty m)
end

structure RatMatrix = Matrix(Rational);

TextIO.print "find me";

val matr = RatMatrix.from_lists (map (map Rational.mk) [
  [(1,1), (1,1), (2,1)],
  [(1,1), (1,1), (2,1)]
]);

RatMatrix.print (the (RatMatrix.gauss 2 matr));
