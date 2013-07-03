(**
  * A simple matrix implementation. Primarily for doing Guass elimination.
  *)

signature MATRIX =
sig
  type T
  (* The type of matrix elements; see coeff.ML *)
  structure Field : FIELD

  val num_rows : T -> int
  val num_cols : T -> int

  val empty : T

  (* adds a zero column at the given index *)
  val insert_zero_column : int -> T -> T

  (* Takes a size (rows,cols) and a function from positions to elements *)
  val mk : (int * int) -> (int * int -> Field.T) -> T
  (* Takes a list of rows, each row being a list of elements *)
  val from_lists : Field.T list list -> T

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
  val gauss_update : int -> Field.T list -> T -> T option
  
  (* As before, but apply the given function to the last column to reduce the
   * field element, e.g. modulo 2. *)
  val gauss_update_reduced : (Field.T -> Field.T) -> int -> Field.T list -> T -> T option


  (* Get a single element; takes a position in (row,col) form *)
  val get : T -> int * int -> Field.T

  val pretty : T -> Pretty.T
  val print : T -> unit
end

functor Matrix(Field: FIELD) : MATRIX =
struct
  structure F = Field
  type T = (Field.T Vector.vector) Vector.vector
  structure Field = Field

  fun mk (rows,cols) f =
    Vector.tabulate (rows, fn i => Vector.tabulate (cols, fn j => f(i,j)))

  val empty = mk (0,0) (K Field.zero)

  fun from_lists lists = Vector.fromList (map Vector.fromList lists)

  fun get m (i,j) = Vector.sub(Vector.sub(m,i),j)

  fun num_rows m = Vector.length m
  fun num_cols m = case Vector.find (K true) m of SOME r => Vector.length r | NONE => 0

  exception not_in_rref_exp of T

  fun find_pivot row = Vector.findi (fn (_,e) => not (Field.is_zero e)) row

  fun insert_zero i row = Vector.tabulate (Vector.length row + 1,
    fn j => (
      if j < i then Vector.sub (row, j)
      else if j = i then Field.zero
      else Vector.sub (row, j - 1)
    ))

  fun insert_zero_column i = Vector.map (insert_zero i)

  fun mult_row d row = Vector.map (fn e => Field.mult d e) row
  fun subtr_row row1 row2 = Vector.mapi (fn (i,e) => Field.subtr e (Vector.sub (row2,i))) row1

  exception inconsistent_exp of unit

  fun map_last f vec =
    case Vector.length vec
      of 0   => vec
       | len => Vector.update (vec, len - 1, f (Vector.sub (vec, len-1)))

  fun gauss_update' reducef free new_row m = let
    val rf = map_last reducef
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
                        (NONE, current_row :: rf (mult_row (Field.divide Field.one pval) row) :: accum)
                      else if pi = current_pi then
                        (SOME (rf (subtr_row row (mult_row pval current_row))), current_row :: accum)
                      else (* pi > current_pi *)
                        (SOME row, current_row :: accum)
                | NONE => (NONE, current_row :: accum)
            end
           | NONE => (NONE, current_row :: accum)
      )) (SOME (rf new_row), []) m
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

  (*fun gauss_updatef free rowf m = gauss_update' I free (Vector.tabulate (num_cols m,rowf)) m*)
  fun gauss_update free row_list m = gauss_update' I free (Vector.fromList row_list) m

  fun gauss_update_reduced reducef free row_list m =
    gauss_update' reducef free (Vector.fromList row_list) m

  fun gauss free = Vector.foldl (fn (row, m_opt) => (
      case m_opt
        of SOME m => gauss_update' I free row m
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

structure RationalMatrix = Matrix(Rational)

val matr = RationalMatrix.from_lists (map (map Rational.mk) [
  [(1,1), (1,1), (2,1)],
  [(1,1), (1,1), (2,1)]
])

val _ = RationalMatrix.print (the (RationalMatrix.gauss 2 matr));
