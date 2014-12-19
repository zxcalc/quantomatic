signature TENSOR =
sig
  type T
  type perm = int list
  structure Semiring : GCD_SEMIRING
  structure Tab : TABLE
  sharing type T = Tab.key
  val pow : int -> int -> int
  val get : T -> (int * int) -> Semiring.T
  val dim : T -> int
  val ord : T * T -> order
  val eq : T * T -> bool
  val lower_index_count : T -> int
  val upper_index_count : T -> int
  val tensor : (int * int * int) -> Semiring.T list -> T
  val tensorf : (int * int * int) -> ((int * int) -> Semiring.T) -> T
  val id : int -> int -> T
  val to_string : T -> string
  val print : T -> unit
  val product : T -> T -> T
  val add : T -> T -> T
  val conjugate_transpose : T -> T
  val reduce : T -> (Semiring.T * Semiring.T) * T
  (*val all_perms : T -> (perm * perm) Tab.table*)
  val normalise : T -> ((Semiring.T * Semiring.T) * perm * perm) * T
  val contract : (int * int) -> T -> T
  val contract2 : (int * int) -> T -> T -> T
  val compose : T -> T -> T
  val decompose_index : int -> int -> int -> int list
  val recompose_index : int -> int list -> int
  val scalar_compare : T -> T -> (Semiring.T * Semiring.T) option
  val scalar_eq : T -> T -> bool
  val perm_scalar_eq : T -> T -> bool
end

functor TensorKey (Semiring : GCD_SEMIRING) : KEY =
struct
  type key = ((int * int * int) * Semiring.T vector)
  fun ord (  ((d,l,u),v),  ((d1,l1,u1),v1)  ) =
    case (prod_ord int_ord (prod_ord int_ord int_ord)) (  (d,(l,u))  ,  (d1,(l1,u1))  )
      of EQUAL => Vector.collate Semiring.total_ord (v,v1)
       | comp => comp
end

functor Tensor (Semiring : GCD_SEMIRING) : TENSOR = 
struct
  structure Semiring = Semiring
  structure TKey = TensorKey(Semiring)
  type T = TKey.key
  type perm = int list
  structure Tab = Table(TKey)
  
  exception negative_exponent_exn of unit
  exception wrong_size_exn of unit
  
  val ord = TKey.ord
  fun eq (t1,t2) = ord (t1,t2) = EQUAL
  
  (* fast exponentiation *)
  fun pow x 0 = 1
    | pow x 1 = x
    | pow x n = if n < 0 then raise negative_exponent_exn () else
          (if (n mod 2 = 0) then pow (x*x) (n div 2)
                            else x * (pow (x*x) (n div 2)))
  
    
    
  fun dim ((d,_,_),_) = d;
  fun lower_index_count ((_,l,_),_) = l
  fun upper_index_count ((_,_,u),_) = u
    
  fun get ((d,l,u),vect) (row,col) = Vector.sub (vect, (pow d l) * row + col);
  fun tensor (d,l,u) lst = ((d,l,u), Vector.fromList lst);
  fun tensorf (d,l,u) f = let
    val offset = pow d l
  in ((d,l,u), Vector.tabulate (pow d (u+l), (fn i => f(i div offset, i mod offset))))
  end
  
  fun id d n = tensorf (d,n,n) (fn (row,col) => if row=col then Semiring.one else Semiring.zero)
  
  
  (* reduce to scalar-canonical form. reduce T = ((a,b), T') ==> a/b T = T' *)
  fun reduce (size, vect) = let
    val c = case Vector.find (not o Semiring.is_zero) vect
              of SOME v => if Semiring.is_real v then Semiring.one
                           else Semiring.conjugate v
               | NONE   => Semiring.one
    val vect' = if Semiring.is_one c then vect else Vector.map ((curry Semiring.times) c) vect
    val n = Vector.foldr (Semiring.gcd) Semiring.zero vect'
  in if Semiring.is_zero n then ((Semiring.one, Semiring.one), (size,vect))
                           else ((c,n), (size, Vector.map (fn m =>Semiring.divide (m,n)) vect'))
  end
  
  fun get_from_product t1 (t2 as ((d2, l2, u2), v2)) (row,col) = let
    val uoff = pow d2 u2
    val loff = pow d2 l2
  in Semiring.times (get t1 (row div uoff, col div loff),
                     get t2 (row mod uoff, col mod loff))
  end
  
  fun product (t1 as ((d1, l1, u1), v1)) (t2 as ((d2, l2, u2), v2)) =
    tensorf (d1, l1+l2, u1+u2) (get_from_product t1 t2)
  
  fun add (t1 as ((d1,l1,u1),v1)) (t2 as ((d2,l2,u2),v2)) =
    if d1 = d2 andalso l1 = l2 andalso u1 = u2
    then tensorf (d1,l1,u1) (fn idx => Semiring.plus (get t1 idx, get t2 idx))
    else raise wrong_size_exn ()
    
  fun conjugate_transpose (t as ((d,l,u),_)) = tensorf (d,u,l) (fn (r,c) => Semiring.conjugate (get t (c,r)))
  
  fun decompose_index d len index = let
    fun dec 0 idx = []
      | dec k idx = (idx mod d)::(dec (k-1) (idx div d))
  in rev (dec len index)
  end
  
  fun recompose_index d lst = List.foldl (fn (part, rest) => part + d * rest) 0 lst
  
  (* insert element "k" in the list before "bef" *)
  fun list_ins 0 k [] = [k]
    | list_ins _ _ [] = []
    | list_ins bef k (l::ls) = if bef = 0 then k::l::(list_ins (bef-1) k ls)
                               else l::(list_ins (bef-1) k ls)
  
  
  (* trace the j-th input (lower index) to the k-th output (upper index) *)
  fun contractf f (j,k) (d,l,u) = let
    fun tr (row, col) = let
      val rowdec = decompose_index d (u-1) row
      val coldec = decompose_index d (l-1) col
      fun sum 0 = Semiring.zero
        | sum i = Semiring.plus (f (recompose_index d (list_ins k (i-1) rowdec),
                                    recompose_index d (list_ins j (i-1) coldec)),
                                 sum (i-1))
    in sum d
    end
  in
    tensorf (d, l-1, u-1) tr
  end
  
  fun contract io (t as (size,_)) = contractf (get t) io size 
  
  (* connect the k-th output of t1 to the j-th input of t2 *)
  fun contract2 (j,k) (t2 as ((_,l2,u2),_)) (t1 as ((d,l1,u1),_)) =
    contractf (get_from_product t1 t2) (l1+j,k) (d,l1+l2,u1+u2)
    
  fun compose (t2 as ((d,l2,u2),_)) (t1 as ((_,l1,_),_)) = let
    fun comp_get 0 _ = Semiring.zero
      | comp_get n (row,col) = Semiring.plus (Semiring.times (get t2 (row,n-1), get t1 (n-1,col)),
                                              comp_get (n-1) (row,col))
  in tensorf (d,l1,u2) (comp_get (pow d l2))
  end
  
  (* scalar_compare T1 T2 = SOME (a,b) ==> a T1 = b T2 *)
  fun scalar_compare t1 t2 = let
    val ((a ,b ),t1') = reduce t1
    val ((a',b'),t2') = reduce t2
  in if eq (t1',t2') then SOME (Semiring.times (a,b'), Semiring.times (a',b)) else NONE
  end
  
  fun apply_perm perm lst = map (fn i => List.nth (lst,i)) perm
  
  fun permrow_get (t as ((d,_,_),_)) perm (row,col) = let
    val prow = recompose_index d (apply_perm perm (decompose_index d (length perm) row))
  in get t (prow, col)
  end
  
  fun permcol_get (t as ((d,_,_),_)) perm (row,col) = let
    val pcol = recompose_index d (apply_perm perm (decompose_index d (length perm) col))
  in get t (row,pcol)
  end
  
  fun permrowcol_get (t as ((d,_,_),_)) permrow permcol (row,col) = let
    val prow = recompose_index d (apply_perm permrow (decompose_index d (length permrow) row))
    val pcol = recompose_index d (apply_perm permcol (decompose_index d (length permcol) col))
  in get t (prow, pcol)
  end
  
  fun perm_matrix d perm = let
    val n = length perm
    fun ptens (row,col) =
      if (recompose_index d
           (apply_perm perm
             (decompose_index d n row)
           )) = col then 1 else 0
  in tensorf (d,n,n) ptens
  end
  
  fun perm_matrix d perm = let val n = length perm
  in tensorf (d,n,n) (permcol_get (id d n) perm)
  end
  
  fun perms size = let
    fun lst 0 = [] | lst n = (n-1)::(lst (n-1))
    fun prm [] [] = [[]]
      | prm _ [] = []
      | prm h (x::xs) = (map (fn p=>x::p) (prm [] ((rev h)@xs)))
                          @ (prm (x::h) xs)
  in prm [] (lst size)
  end
  
  fun scalar_eq t1 t2 = case scalar_compare t1 t2
                          of SOME _ => true | NONE => false
  
  
  fun compare_all comp [] _ = []
    | compare_all comp _ [] = []
    | compare_all comp (x::xs) (y::ys) = if comp x y then
                                         (x,y)::(compare_all comp (x::xs) ys @
                                                 compare_all comp xs ys)
                                         else (compare_all comp (x::xs) ys @
                                               compare_all comp xs ys)
  
  (*fun perm_scalar_eq (t1 as ((d,l,u),_)) t2 = let
    val lst1 = map (fn p => tensorf (d,l,u) (permrow_get t1 p)) (perms u)
    val lst2 = map (fn p => tensorf (d,l,u) (permcol_get t2 p)) (perms l)
  in (compare_all scalar_eq lst1 lst2) <> []
  end*)
  
  fun all_perms (t as ((d,l,u),_)) = let
    fun f prow pcol tab = let
      val (sc, tens) = reduce (tensorf (d,l,u) (permrowcol_get t prow pcol))
    in tab |> Tab.update (tens, (sc,prow,pcol))
    end
  in fold_product f (perms u) (perms l) (Tab.empty)
  end
  
  fun normalise t = let
    val perm_table = all_perms t
    val (t',_) = the (Tab.min perm_table)
  in (the (Tab.lookup perm_table t'), t')
  end
  
  fun perm_scalar_eq t1 t2 = let
    val (_,t1') = normalise t1
    val (_,t2') = normalise t2
  in eq (t1',t2')
  end
  
  fun pad width s = funpow (width - size s) (prefix " ") s
  
  fun to_string ((d,l,u),v) = let
    val width = (Vector.foldr (fn (s,max) => Int.max (size (Semiring.to_string s), max)) 0 v) + 1
    val rowsize = pow d l
    fun str (i, num, rest) =
      (if (i <> 0) andalso ((i mod rowsize) = 0) then "]\n[ " else "") ^
      (pad width (Semiring.to_string num)) ^ rest
  in
    "[" ^ (Vector.foldri str "" v) ^ " ]\n\n"
  end
  
  fun print t = TextIO.print (to_string t)
end

structure IntTensor = Tensor(IntSemiring)
structure CIntTensor = Tensor(CIntSemiring)

(*val t1 = Tensor.tensor (2,2,1)
[ 0, 4, 0,  8,
 16,64,32,128]
val t2 = Tensor.tensor (2,2,1) [0,0,8,16,32,64,128,256]
val t3 = Tensor.tensor (2,2,1) [2,4,8,16,32,64,128,255]
val X = Tensor.tensor (2,1,1) [0,1,1,0]
val vect = Tensor.tensor (2,0,1) [1,2]
val vect2 = Tensor.contract2 (0,0) X vect

val _ = Tensor.print t1*)






